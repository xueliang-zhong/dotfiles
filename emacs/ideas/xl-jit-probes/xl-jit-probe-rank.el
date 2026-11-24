;;; xl-jit-probe-rank.el --- static hot-file ranking -*- lexical-binding: t; -*-
;; Probe, not part of the config.  Run after init in a batch Emacs (see
;; README.md in this folder).  PROBE_PROFILE names the real profile folder,
;; which holds savehist's `history' and `recentf.eld'; both are read as data.
;; It byte-compiles interpreted functions in place, which only affects the
;; batch Emacs it runs in.  Known gaps are listed in README.md.
(require 'byte-opt)
(require 'cl-lib)
(require 'seq)
(setq byte-compile-warnings nil)

(defvar probe-profile (getenv "PROBE_PROFILE"))

;; ------------------------------------------------------------
;; Byte-code features
;; ------------------------------------------------------------
(defconst probe-inline-ops
  '(byte-car byte-cdr byte-cons byte-eq byte-plus byte-diff byte-add1 byte-sub1
    byte-lss byte-gtr byte-leq byte-geq byte-eqlsign byte-nth byte-nthcdr
    byte-setcar byte-setcdr byte-aref byte-aset byte-car-safe byte-cdr-safe
    byte-memq byte-consp byte-listp byte-not byte-mult byte-max byte-min
    byte-symbolp byte-stringp byte-integerp byte-numberp byte-length byte-elt
    byte-nreverse byte-list1 byte-list2 byte-list3 byte-list4 byte-listN)
  "Opcodes native code runs much faster than the byte-code interpreter.")

(defconst probe-buffer-ops
  '(byte-insert byte-goto-char byte-forward-line byte-point byte-char-after
    byte-following-char byte-preceding-char byte-buffer-substring
    byte-delete-region byte-skip-chars-forward byte-skip-chars-backward
    byte-forward-char byte-forward-word byte-insertN byte-point-max byte-point-min
    byte-set-buffer byte-save-excursion byte-save-restriction byte-narrow-to-region)
  "Opcodes whose time is spent in C; native code changes little.")

(defconst probe-c-heavy
  '(insert insert-before-markers re-search-forward re-search-backward
    search-forward search-backward looking-at looking-back string-match
    replace-match replace-regexp-in-string buffer-substring
    buffer-substring-no-properties put-text-property add-text-properties
    propertize all-completions try-completion completion-all-completions
    sort json-parse-string json-parse-buffer read format message
    call-process make-process process-send-string delete-region
    overlay-put make-overlay font-lock-ensure)
  "Functions called from Lisp whose time is spent in C.")

(defconst probe-dynamic-callers
  '(funcall apply run-hooks run-hook-with-args run-hook-with-args-until-success
    run-hook-with-args-until-failure run-hook-wrapped mapcar mapc mapcan
    mapconcat seq-map seq-filter seq-do cl-mapcar))

(defun probe--lap (fn)
  (byte-decompile-bytecode (aref fn 1) (aref fn 2)))

(defun probe--closures (fn)
  "FN plus every byte-code closure nested in its constants."
  (let ((out (list fn)))
    (mapc (lambda (c) (when (byte-code-function-p c)
                        (setq out (nconc out (probe--closures c)))))
          (aref fn 2))
    out))

(defun probe--one (fn self)
  "Benefit and call sites of one byte-code object FN (no nested closures)."
  (let* ((ops (vconcat (seq-filter #'consp (probe--lap fn))))
         (n (length ops))
         (tag-pos (make-hash-table :test #'eq))
         (depth (make-vector n 0))
         (recursive (memq self (append (aref fn 2) nil)))
         (benefit 0.0) (loops 0) (calls nil) (varrefs nil) (dynamic nil))
    (dotimes (i n)
      (when (eq (car (aref ops i)) 'TAG) (puthash (aref ops i) i tag-pos)))
    ;; Loop regions: a jump back to an earlier tag.
    (dotimes (i n)
      (let* ((op (aref ops i)) (j (and (memq (car op) '(byte-goto byte-goto-if-nil
                                                         byte-goto-if-not-nil
                                                         byte-goto-if-nil-else-pop
                                                         byte-goto-if-not-nil-else-pop))
                                     (gethash (cdr op) tag-pos))))
        (when (and j (< j i))
          (cl-incf loops)
          (cl-loop for k from j to i do (cl-incf (aref depth k))))))
    (dotimes (i n)
      (let* ((op (aref ops i))
             ;; Code in a loop counts 4x per nesting level; recursion counts as one level.
             (w (expt 4.0 (+ (aref depth i) (if recursive 1 0))))
             (head (car op)))
        (cond
         ((memq head probe-inline-ops) (cl-incf benefit w))
         ((memq head probe-buffer-ops) (cl-decf benefit (* 2 w)))
         ((eq head 'byte-varref)
          (let ((v (car-safe (cdr op)))) (when (symbolp v) (push v varrefs))))
         ((eq head 'byte-constant)
          (let ((c (car-safe (cdr op))))
            (when (and (symbolp c) c (fboundp c))
              (push c calls)
              (cond ((memq c probe-c-heavy) (cl-decf benefit (* 2 w)))
                    ((memq c probe-dynamic-callers) (setq dynamic t) (cl-incf benefit (* 0.5 w)))
                    ((subrp (indirect-function c)) nil)
                    (t (cl-incf benefit (* 0.5 w))))))))))
    (list :benefit benefit :loops loops :calls calls :varrefs varrefs
          :dynamic dynamic :size (length (aref fn 1)))))

(defun probe-features (sym)
  "Summed features of SYM's definition and its nested closures."
  (let ((def (indirect-function sym)) (b 0.0) (loops 0) (size 0) calls vars dyn)
    (when (byte-code-function-p def)
      (dolist (f (probe--closures def))
        (let ((r (probe--one f sym)))
          (cl-incf b (plist-get r :benefit))
          (cl-incf loops (plist-get r :loops))
          (cl-incf size (plist-get r :size))
          (setq calls (nconc (plist-get r :calls) calls)
                vars (nconc (plist-get r :varrefs) vars)
                dyn (or dyn (plist-get r :dynamic))))))
    (list :benefit (max 0.0 b) :loops loops :size size
          :calls (delete-dups calls) :varrefs (delete-dups vars) :dynamic dyn)))

;; ------------------------------------------------------------
;; Function universe: every defun that is byte code (compile interpreted ones)
;; ------------------------------------------------------------
(defun probe--analysable (sym)
  (let ((def (and (fboundp sym) (indirect-function sym))))
    (cond ((byte-code-function-p def) t)
          ((interpreted-function-p def)
           (ignore-errors (byte-compile sym))
           (byte-code-function-p (indirect-function sym))))))

(defvar probe-file-of (make-hash-table :test #'eq))
(defvar probe-feat (make-hash-table :test #'eq))
(defvar probe-native-files nil)

(dolist (entry load-history)
  (let ((file (car entry)) (native nil))
    (dolist (d (cdr entry))
      (when (and (consp d) (eq (car d) 'defun) (symbolp (cdr d)) (fboundp (cdr d)))
        (let ((def (indirect-function (cdr d))))
          (cond ((native-comp-function-p def) (setq native t))
                ((probe--analysable (cdr d))
                 (puthash (cdr d) file probe-file-of))))))
    (when native (push file probe-native-files))))

(maphash (lambda (sym _) (puthash sym (probe-features sym) probe-feat)) probe-file-of)

;; ------------------------------------------------------------
;; Edges: constants + value-flow (function-valued variables) + hooks + advice
;; ------------------------------------------------------------
(defun probe--fn-values (v)
  "Functions named by the value of variable V (a function or list of them)."
  (when (and (symbolp v) (boundp v))
    (let ((val (symbol-value v)))
      (cond ((and (symbolp val) val (fboundp val)) (list val))
            ((and (consp val) (proper-list-p val))
             (seq-filter (lambda (x) (and (symbolp x) x (not (eq x t)) (fboundp x))) val))))))

(defvar probe-edges (make-hash-table :test #'eq))
(defun probe--edge (a b) (unless (eq a b) (cl-pushnew b (gethash a probe-edges))))

(maphash
 (lambda (sym feat)
   (dolist (c (plist-get feat :calls)) (probe--edge sym c))
   (when (plist-get feat :dynamic)
     (dolist (v (plist-get feat :varrefs))
       (dolist (f (probe--fn-values v)) (probe--edge sym f)))))
 probe-feat)

;; Advice: calling the original also runs the advice.
(mapatoms
 (lambda (s)
   (when (and (fboundp s) (advice--p (advice--symbol-function s)))
     (advice-mapc (lambda (f _) (when (symbolp f) (probe--edge s f))) s))))

;; completion-styles registry: style name -> its all-completions function.
(dolist (style (and (boundp 'completion-styles) completion-styles))
  (dolist (f (cdr (assq style completion-styles-alist)))
    (when (and (symbolp f) (fboundp f)) (probe--edge 'completing-read f)
          (probe--edge 'completion-all-completions f))))

;; ------------------------------------------------------------
;; Roots with evidence weights
;; ------------------------------------------------------------
(defvar probe-roots (make-hash-table :test #'eq))
(defun probe--root (f w) (when (and (symbolp f) (fboundp f))
                           (puthash f (+ w (gethash f probe-roots 0.0)) probe-roots)))

(defun probe--read-file (file)
  (when (file-readable-p file)
    (with-temp-buffer (insert-file-contents file) (buffer-string))))

;; 1. Commands the user actually ran (savehist), newest weigh more.
(let ((text (probe--read-file (expand-file-name "history" probe-profile))) (cmds nil))
  ;; Only this variable is parsed: the same file holds the kill ring.
  (when (and text (string-match "(setq extended-command-history '" text))
    (setq cmds (seq-filter #'stringp
                           (ignore-errors (car (read-from-string text (match-end 0)))))))
  (cl-loop for c in cmds for i from 0
           do (probe--root (intern-soft c) (/ 3.0 (1+ (* 0.05 i)))))
  (princ (format "savehist commands: %d\n" (length cmds))))

;; 2. Modes the user edits (recentf -> auto-mode-alist), weighted by file count.
(defvar probe-used-modes (make-hash-table :test #'eq))
(let ((text (probe--read-file (expand-file-name "recentf.eld" probe-profile))) files)
  ;; recentf.eld is (setq recentf-list '(...)): take the quoted list with
  ;; `read' and keep only strings.  Never `eval' it: the file is data.
  (when (and text (string-match "(setq recentf-list" text))
    (let ((form (ignore-errors (car (read-from-string text (match-beginning 0))))))
      (when (and (eq (car-safe form) 'setq) (eq (car-safe (nth 2 form)) 'quote))
        (setq files (seq-filter #'stringp (cadr (nth 2 form)))))))
  (dolist (f files)
    (let ((mode (assoc-default f auto-mode-alist #'string-match)))
      (when (consp mode) (setq mode (car mode)))
      (when (and mode (symbolp mode))
        (puthash mode (1+ (gethash mode probe-used-modes 0)) probe-used-modes))))
  (puthash 'dired-mode (max 1 (gethash 'dired-mode probe-used-modes 0)) probe-used-modes)
  (princ (format "recentf files: %d, modes: %S\n" (length files)
                 (let (l) (maphash (lambda (k v) (push (cons k v) l)) probe-used-modes) l))))

;; Mode function, its hook and its keymap commands: only for modes in use.
(maphash
 (lambda (mode count)
   (let ((w (* 1.0 (log (1+ count) 2))))
     (probe--root mode w)
     (dolist (f (probe--fn-values (intern-soft (format "%s-hook" mode)))) (probe--root f w))
     (let ((map (intern-soft (format "%s-map" mode))))
       (when (and map (boundp map) (keymapp (symbol-value map)))
         (map-keymap-internal
          (lambda (_ b) (when (symbolp b) (probe--root b (* 0.1 w))))
          (symbol-value map))))))
 probe-used-modes)

;; 3. Per-keystroke and per-redisplay hooks: run all the time, so weigh most.
(dolist (h '(pre-command-hook post-command-hook after-change-functions
             before-change-functions post-self-insert-hook
             window-configuration-change-hook window-scroll-functions
             window-buffer-change-functions window-state-change-functions
             jit-lock-functions completion-at-point-functions
             minibuffer-setup-hook))
  (dolist (f (probe--fn-values h)) (probe--root f 5.0))
  (dolist (f (and (boundp h) (default-value h) (proper-list-p (default-value h))
                  (default-value h)))
    (probe--root f 5.0)))

;; 4. Commands in the global and leader maps: weak evidence.
(dolist (map (list global-map (and (boundp 'xl-leader-map) xl-leader-map)))
  (when (keymapp map)
    (map-keymap (lambda (_ b) (when (symbolp b) (probe--root b 0.05))) map)))

;; ------------------------------------------------------------
;; Propagation: R(callee) += alpha * R(caller), 4 levels
;; ------------------------------------------------------------
(defvar probe-heat (make-hash-table :test #'eq))
(maphash (lambda (f w) (puthash f w probe-heat)) probe-roots)
(let ((frontier (let (l) (maphash (lambda (f w) (push (cons f w) l)) probe-roots) l)))
  (dotimes (_ 4)
    (let ((next (make-hash-table :test #'eq)))
      (dolist (fw frontier)
        (let* ((outs (gethash (car fw) probe-edges))
               (share (/ (* 0.5 (cdr fw)) (max 1 (sqrt (length outs))))))
          (dolist (g outs)
            (puthash g (+ share (gethash g next 0.0)) next))))
      (setq frontier nil)
      (maphash (lambda (g w) (puthash g (+ w (gethash g probe-heat 0.0)) probe-heat)
                 (push (cons g w) frontier))
               next))))

;; ------------------------------------------------------------
;; File ranking: sum heat x benefit, divided by compile cost
;; ------------------------------------------------------------
(let ((files (make-hash-table :test #'equal)))
  (maphash
   (lambda (sym file)
     (let* ((feat (gethash sym probe-feat))
            (score (* (gethash sym probe-heat 0.0) (plist-get feat :benefit)))
            (row (or (gethash file files) (list 0.0 0 nil))))
       (setf (nth 0 row) (+ (nth 0 row) score)
             (nth 1 row) (+ (nth 1 row) (plist-get feat :size)))
       (when (> score 0) (push (cons sym score) (nth 2 row)))
       (puthash file row files)))
   probe-file-of)
  (defvar probe-rows nil)
  (let ((rows nil))
    (maphash (lambda (f r) (when (> (nth 0 r) 0) (push (cons f r) rows))) files)
    (setq rows (sort rows (lambda (a b) (> (/ (nth 1 a) (max 1 (nth 2 a)))
                                           (/ (nth 1 b) (max 1 (nth 2 b)))))))
    (setq probe-rows rows))
  (let ((rows probe-rows))
    (princ (format "analysed functions: %d, edges from %d, roots: %d, native files skipped: %d\n"
                   (hash-table-count probe-feat) (hash-table-count probe-edges)
                   (hash-table-count probe-roots) (length probe-native-files)))
    (princ "rank  score/KB   score   KB  file  [top functions]\n")
    (cl-loop for (file score size fns) in (seq-take rows 25) for i from 1
             do (princ (format "%2d %9.1f %8.1f %5.1f  %s  %S\n" i
                               (/ score (max 0.1 (/ size 1024.0))) score (/ size 1024.0)
                               (file-name-nondirectory file)
                               (mapcar #'car (seq-take (sort fns (lambda (a b) (> (cdr a) (cdr b)))) 3)))))))
