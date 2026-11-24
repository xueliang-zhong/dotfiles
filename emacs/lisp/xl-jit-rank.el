;;; xl-jit-rank.el --- which files are worth native compiling (report only) -*- lexical-binding: t; -*-
;; Step 5 of emacs/ideas/xl-jit.md.  Nothing here compiles anything.  A child
;; Emacs with the profile loaded reads the byte code, the call graph, and the
;; usage records (savehist, recentf) as data, and prints a ranking.  Entry
;; points: `xl-jit-report' (M-x), and `emacs-setup.sh jit-report'.

(require 'cl-lib)
(require 'seq)
(require 'xl-jit)
(require 'xl-spinner)
(require 'xl-async)

;;; ------------------------------------------------------------
;;; Settings.  The weights are starting values; calibrate them against a
;;; profiler run before trusting the order (xl-jit.md, "校准").
;;; ------------------------------------------------------------

(defconst xl-jit-rank-heat-levels 4
  "How many call levels heat spreads through.")

(defconst xl-jit-rank-spread 0.5
  "Share of a caller's heat passed to each callee (divided by sqrt of callees).")

(defconst xl-jit-rank-loop-factor 4.0
  "Weight of code inside a loop, per nesting level.  A recursive function
counts as one level deeper.")

(defconst xl-jit-rank-savehist-cap 1.0
  "Heat of the most recent savehist command.  Older commands decay, and no
command can weigh more than this, so one habit cannot dominate.")

(defconst xl-jit-rank-always-heat 5.0
  "Heat of hooks that run on every keystroke or redisplay.")

(defconst xl-jit-rank-baseline-users 50
  "When this many savehist commands and recentf files exist, the baseline is
gone.  Below it the baseline's weight falls in step with the user's data.")

(defconst xl-jit-rank--inline-ops
  '(byte-car byte-cdr byte-cons byte-eq byte-plus byte-diff byte-add1 byte-sub1
    byte-lss byte-gtr byte-leq byte-geq byte-eqlsign byte-nth byte-nthcdr
    byte-setcar byte-setcdr byte-aref byte-aset byte-car-safe byte-cdr-safe
    byte-memq byte-consp byte-listp byte-not byte-mult byte-max byte-min
    byte-symbolp byte-stringp byte-integerp byte-numberp byte-length byte-elt
    byte-nreverse)
  "Opcodes native code runs much faster than byte code.  Count them.")

(defconst xl-jit-rank--buffer-ops
  '(byte-insert byte-goto-char byte-forward-line byte-point byte-char-after
    byte-following-char byte-preceding-char byte-buffer-substring
    byte-delete-region byte-skip-chars-forward byte-skip-chars-backward
    byte-forward-char byte-forward-word byte-insertN byte-point-max byte-point-min
    byte-set-buffer byte-save-excursion byte-save-restriction byte-narrow-to-region)
  "Opcodes whose time is spent in C.  Native code changes little.")

(defconst xl-jit-rank--c-heavy
  '(insert insert-before-markers re-search-forward re-search-backward
    search-forward search-backward looking-at looking-back string-match
    replace-match replace-regexp-in-string buffer-substring
    buffer-substring-no-properties put-text-property add-text-properties
    propertize all-completions try-completion completion-all-completions
    sort json-parse-string json-parse-buffer read format message
    call-process make-process process-send-string delete-region
    overlay-put make-overlay font-lock-ensure)
  "Functions called from Lisp whose time is spent in C.")

(defconst xl-jit-rank--alloc-heavy
  '(substring concat format split-string string-join string-trim
    make-string number-to-string mapconcat)
  "Calls that allocate.  Measured: native code gains little where they dominate
(xl-jit.md, \"实测\"), because time goes to allocation and GC.")

(defconst xl-jit-rank--dynamic-callers
  '(funcall apply run-hooks run-hook-with-args run-hook-with-args-until-success
    run-hook-with-args-until-failure run-hook-wrapped mapcar mapc mapcan
    seq-map seq-filter seq-do cl-mapcar))

;;; ------------------------------------------------------------
;;; Byte-code features of one function (nested closures included)
;;; ------------------------------------------------------------

(defun xl-jit-rank--lap (fn)
  (seq-filter #'consp
              (byte-decompile-bytecode (aref fn 1) (aref fn 2))))

(defun xl-jit-rank--closures (fn)
  "FN and every byte-code closure nested in its constants."
  (let ((out (list fn)))
    (mapc (lambda (c)
            (when (byte-code-function-p c)
              (setq out (nconc out (xl-jit-rank--closures c)))))
          (aref fn 2))
    out))

(defun xl-jit-rank--one (fn self)
  "Benefit and call sites of one byte-code object FN, nested closures excluded.
Returns (:benefit B :loops N :calls LIST :varrefs LIST :dynamic BOOL :size N)."
  (let* ((ops (vconcat (xl-jit-rank--lap fn)))
         (n (length ops))
         (tag-pos (make-hash-table :test #'eq))
         (depth (make-vector n 0))
         (recursive (memq self (append (aref fn 2) nil)))
         (benefit 0.0) (loops 0) (calls nil) (varrefs nil) (dynamic nil))
    (dotimes (i n)
      (when (eq (car (aref ops i)) 'TAG)
        (puthash (aref ops i) i tag-pos)))
    ;; A jump back to an earlier tag marks a loop body.
    (dotimes (i n)
      (let* ((op (aref ops i))
             (j (and (memq (car op) '(byte-goto byte-goto-if-nil byte-goto-if-not-nil
                                      byte-goto-if-nil-else-pop
                                      byte-goto-if-not-nil-else-pop))
                     (gethash (cdr op) tag-pos))))
        (when (and j (< j i))
          (cl-incf loops)
          (cl-loop for k from j to i do (cl-incf (aref depth k))))))
    (dotimes (i n)
      (let* ((op (aref ops i))
             (head (car op))
             (w (expt xl-jit-rank-loop-factor
                      (+ (aref depth i) (if recursive 1 0)))))
        (cond
         ((memq head xl-jit-rank--inline-ops) (cl-incf benefit w))
         ((memq head xl-jit-rank--buffer-ops) (cl-decf benefit (* 2 w)))
         ((eq head 'byte-varref)
          (let ((v (car-safe (cdr op)))) (when (symbolp v) (push v varrefs))))
         ((eq head 'byte-constant)
          (let ((c (car-safe (cdr op))))
            (when (and (symbolp c) c (fboundp c))
              (push c calls)
              (cond ((memq c xl-jit-rank--c-heavy) (cl-decf benefit (* 2 w)))
                    ((memq c xl-jit-rank--alloc-heavy) (cl-decf benefit (* 2 w)))
                    ((memq c xl-jit-rank--dynamic-callers)
                     (setq dynamic t) (cl-incf benefit (* 0.5 w)))
                    ((subrp (indirect-function c)) nil)
                    (t (cl-incf benefit (* 0.5 w))))))))))
    (list :benefit benefit :loops loops :calls calls :varrefs varrefs
          :dynamic dynamic :size (length (aref fn 1)))))

(defun xl-jit-rank--features (sym)
  "Summed features of SYM's definition and its nested closures."
  (let ((def (indirect-function sym))
        (b 0.0) (loops 0) (size 0) calls vars dyn)
    (when (byte-code-function-p def)
      (dolist (f (xl-jit-rank--closures def))
        (let ((r (xl-jit-rank--one f sym)))
          (cl-incf b (plist-get r :benefit))
          (cl-incf loops (plist-get r :loops))
          (cl-incf size (plist-get r :size))
          (setq calls (nconc (plist-get r :calls) calls)
                vars (nconc (plist-get r :varrefs) vars)
                dyn (or dyn (plist-get r :dynamic))))))
    (list :benefit (max 0.0 b) :loops loops :size size
          :calls (delete-dups calls) :varrefs (delete-dups vars) :dynamic dyn)))

;;; ------------------------------------------------------------
;;; Files: which source a function belongs to, and whether it may be compiled
;;; ------------------------------------------------------------

(defun xl-jit-rank--file-key (file)
  "Name used in the ranking: basename without .elc, .el or .gz.
Returns nil for autoload files, which only hold stubs."
  (let ((base (file-name-nondirectory file)))
    (unless (string-match-p "-autoloads\\.elc?\\'" base)
      (replace-regexp-in-string "\\.\\(elc\\|el\\)\\(\\.gz\\)?\\'" "" base))))

(defvar xl-jit-rank--no-native-cache (make-hash-table :test #'equal))

(defun xl-jit-rank--no-native-p (file)
  "Non-nil if FILE opts out of native or byte compilation with a local variable."
  (let ((true (or (ignore-errors (file-truename file)) file)))
    (let ((hit (gethash true xl-jit-rank--no-native-cache 'unknown)))
      (if (not (eq hit 'unknown))
          hit
        (puthash true
                 (and (file-readable-p true)
                      (with-temp-buffer
                        (insert-file-contents true)
                        (let ((text (buffer-string)))
                          (or (string-match-p "no-native-compile:[ \t]*t" text)
                              (string-match-p "no-byte-compile:[ \t]*t" text)))))
                 xl-jit-rank--no-native-cache)))))

;;; ------------------------------------------------------------
;;; Graph: functions, call edges, and roots with evidence weights
;;; ------------------------------------------------------------

(defun xl-jit-rank--fn-values (v)
  "Functions named by the value of variable V: one function or a list of them."
  (when (and (symbolp v) (boundp v))
    (let ((val (symbol-value v)))
      (cond ((and (symbolp val) val (fboundp val)) (list val))
            ((and (consp val) (proper-list-p val))
             (seq-filter (lambda (x) (and (symbolp x) x (not (eq x t)) (fboundp x)))
                         val))))))

(defun xl-jit-rank--read-file (file)
  (when (file-readable-p file)
    (with-temp-buffer (insert-file-contents file) (buffer-string))))

(defun xl-jit-rank--graph (profile-dir)
  "Build the graph from the loaded Emacs.  Returns a plist: :features (sym
. feature-plist), :file (sym . file-key), :path (sym . full file), :edges
 (sym . callees), :excluded (file-key . reason), :native-skipped N,
:interpreted-skipped N."
  (let ((features (make-hash-table :test #'eq))
        (file-of (make-hash-table :test #'eq))
        (path-of (make-hash-table :test #'eq))
        (edges (make-hash-table :test #'eq))
        (excluded nil)
        (interpreted-skipped 0)
        (native-skipped 0))
    ;; Functions: every defun from load-history, keyed by its source file.
    (dolist (entry load-history)
      (let* ((file (car entry))
             (key (and (stringp file) (xl-jit-rank--file-key file))))
        (when key
          (dolist (d (cdr entry))
            (when (and (consp d) (eq (car d) 'defun) (symbolp (cdr d))
                       (fboundp (cdr d)))
              (let ((sym (cdr d)))
                (cond
                 ((native-comp-function-p (indirect-function sym))
                  (cl-incf native-skipped))
                 ((xl-jit-rank--no-native-p file)
                  (cl-pushnew (cons key "no-native-compile or no-byte-compile") excluded
                              :test #'equal))
                 ;; Interpreted functions are skipped, not compiled here: the
                 ;; report must have no side effects.  Our own lisp is byte
                 ;; code once compile-lisp has run.
                 ((byte-code-function-p (indirect-function sym))
                  (puthash sym key file-of)
                  (puthash sym file path-of)
                  (puthash sym (xl-jit-rank--features sym) features))
                 (t (cl-incf interpreted-skipped)))))))))
    ;; Edges: constants, plus function-valued variables read by funcall/apply.
    (maphash
     (lambda (sym feat)
       (dolist (c (plist-get feat :calls))
         (unless (eq c sym) (puthash sym (cons c (gethash sym edges)) edges)))
       (when (plist-get feat :dynamic)
         (dolist (v (plist-get feat :varrefs))
           (dolist (f (xl-jit-rank--fn-values v))
             (unless (eq f sym) (puthash sym (cons f (gethash sym edges)) edges))))))
     features)
    ;; Advice: calling the original also runs its advice.
    (mapatoms
     (lambda (s)
       (when (and (fboundp s) (advice--p (advice--symbol-function s)))
         (advice-mapc (lambda (f _) (when (and (symbolp f) (not (eq f s)))
                                      (puthash s (cons f (gethash s edges)) edges)))
                      s))))
    (list :features features :file file-of :path path-of :edges edges
          :excluded excluded :native-skipped native-skipped
          :interpreted-skipped interpreted-skipped
          :profile profile-dir)))

(defun xl-jit-rank--roots (profile-dir)
  "Roots with evidence weights, as an alist (SYM . WEIGHT).
Savehist commands are newest first, capped and decaying.  Recentf files map to
modes through `auto-mode-alist', and a mode's hook and keymap commands follow
it at a lower weight.  Hooks that run on every key are heavy."
  (let ((roots (make-hash-table :test #'eq))
        (savehist nil)
        (user-count 0))
    (cl-flet ((add (sym w)
                (when (and sym (symbolp sym) (fboundp sym))
                  (puthash sym (+ w (gethash sym roots 0.0)) roots))))
      ;; 1. Commands the user ran: savehist keeps only the command history
      ;;    here; the rest of that file (the kill ring) is never read.
      (let* ((text (xl-jit-rank--read-file (expand-file-name "history" profile-dir)))
             (cmds (and text (string-match "(setq extended-command-history '" text)
                        (seq-filter #'stringp
                                    (ignore-errors
                                     (car (read-from-string text (match-end 0))))))))
        (cl-loop for c in cmds for i from 0
                 do (let ((sym (intern-soft c))
                          (w (min xl-jit-rank-savehist-cap (/ 1.0 (+ 1 (* 0.05 i))))))
                      (when (and sym (fboundp sym))
                        (push (cons sym w) savehist))
                      (add sym w)))
        (cl-incf user-count (length cmds)))
      ;; 2. Modes the user edits.  recentf.eld is data: read, never eval'd.
      (let* ((text (xl-jit-rank--read-file (expand-file-name "recentf.eld" profile-dir)))
             (form (and text (string-match "(setq recentf-list" text)
                        (ignore-errors (car (read-from-string text (match-beginning 0))))))
             (files (and (eq (car-safe form) 'setq) (eq (car-safe (nth 2 form)) 'quote)
                         (seq-filter #'stringp (cadr (nth 2 form)))))
             (modes (make-hash-table :test #'eq)))
        (dolist (f files)
          (let ((mode (assoc-default f auto-mode-alist #'string-match)))
            (when (consp mode) (setq mode (car mode)))
            (when (and mode (symbolp mode))
              (puthash mode (1+ (gethash mode modes 0)) modes))))
        (cl-incf user-count (length files))
        (maphash
         (lambda (mode count)
           (let ((w (log (1+ count) 2)))
             (add mode w)
             (dolist (f (xl-jit-rank--fn-values (intern-soft (format "%s-hook" mode))))
               (add f w))
             (let ((map (intern-soft (format "%s-map" mode))))
               (when (and map (boundp map) (keymapp (symbol-value map)))
                 (map-keymap-internal
                  (lambda (_ b) (when (symbolp b) (add b (* 0.1 w))))
                  (symbol-value map))))))
         modes))
      ;; 3. Hooks that run on every key or redisplay.
      (dolist (h '(pre-command-hook post-command-hook after-change-functions
                   before-change-functions post-self-insert-hook
                   window-configuration-change-hook window-scroll-functions
                   window-buffer-change-functions window-state-change-functions
                   jit-lock-functions completion-at-point-functions
                   minibuffer-setup-hook))
        (dolist (f (append (xl-jit-rank--fn-values h)
                           (let ((d (and (boundp h) (default-value h))))
                             (and (proper-list-p d) (seq-filter #'symbolp d)))))
          (add f xl-jit-rank-always-heat)))
      ;; 4. Global keymap commands: weak evidence.
      (when (keymapp global-map)
        (map-keymap (lambda (_ b) (when (symbolp b) (add b 0.05))) global-map)))
    (list :roots (let (l) (maphash (lambda (k v) (push (cons k v) l)) roots) l)
          :savehist savehist
          :user-count user-count)))

(defun xl-jit-rank--baseline (profile-dir)
  "The shipped baseline roots, weighted down as the user's own data grows.
Returns (ROOTS . FACTOR).  The baseline file names functions only."
  (let* ((file (or (and (bound-and-true-p xl-dotfiles)
                        (expand-file-name "emacs/jit-baseline.eld" xl-dotfiles))
                   (expand-file-name "jit-baseline.eld" profile-dir)))
         (form (xl-jit--read-eld file))
         (names (plist-get form :functions)))
    (cons (and names
               (seq-filter (lambda (s) (and (symbolp s) (fboundp s))) names))
          nil)))

(defun xl-jit-rank--propagate (seeds edges)
  "Heat per symbol after spreading SEEDS, an alist (SYM . WEIGHT), along EDGES."
  (let ((heat (make-hash-table :test #'eq))
        (frontier seeds))
    (dolist (s seeds)
      (puthash (car s) (+ (cdr s) (gethash (car s) heat 0.0)) heat))
    (dotimes (_ xl-jit-rank-heat-levels)
      (let ((next (make-hash-table :test #'eq)))
        (dolist (fw frontier)
          (let* ((outs (gethash (car fw) edges))
                 (share (/ (* xl-jit-rank-spread (cdr fw))
                           (max 1 (sqrt (length outs))))))
            (dolist (g outs)
              (puthash g (+ share (gethash g next 0.0)) next))))
        (setq frontier nil)
        (maphash (lambda (g w)
                   (puthash g (+ w (gethash g heat 0.0)) heat)
                   (push (cons g w) frontier))
                 next)))
    heat))

;;; ------------------------------------------------------------
;;; Ranking
;;; ------------------------------------------------------------

(defun xl-jit-rank--file-scores (heat graph)
  "Per file key: score = sum of heat x benefit, and the file's total size."
  (let ((scores (make-hash-table :test #'equal)))
    (maphash
     (lambda (sym h)
       (let ((key (gethash sym (plist-get graph :file)))
             (feat (gethash sym (plist-get graph :features))))
         (when (and key feat (> h 0))
           (cl-incf (gethash key scores 0.0) (* h (plist-get feat :benefit))))))
     heat)
    scores))

(defun xl-jit-rank--compute (profile-dir)
  "Full analysis.  Returns a plist: :rows (sorted), :savehist-share, and the
numbers for the header."
  (let* ((graph (xl-jit-rank--graph profile-dir))
         (edges (plist-get graph :edges))
         (roots-info (xl-jit-rank--roots profile-dir))
         (user-roots (plist-get roots-info :roots))
         (user-count (plist-get roots-info :user-count))
         (baseline (car (xl-jit-rank--baseline profile-dir)))
         (base-factor (max 0.0 (- 1.0 (/ (float user-count) xl-jit-rank-baseline-users))))
         (base-roots (mapcar (lambda (s) (cons s (* base-factor 1.0))) baseline))
         (seeds (append user-roots base-roots))
         (heat (xl-jit-rank--propagate seeds edges))
         (scores (xl-jit-rank--file-scores heat graph))
         (total (let ((sum 0.0)) (maphash (lambda (_ v) (cl-incf sum v)) scores) sum))
         ;; Each savehist command's share of the whole score.  Heat is linear,
         ;; so one command's contribution is its own propagation.
         (savehist-cmds (plist-get roots-info :savehist))
         (max-share 0.0))
    (when (> total 0)
      (dolist (r savehist-cmds)
        (let* ((h (xl-jit-rank--propagate (list r) edges))
               (s (xl-jit-rank--file-scores h graph))
               (part 0.0))
          (maphash (lambda (_ v) (cl-incf part v)) s)
          (setq max-share (max max-share (/ part total))))))
    (let (rows)
      (maphash
       (lambda (key score)
         (let ((size 0) (fns nil))
           (maphash (lambda (sym k)
                      (when (equal k key)
                        (let ((feat (gethash sym (plist-get graph :features)))
                              (h (gethash sym heat 0.0)))
                          (cl-incf size (plist-get feat :size))
                          (when (> h 0)
                            (push (cons sym (* h (plist-get feat :benefit))) fns)))))
                    (plist-get graph :file))
           (when (> score 0)
             (push (list :file key :score score :size size
                         :ratio (/ score (max 0.1 (/ size 1024.0)))
                         :top (mapcar #'car (seq-take (sort fns (lambda (a b) (> (cdr a) (cdr b))))
                                                      3))
                         :defuns (let (names)
                                   (maphash (lambda (sym k)
                                              (when (equal k key) (push sym names)))
                                            (plist-get graph :file))
                                   names)
                         :path (cl-loop for s being the hash-keys of (plist-get graph :file)
                                        using (hash-values k)
                                        when (equal k key)
                                        return (gethash s (plist-get graph :path)))
                         :trusted (let ((p (cl-loop for s being the hash-keys of (plist-get graph :file)
                                                    using (hash-values k)
                                                    when (equal k key)
                                                    return (gethash s (plist-get graph :path)))))
                                    (and p (xl-jit-trusted-file-p p))))
                   rows))))
       scores)
      (list :rows (sort rows (lambda (a b) (> (plist-get a :score) (plist-get b :score))))
            :built-rows (xl-jit-rank--built-rows)
            :next (seq-take (seq-filter #'xl-jit--candidate-p
                                        (sort (copy-sequence rows)
                                              (lambda (a b) (> (plist-get a :score) (plist-get b :score)))))
                            xl-jit-compile-budget)
            :ledger (xl-jit-rank--ledger-summary)
            :savehist-share max-share
            :functions (hash-table-count (plist-get graph :features))
            :roots (length user-roots)
            :user-count user-count
            :baseline-roots (length baseline)
            :baseline-factor base-factor
            :excluded (plist-get graph :excluded)
            :native-skipped (plist-get graph :native-skipped)
            :interpreted-skipped (plist-get graph :interpreted-skipped)))))

(defun xl-jit-rank--built-rows ()
  "One row per file xl-jit built and that is native now.
These files drop out of the ranking, because their functions are native, so
they are listed here with native \"xl-jit\".  Their KB is the eln size."
  (let* ((info (xl-jit-native-list))
         (natives (plist-get info :files))
         rows)
    (dolist (b (plist-get info :built))
      (let* ((src (car b))
             (eln (cdr b))
             (stem (xl-jit--source-stem src))
             (fns (cl-loop for (file . syms) in natives
                           when (equal (xl-jit--source-stem file) stem)
                           append syms)))
        (push (list :file (xl-jit-rank--file-key src) :native "xl-jit"
                    :path src :trusted t :ratio nil :score nil
                    :size (or (xl-jit--file-size eln) 0)
                    :count (length fns)
                    :top (seq-take fns 3))
              rows)))
    (sort rows (lambda (a b) (string< (plist-get a :file) (plist-get b :file))))))

(defun xl-jit-rank--line (rank score kb trusted native file top)
  "One report row.  Every row, ranked or built, uses this one layout."
  (format "%4s  %6s  %6s  %-7s  %-6s  %-28s  %s\n"
          rank score kb trusted native file top))

(defun xl-jit-rank--ledger-summary ()
  "Every ledger entry as a plist, newest first, with the source name."
  (let (out)
    (dolist (e (xl-jit--ledger-load))
      (let ((p (cdr e)))
        (push (list :file (file-name-nondirectory (car (car e)))
                    :status (plist-get p :status)
                    :time (plist-get p :time)
                    :fails (or (plist-get p :fails) 0)
                    :reason (plist-get p :reason)
                    :kb (and (plist-get p :size) (/ (plist-get p :size) 1024.0)))
              out)))
    (sort out (lambda (a b) (> (or (plist-get a :time) 0) (or (plist-get b :time) 0))))))

(defun xl-jit-rank--when (time)
  (if time (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time time)) "-"))

(defun xl-jit-rank--format (result)
  "The report as a string.  Sections, in order: what the next pass does, what
was compiled, what failed, why each ranked file was skipped, what is left out
altogether.  The ranking table comes last."
  (let* ((built (plist-get result :built-rows))
         (next (plist-get result :next))
         (ledger (plist-get result :ledger))
         (done (seq-filter (lambda (e) (eq (plist-get e :status) 'done)) ledger))
         (bad (seq-filter (lambda (e) (not (eq (plist-get e :status) 'done))) ledger))
         (rows (plist-get result :rows)))
    (with-output-to-string
      ;; 1. The answer
      (princ "xl-jit report (report only; nothing is compiled here)\n\n")
      (princ "Next background pass compiles: ")
      (if (null next)
          (princ "nothing.\n")
        (princ (format "%d file(s), in this order: %s\n" (length next)
                       (mapconcat (lambda (r) (plist-get r :file)) next ", "))))
      ;; 2. What was compiled
      (princ (format "\nCompiled by xl-jit and native now: %d file(s), %d function(s)\n"
                     (length built) (apply #'+ (mapcar (lambda (r) (plist-get r :count)) built))))
      (when built
        (princ (format "\n%-28s  %6s  %9s  %s\n" "file" "KB" "functions" "examples"))
        (dolist (row built)
          (princ (format "%-28s  %6.1f  %9d  %S\n"
                         (plist-get row :file)
                         (/ (plist-get row :size) 1024.0)
                         (plist-get row :count)
                         (plist-get row :top)))))
      (when done
        (princ "\nEntries in the ledger, status done (time of the build):\n")
        (dolist (e done)
          (princ (format "  %-30s %s  %5.1f KB\n" (plist-get e :file)
                         (xl-jit-rank--when (plist-get e :time))
                         (or (plist-get e :kb) 0)))))
      ;; 3. What failed or was held back
      (when bad
        (princ "\nFailed or quarantined (not retried until the key changes, or for a day):\n")
        (dolist (e bad)
          (princ (format "  %-30s %-11s %s  fails %d\n    %s\n"
                         (plist-get e :file) (plist-get e :status)
                         (xl-jit-rank--when (plist-get e :time))
                         (plist-get e :fails)
                         (or (plist-get e :reason) "")))))
      ;; 4. Why each ranked file is or is not compiled
      (princ "\nRanked files not yet native, and what happens to each:\n")
      (let ((n 0))
        (dolist (row rows)
          (let ((why (if (memq row next)
                         "compile in the next pass"
                       (or (xl-jit--candidate-reason row)
                           "candidate, but past the budget of 3 per pass"))))
            (when (< n 40)
              (princ (format "  %-28s score %6.1f  %s\n"
                             (plist-get row :file) (plist-get row :score) why)))
            (cl-incf n)))
        (when (> n 40) (princ (format "  ... and %d more\n" (- n 40)))))
      ;; 5. What is left out altogether
      (princ "\nLeft out before ranking:\n")
      (princ (format "  already native in this Emacs (after init): %d function(s)\n"
                     (plist-get result :native-skipped)))
      (let ((excl (plist-get result :excluded)))
        (princ (format "  opted out with no-native-compile: %d file(s)%s\n"
                       (length excl)
                       (if excl (format ": %s"
                                        (mapconcat (lambda (e) (car e)) excl ", "))
                         ""))))
      (princ (format "  interpreted, no .elc (Crafted's modules): %d function(s)\n"
                     (plist-get result :interpreted-skipped)))
      (princ (format "  functions analysed: %d;  largest single savehist command share of the score: %.1f%%\n"
                     (plist-get result :functions)
                     (* 100 (plist-get result :savehist-share))))
      ;; 6. The table
      (princ "\nRanking (score: expected gain; KB: bytecode size, the cost):\n")
      (princ (xl-jit-rank--line "rank" "score" "KB" "trusted" "native" "file" "[top functions]"))
      (cl-loop for row in (seq-take rows 25) for i from 1
               do (princ (xl-jit-rank--line
                          i
                          (format "%.1f" (plist-get row :score))
                          (format "%.1f" (/ (plist-get row :size) 1024.0))
                          (if (plist-get row :trusted) "yes" "NO")
                          "no" (plist-get row :file)
                          (format "%S" (plist-get row :top))))))))

;;; The report as a buffer.  The ranking takes seconds, so it runs in a child
;;; Emacs that loads the profile and prints this same text; the buffer shows a
;;; spinner in its mode line until the child answers (as GBlame does).

(defconst xl-jit-rank--marker "<<XL-JIT-REPORT>>"
  "Printed on its own line before a child's report.  The child's own loading
messages also go to its output, so the parent keeps only what follows it.")

(defvar xl-jit-rank--with-marker nil
  "Non-nil in the child: print `xl-jit-rank--marker' before the report.")

(defun xl-jit-rank--report-from-output (output)
  "The report in a child's OUTPUT: what follows the marker, or nil."
  (let ((at (string-match (regexp-quote xl-jit-rank--marker) output)))
    (and at (string-trim-left
             (substring output (+ at (length xl-jit-rank--marker))) "\n+"))))

(defun xl-jit-rank--child-command ()
  "The child that prints the report: this binary, this profile, the same
init, then `xl-jit-report' in batch."
  (let* ((exe (expand-file-name invocation-name invocation-directory))
         (profile (file-name-as-directory user-emacs-directory)))
    (list exe "--batch" "--init-directory" profile
          "-l" (expand-file-name "early-init.el" profile)
          "--eval" "(run-hooks (quote before-init-hook))"
          "-l" (expand-file-name "init.el" profile)
          "--eval" "(progn (require (quote xl-jit-rank)) (setq xl-jit-rank--with-marker t) (xl-jit-report))")))

(defvar-local xl-jit-rank--center nil
  "The overlay that shows the loader in the middle of the window, while waiting.")

(defun xl-jit-rank--draw-center (frame)
  "Draw FRAME in the middle of this buffer's window while the report is pending.
nil removes it.  The loader is a `before-string' on an empty placeholder line,
so no text is added and the window does not move, as in GBlame."
  (if (null frame)
      (when (overlayp xl-jit-rank--center)
        (delete-overlay xl-jit-rank--center)
        (setq xl-jit-rank--center nil))
    (when-let* ((window (get-buffer-window (current-buffer) t)))
      (let* ((text (concat frame "  computing the ranking"))
             (pad (max 0 (/ (- (window-body-width window) (string-width text)) 2)))
             (pos (save-excursion
                    (goto-char (window-start window))
                    (forward-line (/ (window-body-height window) 2))
                    (line-beginning-position))))
        (unless (overlayp xl-jit-rank--center)
          (setq xl-jit-rank--center (make-overlay pos pos)))
        (move-overlay xl-jit-rank--center pos pos)
        (overlay-put xl-jit-rank--center 'before-string
                     (concat (make-string pad ?\s) text))))))

(defun xl-jit-rank--show-async (buffer-name)
  "Fill BUFFER-NAME with the report, computed in a child Emacs.
While it computes, a loader turns in the mode line and in the middle of the
window, on empty placeholder lines, as GBlame does."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Empty placeholder lines: the loader sits on one of them.
        (insert (make-string 400 ?\n)))
      (special-mode)
      (xl-spinner-mode-line-start "xl-jit report" :also #'xl-jit-rank--draw-center)
      (xl-async-run
       :name "xl-jit-report"
       :owner buf
       :slot 'xl-jit-report
       :command (xl-jit-rank--child-command)
       :on-done
       (lambda (output code)
         (xl-spinner-mode-line-stop)
         (xl-jit-rank--draw-center nil)
         (let ((report (xl-jit-rank--report-from-output output))
               (inhibit-read-only t))
           (erase-buffer)
           (if (and (eql code 0) report (not (string-empty-p (string-trim report))))
               (insert report)
             (insert "The report failed.\n\n" output))
           (goto-char (point-min))))))
    (display-buffer buf)
    buf))

(defun xl-jit-report ()
  "Show the ranking.  Interactive: a buffer, filled in the background.
Batch: printed to stdout, computed here."
  (interactive)
  (if noninteractive
      (progn
        (when xl-jit-rank--with-marker
          (princ (format "\n%s\n" xl-jit-rank--marker)))
        (princ (xl-jit-rank--format (xl-jit-rank--compute (xl-jit--profile-dir)))))
    (xl-jit-rank--show-async "*xl-jit report*")))

(provide 'xl-jit-rank)
;;; xl-jit-rank.el ends here
