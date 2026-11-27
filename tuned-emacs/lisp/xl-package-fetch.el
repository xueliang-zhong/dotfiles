;;; xl-package-fetch.el --- download and check packages in parallel -*- lexical-binding: t; -*-

;;; Commentary:

;; `package.el' downloads and checks one package at a time, and most of that
;; time is spent waiting for the network and for gpg.  This file does those two
;; parts in parallel, then unpacks each package from its local file in
;; dependency order.  Anything it cannot finish is left for `package-install'
;; (see `xl-install-packages' in init.el), which runs afterwards.

;;; Code:

(require 'package)
(require 'seq)
(require 'cl-lib)
(require 'epg)

(defvar xl-fetch-max-jobs (* 2 (max 1 (num-processors)))
  "Most downloads or gpg checks that run at the same time: twice the CPU count.
They mostly wait on the network, so more than one per CPU keeps them busy.")

(defun xl-fetch--run (commands &optional on-done)
  "Run COMMANDS (argv lists) in parallel and wait for all of them.
At most `xl-fetch-max-jobs' run at once.  Return a list of
\(EXIT-CODE . OUTPUT), one per command, in the order of COMMANDS.
ON-DONE, when given, is called as (ON-DONE INDEX RESULT) as each command
finishes, with RESULT the (EXIT-CODE . OUTPUT) of that command."
  (let* ((count (length commands))
         (results (make-vector count nil))
         (queue (number-sequence 0 (1- count)))
         (running 0)
         (done 0))
    (cl-labels ((launch ()
                  (while (and queue (< running xl-fetch-max-jobs))
                    (let* ((index (pop queue))
                           (buffer (generate-new-buffer " *xl-fetch*")))
                      (make-process
                       :name "xl-fetch" :buffer buffer :noquery t
                       :command (nth index commands)
                       :sentinel
                       (lambda (proc _event)
                         (unless (process-live-p proc)
                           (aset results index
                                 (cons (process-exit-status proc)
                                       (with-current-buffer buffer
                                         (buffer-string))))
                           (kill-buffer buffer)
                           (cl-decf running)
                           (cl-incf done)
                           (when on-done
                             (funcall on-done index (aref results index)))
                           (launch))))
                      (cl-incf running)))))
      (launch)
      (while (< done count)
        (accept-process-output nil 0.1))
      (append results nil))))

(defun xl-fetch--wanted (names)
  "Descriptors to install for NAMES, dependencies first, without duplicates.
NAMES are packages missing from `package-alist'.  Their versions come from
`package-archive-contents', the same choice `package-install' makes."
  (let ((seen nil) (wanted nil))
    (dolist (name names)
      (when-let* ((desc (cadr (assq name package-archive-contents))))
        (dolist (dep (package-compute-transaction (list desc)
                                                  (package-desc-reqs desc)))
          (unless (or (member (package-desc-full-name dep) seen)
                      (package-installed-p (package-desc-name dep)
                                           (package-desc-version dep)))
            (push (package-desc-full-name dep) seen)
            (push dep wanted)))))
    (nreverse wanted)))

(defun xl-fetch--allow-unsigned-p (archive)
  "Non-nil when a package from ARCHIVE may be installed without a signature."
  (or (null (package-check-signature))
      (eq (package-check-signature) 'allow-unsigned)
      (member archive package-unsigned-archives)))

(defun xl-fetch-packages (names)
  "Install the packages in NAMES that are not installed yet, in parallel.
Download each archive file and its signature, check the signatures with gpg,
then unpack the good ones from their local files, dependencies first.  A
package that fails any step is skipped; the caller installs the rest."
  (let* ((descs (xl-fetch--wanted names))
         (dir (make-temp-file "xl-fetch-" t))
         (curl (or (executable-find "curl")
                   (error "xl-fetch: curl is needed for parallel downloads")))
         (gpg (or (executable-find epg-gpg-program)
                  (error "xl-fetch: gpg is needed for signature checks")))
         (home (expand-file-name package-gnupghome-dir))
         (files (mapcar (lambda (d) (concat (package-desc-full-name d)
                                            (package-desc-suffix d)))
                        descs))
         (sigs (mapcar (lambda (f) (concat f ".sig")) files))
         (urls (cl-mapcar (lambda (d f) (concat (package-archive-base d) f))
                       descs files))
         (local (mapcar (lambda (f) (expand-file-name f dir)) files))
         (sig-local (mapcar (lambda (f) (expand-file-name f dir)) sigs))
         ;; One archive file and one signature per package, in that order.
         (fetched
          (progn
            (when (and descs (fboundp 'xl-report))
              (xl-report "fetching & compiling package(s) in parallel -j %d ..." xl-fetch-max-jobs))
            (prog1
                (xl-fetch--run
                 (cl-loop for url in urls for file in local for sig in sig-local
                          append (list (list curl "-sSf" "-o" file url)
                                       (list curl "-s" "-o" sig "-w" "%{http_code}"
                                             (concat url ".sig"))))
                 ;; One dot per package whose archive file arrived (even indices).
                 (lambda (index result)
                   (when (and (bound-and-true-p xl-progress)
                              (zerop (% index 2))
                              (zerop (car result)))
                     (external-debugging-output ?.))))
              (when (and descs (bound-and-true-p xl-progress))
                (external-debugging-output ?\n))))))
    (unwind-protect
        (let* ((tar-ok (cl-loop for i from 0 below (length descs)
                                collect (zerop (car (nth (* 2 i) fetched)))))
               (sig-code (cl-loop for i from 0 below (length descs)
                                  collect (cdr (nth (1+ (* 2 i)) fetched))))
               (signed (cl-loop for i from 0 below (length descs)
                                collect (and (nth i tar-ok)
                                             (equal (nth i sig-code) "200"))))
               (checks
                (xl-fetch--run
                 (cl-loop for i from 0 below (length descs)
                          when (nth i signed)
                          collect (list gpg "--batch" "--quiet"
                                        "--homedir" home "--verify"
                                        (nth i sig-local) (nth i local)))))
               (check-ok (let ((rest checks))
                           (cl-loop for i from 0 below (length descs)
                                    collect (cond
                                             ((not (nth i tar-ok)) nil)
                                             ((nth i signed)
                                              (let ((r (pop rest)))
                                                (zerop (car r))))
                                             ((equal (nth i sig-code) "404")
                                              (xl-fetch--allow-unsigned-p
                                               (package-desc-archive
                                                (nth i descs))))
                                             (t nil)))))
               (installed 0))
          (cl-loop for desc in descs for file in local
                   for ok in check-ok for checked in signed
                   do (when (and ok (not (package-installed-p
                                          (package-desc-name desc)
                                          (package-desc-version desc))))
                        (condition-case err
                            (progn
                              (package-install-file file)
                              (cl-incf installed)
                              ;; One dot per package installed (byte-compiled by package.el).
                              (when (bound-and-true-p xl-progress)
                                (external-debugging-output ?.))
                              ;; The marker package.el writes for good signatures.
                              (when checked
                                (write-region "verified by xl-fetch-packages\n"
                                              nil
                                              (expand-file-name
                                               (concat (package-desc-full-name desc)
                                                       ".signed")
                                               package-user-dir)
                                              nil 'silent)))
                          (error
                           (message "xl-fetch: %s failed: %s"
                                    (package-desc-full-name desc)
                                    (error-message-string err))))))
          (when (and (bound-and-true-p xl-progress) (> installed 0))
            (external-debugging-output ?\n))
          (when (and (fboundp 'xl-report) (> installed 0))
            (xl-report "fetched %d package(s) in parallel" installed))
          installed)
      (delete-directory dir t))))

(provide 'xl-package-fetch)
;;; xl-package-fetch.el ends here
