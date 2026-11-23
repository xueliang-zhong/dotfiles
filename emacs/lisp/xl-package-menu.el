;;; xl-package-menu.el --- Animated loader for the Package Menu  -*- lexical-binding: t; -*-

;; Author: Xueliang Zhong
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, convenience

;;; Commentary:
;; Emacs 31 already refreshes the archives asynchronously
;; (`package-menu-async', `package--downloads-in-progress'), and
;; `list-packages' shows its cached list while the downloads run.  What it
;; lacks is a moving indicator: the mode line only says ":Loading".
;;
;; This file keeps that asynchronous core untouched and shows the same
;; mode-line loader as the Git buffers instead: the `xl-spinner' slot at the
;; left edge of the mode line, visible while the archives download, so
;; `list-packages' never looks stuck and the list itself never moves.

;;; Code:

(require 'package)
(require 'xl-spinner)
(require 'xl-package-upgrades) ; the "can be upgraded" count, from a child Emacs

(defun xl-package-menu--start ()
  "Start the loader in this Package Menu buffer.
A refresh that never reports completion gives up after a minute of
frames, so a broken archive cannot leave the loader spinning forever."
  (xl-spinner-mode-line-start "Loading archives" :limit 600))

(defun xl-package-menu--stop ()
  "Stop the loader in this Package Menu buffer."
  (xl-spinner-mode-line-stop))

(defun xl-package-menu--on-refresh (&optional _async)
  "Show the loader in the Package Menu while the archives download.
Runs from `package-refresh-contents-hook', which both the asynchronous
and the synchronous paths go through."
  (when package-archives
    (when-let* ((buffer (get-buffer "*Packages*")))
      (with-current-buffer buffer
        (when (derived-mode-p 'package-menu-mode)
          (xl-package-menu--start))))))

(defun xl-package-menu--on-done ()
  "Stop the loader once the archives have been downloaded.
Runs from `package--post-download-archives-hook', which package.el runs
when the last download finishes."
  (when-let* ((buffer (get-buffer "*Packages*")))
    (with-current-buffer buffer
      (when (derived-mode-p 'package-menu-mode)
        (xl-package-menu--stop)))))

;; ============================================================
;; Chunked listing
;; ============================================================
;; `package-menu--generate' computes every row's status, sorts the rows by
;; the Status column and prints them, all in one call on the main thread:
;; about 0.5 s with ten thousand packages, during which Emacs does not
;; respond.  The listing below does the same work in chunks.  Each chunk
;; ends with a zero-second timer, so Emacs can redraw and take input
;; between two chunks.  The result is the same list, the same rows and the
;; same point as package.el's; only the time it takes to appear changes.
;; Listing all packages with no keyword filter is the case this handles.
;; Anything else is package.el's own function.

(defcustom xl-package-menu-status-chunk 150
  "Packages whose status is computed between two redraws."
  :type 'integer
  :group 'package)

(defcustom xl-package-menu-row-chunk 300
  "Rows printed between two redraws."
  :type 'integer
  :group 'package)

(defvar-local xl-package-menu--generation 0
  "Counts the listings of this Package Menu; an older listing stops.")

(defvar-local xl-package-menu--listing-timer nil
  "Timer of the listing chunk that runs next in this Package Menu, if any.")

(defvar-local xl-package-menu--listing-p nil
  "Non-nil while this Package Menu is being listed chunk by chunk.")

(defun xl-package-menu--info-list ()
  "Return the packages a full listing shows, as `package-menu--refresh' does.
The list is in the same order, so rows that compare equal keep their order."
  (let ((hidden-names (mapconcat #'identity package-hidden-regexps "\\|"))
        info-list)
    (dolist (elt package-alist)
      (dolist (pkg (cdr elt))
        (push pkg info-list)))
    (dolist (elt (package--builtin-alist))
      (let ((pkg (cadr elt)))
        (when (or package-list-unversioned (package-desc-version pkg))
          (push pkg info-list))))
    (unless (equal package--old-archive-priorities package-archive-priorities)
      (package-read-all-archive-contents))
    (dolist (elt package-archive-contents)
      (let ((name (car elt)))
        (when (or (not package-menu--hide-packages)
                  (not package-hidden-regexps)
                  (not (string-match hidden-names (symbol-name name))))
          (dolist (pkg (package--remove-hidden (cdr elt)))
            (push pkg info-list)))))
    info-list))

(defun xl-package-menu--cancel-listing ()
  "Stop the listing of this Package Menu, if one runs."
  (when (timerp xl-package-menu--listing-timer)
    (cancel-timer xl-package-menu--listing-timer))
  (setq xl-package-menu--listing-timer nil)
  (when xl-package-menu--listing-p
    (setq xl-package-menu--listing-p nil)
    (xl-spinner-mode-line-stop)))

(defun xl-package-menu--current-p (buffer generation)
  "Return non-nil when BUFFER still wants the listing GENERATION."
  (and (buffer-live-p buffer)
       (= generation (buffer-local-value 'xl-package-menu--generation buffer))))

(defun xl-package-menu--status-chunk (buffer generation todo done total id column)
  "Compute the entries of TODO, a chunk at a time, into DONE.
TOTAL is the number of packages, for the mode-line label.  ID and COLUMN
are where point was when the listing started, if REMEMBER-POS was given."
  (when (xl-package-menu--current-p buffer generation)
    (with-current-buffer buffer
      (let ((n 0))
        (while (and todo (< n xl-package-menu-status-chunk))
          (push (package-menu--print-info-simple (pop todo)) done)
          (setq n (1+ n))))
      (xl-spinner-mode-line-label
       (format "Listing packages %d/%d" (- total (length todo)) total))
      (if todo
          (setq xl-package-menu--listing-timer
                (run-with-timer 0 nil #'xl-package-menu--status-chunk
                                buffer generation todo done total id column))
        (setq xl-package-menu--listing-timer nil)
        (xl-package-menu--print-start buffer generation (nreverse done)
                                      id column)))))

(defun xl-package-menu--print-start (buffer generation entries id column)
  "Sort ENTRIES as the table does, clear the buffer and print them in chunks.
ID and COLUMN are where point goes afterwards, if given."
  (when (xl-package-menu--current-p buffer generation)
    (with-current-buffer buffer
      (let* ((sorter (tabulated-list--get-sorter))
             (sorted (if sorter (sort entries sorter) entries))
             (inhibit-read-only t))
        (setq tabulated-list-entries sorted)
        (erase-buffer)
        (unless tabulated-list-use-header-line
          (tabulated-list-print-fake-header))
        (setq xl-package-menu--listing-timer
              (run-with-timer 0 nil #'xl-package-menu--print-chunk
                              buffer generation sorter sorted id column))))))

(defun xl-package-menu--print-chunk (buffer generation sorter rest id column)
  "Print the next chunk of REST, the rows still to print.
ID and COLUMN are where point was before the listing, to restore it."
  (when (xl-package-menu--current-p buffer generation)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (chunk nil)
            (n 0))
        (while (and rest (< n xl-package-menu-row-chunk))
          (push (pop rest) chunk)
          (setq n (1+ n)))
        (goto-char (point-max))
        (tabulated-list-print-entries (nreverse chunk) sorter nil nil)
        (if rest
            (setq xl-package-menu--listing-timer
                  (run-with-timer 0 nil #'xl-package-menu--print-chunk
                                  buffer generation sorter rest id column))
          (setq xl-package-menu--listing-timer nil)
          (xl-package-menu--finish-listing id column))))))

(defun xl-package-menu--finish-listing (id column)
  "End the listing: point goes to ID at COLUMN, or the top; clear the loader."
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (when id
    (while (and (not (eobp)) (not (equal (tabulated-list-get-id) id)))
      (forward-line 1))
    (unless (eobp)
      (move-to-column column)))
  (when (eobp) (goto-char (point-min)))
  (setq xl-package-menu--listing-p nil)
  (xl-spinner-mode-line-stop))

(defcustom xl-package-menu-archive-chunk 500
  "Archive entries merged into the package list between two redraws."
  :type 'integer
  :group 'package)

(defun xl-package-menu--archives-stale-p ()
  "Return non-nil when the archives must be read again before listing.
This is package.el's own test, in `package-menu--refresh'."
  (or (null package-archive-contents)
      (not (equal package--old-archive-priorities package-archive-priorities))))

(defun xl-package-menu--read-archives (buffer generation archives pending continue)
  "Merge the archives into `package-archive-contents', a chunk at a time.
ARCHIVES are the archives still to read; PENDING are (ARCHIVE . PACKAGE)
pairs of the archive being merged.  CONTINUE is called once all are merged.
Each archive file is read in one go, which takes a few tens of
milliseconds; the merge of its entries is what runs in chunks.  This is
what `package-read-all-archive-contents' does, split up."
  (when (xl-package-menu--current-p buffer generation)
    (cond
     (pending
      (let ((n 0))
        (while (and pending (< n xl-package-menu-archive-chunk))
          (let ((item (pop pending)))
            (package--add-to-archive-contents (cdr item) (car item)))
          (setq n (1+ n))))
      (setq xl-package-menu--listing-timer
            (run-with-timer 0 nil #'xl-package-menu--read-archives
                            buffer generation archives pending continue)))
     (archives
      (let* ((archive (car (car archives)))
             (contents (package--read-archive-file
                        (format "archives/%s/archive-contents" archive)))
             (entries nil))
        (dolist (package contents)
          (if package
              (push (cons archive package) entries)
            (lwarn '(package refresh) :warning
                   "Ignoring nil package on `%s' package archive" archive)))
        (setq xl-package-menu--listing-timer
              (run-with-timer 0 nil #'xl-package-menu--read-archives
                              buffer generation (cdr archives)
                              (nreverse entries) continue))))
     (t
      (setq xl-package-menu--listing-timer nil)
      (funcall continue)))))

(defun xl-package-menu--begin (buffer generation id column)
  "List the packages of BUFFER, now that the archives are merged."
  (when (xl-package-menu--current-p buffer generation)
    (with-current-buffer buffer
      (let ((infos (xl-package-menu--info-list)))
        (setf (car (aref tabulated-list-format 0)) "Package")
        (tabulated-list-init-header)
        (xl-package-menu--status-chunk buffer generation infos nil
                                       (length infos) id column)))))

(defun xl-package-menu--generate-around (orig remember-pos &optional packages keywords)
  "List the packages in chunks; ORIG is `package-menu--generate'.
Only the full listing is chunked: a keyword filter or a list of packages
goes to package.el's function."
  (if (not (and (or (null packages) (eq packages t)) (null keywords)))
      (funcall orig remember-pos packages keywords)
    (xl-package-menu--cancel-listing)
    (setq xl-package-menu--generation (1+ xl-package-menu--generation))
    (let* ((generation xl-package-menu--generation)
           (buffer (current-buffer))
           (id (and remember-pos (tabulated-list-get-id)))
           (column (current-column)))
      (setq xl-package-menu--listing-p t)
      (xl-spinner-mode-line-start "Listing packages")
      (if (xl-package-menu--archives-stale-p)
          (progn
            (setq package-archive-contents nil
                  package--old-archive-priorities package-archive-priorities)
            (xl-package-menu--read-archives
             buffer generation package-archives nil
             (lambda () (xl-package-menu--begin buffer generation id column))))
        (xl-package-menu--begin buffer generation id column)))))

(advice-add 'package-menu--generate :around #'xl-package-menu--generate-around)

;; `list-packages' initializes package.el on its first call, and
;; `package-initialize' reads every archive synchronously.  While
;; `list-packages' runs, that read is skipped: the listing reads the
;; archives itself, in chunks (see `xl-package-menu--read-archives').
;; Every other caller reads as before.
(defvar xl-package-menu--defer-archive-reads nil
  "Non-nil while `list-packages' runs; the listing reads the archives.")

(defun xl-package-menu--skip-archive-read-around (orig)
  "Run ORIG, the archive read, unless `list-packages' defers it."
  (unless xl-package-menu--defer-archive-reads
    (funcall orig)))

(defun xl-package-menu--list-packages-around (orig &rest args)
  "Run `list-packages' ORIG with its archive read deferred to the listing.
Only with `package-menu-async' is the listing certain to read the archives:
otherwise nothing would, so the read stays where it was."
  (let ((xl-package-menu--defer-archive-reads package-menu-async))
    (apply orig args)))

(advice-add 'package-read-all-archive-contents :around
            #'xl-package-menu--skip-archive-read-around)
(advice-add 'list-packages :around #'xl-package-menu--list-packages-around)

(defun xl-package-menu-setup ()
  "Put the animated loader in this Package Menu's mode line.
`package-menu-mode' displays a static `:Loading' in `mode-line-process'
while `package--downloads-in-progress' is non-nil; the loader replaces
it, so that slot keeps only the transaction status."
  (xl-spinner-mode-line-install)
  (setq-local mode-line-process
              '(package-menu--transaction-status
                package-menu--transaction-status)))

(add-hook 'package-menu-mode-hook #'xl-package-menu-setup)
(add-hook 'package-refresh-contents-hook #'xl-package-menu--on-refresh)
(add-hook 'package--post-download-archives-hook #'xl-package-menu--on-done)

(provide 'xl-package-menu)
;;; xl-package-menu.el ends here
