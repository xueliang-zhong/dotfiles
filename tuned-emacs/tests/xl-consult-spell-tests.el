;;; xl-consult-spell-tests.el --- tests for xl-consult-spell -*- lexical-binding: t; -*-

;; Run: emacs --batch -Q -L tuned-emacs/lisp -L tuned-emacs/tests \
;;        -l tuned-emacs/tests/xl-consult-spell-tests.el \
;;        -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'xl-consult-spell)

;;; Output and candidates

(ert-deftest xl-consult-spell-lines-remove-windows-carriage-returns ()
  "CRLF output from a Windows checker does not keep the CR in a word."
  (should (equal '("recieve" "teh")
                 (xl-consult-spell--lines "recieve\r\nteh\r\n"))))

(ert-deftest xl-consult-spell-candidate-has-entry-and-group-properties ()
  "A candidate keeps its entry and carries the consult group property."
  (let* ((entry (list :buffer (current-buffer)
                      :pos 1
                      :line 3
                      :word "recieve"
                      :display "notes.org"))
         (candidate (xl-consult-spell--entry-candidate entry)))
    (should (equal "notes.org:3: recieve"
                   (substring-no-properties candidate)))
    (should (equal entry
                   (get-text-property 0 'xl-consult-spell-entry candidate)))
    (should (equal "notes.org"
                   (get-text-property 0 'consult--group candidate)))))

(ert-deftest xl-consult-spell-lookup-finds-the-propertized-candidate ()
  "Consult strips properties from the selected string; lookup restores them."
  (let* ((entry (list :buffer (current-buffer)
                      :pos 1
                      :line 1
                      :word "teh"
                      :display "notes.org"))
         (candidate (xl-consult-spell--entry-candidate entry))
         (selected (substring-no-properties candidate)))
    (should (equal entry
                   (xl-consult-spell--lookup selected (list candidate))))
    (should-not (xl-consult-spell--lookup "missing" (list candidate)))))

(ert-deftest xl-consult-spell-entry-position-ignores-empty-candidates ()
  "Consult's preview exit passes an empty string; it must be harmless."
  (should-not (xl-consult-spell--entry-position ""))
  (should-not (xl-consult-spell--entry-position nil)))

(ert-deftest xl-consult-spell-entry-position-is-a-marker ()
  "The position stored in an entry becomes a marker in its buffer."
  (with-temp-buffer
    (insert "one recieve two\n")
    (let* ((pos (progn (goto-char (point-min))
                       (search-forward "recieve")
                       (match-beginning 0)))
           (candidate (xl-consult-spell--entry-candidate
                       (list :buffer (current-buffer)
                             :pos pos
                             :line 1
                             :word "recieve"
                             :display (buffer-name))))
           (marker (xl-consult-spell--entry-position candidate)))
      (should marker)
      (should (= pos (marker-position marker))))))

;;; Word positions

(ert-deftest xl-consult-spell-locates-repeated-words-in-order ()
  "Repeated misspellings are matched in the checker's order."
  (with-temp-buffer
    (insert "teh one\nteh two\ncorrect\n")
    (let ((entries (xl-consult-spell--locate-words
                    '("teh" "teh") (current-buffer)
                    (point-min) (point-max))))
      (should (= 2 (length entries)))
      (should (= 1 (plist-get (nth 0 entries) :line)))
      (should (= 2 (plist-get (nth 1 entries) :line)))
      (should (equal "teh" (plist-get (nth 0 entries) :word))))))

(ert-deftest xl-consult-spell-locate-uses-word-boundaries ()
  "A misspelled `teh' does not match the `teh' inside `tehran'."
  (with-temp-buffer
    (insert "tehran teh\n")
    (let ((entries (xl-consult-spell--locate-words
                    '("teh") (current-buffer)
                    (point-min) (point-max))))
      (should (= 1 (length entries)))
      (should (= 8 (plist-get (car entries) :pos))))))

(ert-deftest xl-consult-spell-locate-handles-apostrophes ()
  "A word with an apostrophe is still found with word boundaries."
  (with-temp-buffer
    (insert "don't recieve\n")
    (let ((entries (xl-consult-spell--locate-words
                    '("don't") (current-buffer)
                    (point-min) (point-max))))
      (should (= 1 (length entries)))
      (should (= 1 (plist-get (car entries) :pos))))))

(ert-deftest xl-consult-spell-whole-lines-widens-a-region ()
  "A region in the middle of a line becomes whole lines."
  (with-temp-buffer
    (insert "first line\nsecond line\nthird line\n")
    (let ((lines (xl-consult-spell--whole-lines 3 16)))
      (should (= (point-min) (car lines)))
      (should (= (save-excursion
                   (goto-char 16)
                   (line-end-position))
                 (cadr lines))))))

;;; Checker programs

(ert-deftest xl-consult-spell-command-per-program ()
  "Each ispell-compatible program gets its own list-mode arguments."
  (cl-letf (((symbol-function 'xl-consult-spell--program)
             (lambda () "/opt/homebrew/bin/aspell")))
    (should (equal '("/opt/homebrew/bin/aspell" "list" "--encoding=utf-8")
                   (xl-consult-spell--command))))
  (cl-letf (((symbol-function 'xl-consult-spell--program)
             (lambda () "C:/tools/hunspell.exe")))
    (should (equal '("C:/tools/hunspell.exe" "-l" "-i" "utf-8")
                   (xl-consult-spell--command))))
  (cl-letf (((symbol-function 'xl-consult-spell--program)
             (lambda () "/usr/local/bin/ispell")))
    (should (equal '("/usr/local/bin/ispell" "-l")
                   (xl-consult-spell--command)))))

(ert-deftest xl-consult-spell-finds-aspell ()
  "The first executable program wins."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (program) (and (equal program "aspell") "/bin/aspell"))))
    (should (equal "/bin/aspell" (xl-consult-spell--program)))))

(ert-deftest xl-consult-spell-signals-a-helpful-error-without-a-checker ()
  "No checker is a user-error that names the available install paths."
  (cl-letf (((symbol-function 'xl-consult-spell--program) (lambda () nil)))
    (let ((error (should-error (xl-consult-spell--missing)
                               :type 'user-error)))
      (should (string-match-p "aspell" (error-message-string error)))
      (should (string-match-p "hunspell" (error-message-string error))))))

;;; Doctor

(ert-deftest xl-consult-spell-doctor-line-reports-missing-program ()
  "The doctor reports a missing program without signalling."
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
    (should (string-match-p "not found"
                            (xl-consult-spell--doctor-line
                             "ispell" "aspell" "--version")))))

(ert-deftest xl-consult-spell-doctor-line-reports-a-version ()
  "The doctor passes its remaining arguments to the version command."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_) "/bin/aspell"))
            ((symbol-function 'xl-consult-spell--version)
             (lambda (_program &rest args)
               (should (equal '("--version") args))
               "Aspell 0.60.8")))
    (should (string-match-p "Aspell 0.60.8"
                            (xl-consult-spell--doctor-line
                             "ispell" "aspell" "--version")))))

(provide 'xl-consult-spell-tests)

;;; xl-consult-spell-tests.el ends here
