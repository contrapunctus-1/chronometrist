;;; chronometrist-migrate.el --- Commands to aid in migrating from timeclock to chronometrist s-expr format

;;; Commentary:
;; TODO - convert project names to tags

;;; Code:

(defvar chronometrist-migrate-table (make-hash-table))

(defun chronometrist-migrate-populate (in-file)
  "Read data from `timeclock-file' to `chronometrist-migrate-table', storing events as plists."
  (clrhash chronometrist-migrate-table)
  (with-current-buffer (find-file-noselect in-file)
    (save-excursion
      (goto-char (point-min))
      (let ((key-counter 0))
        (while (not (= (point) (point-max)))
          (let* ((event-string       (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                 (event-list         (split-string event-string "[ /:]"))
                 (code               (first event-list))
                 (date-time          (--> event-list
                                          (seq-drop it 1)
                                          (seq-take it 6)
                                          (mapcar #'string-to-number it)))
                 (project-or-comment (replace-regexp-in-string
                                      (rx (and (or "i" "o") " "
                                               (and (= 4 digit) "/" (= 2 digit) "/" (= 2 digit) " ")
                                               (and (= 2 digit) ":" (= 2 digit) ":" (= 2 digit))
                                               (opt " ")))
                                      ""
                                      event-string)))
            (pcase code
              ("i"
               (incf key-counter)
               (puthash key-counter
                        `(:name ,project-or-comment :start ,date-time)
                        chronometrist-migrate-table))
              ("o"
               (--> (gethash key-counter chronometrist-migrate-table)
                    (append it
                            `(:stop ,date-time)
                            (when (and (stringp project-or-comment)
                                       (not
                                        (string= project-or-comment "")))
                              `(:comment ,project-or-comment)))
                    (puthash key-counter it chronometrist-migrate-table)))))
          (forward-line)
          (goto-char (point-at-bol))))
      nil)))

(defun chronometrist-migrate-timelog-file->sexp-file (&optional in-file out-file)
  (interactive `(,(read-file-name (concat "timeclock file (default: "
                                          timeclock-file "): ")
                                  "~/.emacs.d/"
                                  timeclock-file
                                  t)
                 ,(read-file-name "Output file (default: ~/.emacs.d/timelog.sexp): "
                                  "~/.emacs.d/"
                                  "~/.emacs.d/timelog.sexp")))
  (let ((output (find-file-noselect out-file)))
    (with-current-buffer output
      (chronometrist-common-clear-buffer output)
      (chronometrist-migrate-populate in-file)
      (maphash (lambda (key value)
                 (plist-pp value output))
               chronometrist-migrate-table)
      (save-buffer))))

(provide 'chronometrist-migrate)

;;; chronometrist-migrate.el ends here

;; Local Variables:
;; nameless-current-name: "chronometrist-migrate"
;; End:
