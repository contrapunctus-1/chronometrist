;;; chronometrist-sexp.el --- s-expression backend for Chronometrist -*- lexical-binding: t; -*-

;;; Commentary:
;;

(defun chronometrist-sexp-create-file ()
  "Create `chronometrist-file' if it doesn't already exist."
  (unless (file-exists-p chronometrist-file)
    (with-current-buffer (find-file-noselect chronometrist-file)
      (write-file chronometrist-file))))

(defun chronometrist-sexp-last ()
  "Return last s-expression from `chronometrist-file'."
  (let ((buffer (find-file-noselect chronometrist-file)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (backward-list)
        (ignore-errors
          (read buffer))))))

(defun chronometrist-sexp-current-task ()
  "Return the name of the currently clocked-in task, or nil if not clocked in."
  (let ((last-event (chronometrist-sexp-last)))
    (if (plist-member last-event :stop)
        nil
      (plist-get last-event :name))))

(provide 'chronometrist-sexp)

;;; chronometrist-sexp.el ends here
