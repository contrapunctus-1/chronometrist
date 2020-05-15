;;; chronometrist-sexp.el --- s-expression backend for Chronometrist -*- lexical-binding: t; -*-

;;; Commentary:
;;

(require 'chronometrist)

;;; Code:

(defun chronometrist-sexp-create-file ()
  "Create `chronometrist-file' if it doesn't already exist."
  (unless (file-exists-p chronometrist-file)
    (with-current-buffer (find-file-noselect chronometrist-file)
      (write-file chronometrist-file))))

;;;; Queries
(defun chronometrist-sexp-open-log ()
  "Open `chronometrist-file' in another window."
  (find-file-other-window chronometrist-file)
  (goto-char (point-max)))

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

;;;; Modifications
(cl-defun chronometrist-sexp-new (plist &optional (buffer (find-file-noselect chronometrist-file)))
  "Add new PLIST at the end of `chronometrist-file'.
Afterwards, save it and refresh the Chronometrist buffer.

BUFFER is the buffer to operate in - default is one accessing `chronometrist-file'."
  (with-current-buffer buffer
    (goto-char (point-max))
    ;; If we're adding the first s-exp in the file, don't add a
    ;; newline before it
    (unless (bobp) (insert "\n"))
    (unless (bolp) (insert "\n"))
    (chronometrist-plist-pp plist buffer)
    (save-buffer))
  (chronometrist-refresh))

(defun chronometrist-sexp-replace-last (plist)
  "Replace the last s-expression in `chronometrist-file' with PLIST."
  (let ((buffer (find-file-noselect chronometrist-file)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (unless (and (bobp) (bolp))
        (insert "\n"))
      (backward-list 1)
      (chronometrist-delete-list)
      (chronometrist-plist-pp plist buffer)
      (save-buffer))))

(defun chronometrist-sexp-reindent-buffer ()
  "Reindent the current buffer.
This is meant to be run in `chronometrist-file' when using the s-expression backend."
  (interactive)
  (let (expr)
    (goto-char (point-min))
    (while (setq expr (ignore-errors (read (current-buffer))))
      (backward-list)
      (chronometrist-delete-list)
      (when (looking-at "\n*")
        (delete-region (match-beginning 0)
                       (match-end 0)))
      (chronometrist-plist-pp expr (current-buffer))
      (insert "\n")
      (unless (eobp)
        (insert "\n")))))

(provide 'chronometrist-sexp)

;;; chronometrist-sexp.el ends here
