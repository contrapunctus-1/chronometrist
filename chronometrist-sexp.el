;;; chronometrist-sexp.el --- s-expression backend for Chronometrist -*- lexical-binding: t; -*-

;;; Commentary:
;;

(require 'chronometrist)

;;; Code:

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

(defun chronometrist-sexp-events-populate ()
  "Populate hash table `chronometrist-events'.
The data is acquired from `chronometrist-file'.

Return final number of events read from file, or nil if there
were none."
  (with-current-buffer (find-file-noselect chronometrist-file)
    (save-excursion
      (goto-char (point-min))
      (let ((index 0)
            expr
            pending-expr)
        (while (or pending-expr
                   (setq expr (ignore-errors (read (current-buffer)))))
          ;; find and split midnight-spanning events during deserialization itself
          (let* ((split-expr (chronometrist-events-maybe-split expr))
                 (new-value  (cond (pending-expr
                                    (prog1 pending-expr
                                      (setq pending-expr nil)))
                                   (split-expr
                                    (setq pending-expr (cl-second split-expr))
                                    (cl-first split-expr))
                                   (t expr)))
                 (new-value-date (->> (plist-get new-value :start)
                                      (s-left 10)))
                 (existing-value (gethash new-value-date chronometrist-events)))
            (unless pending-expr (cl-incf index))
            (puthash new-value-date
                     (if existing-value
                         (append existing-value
                                 (list new-value))
                       (list new-value))
                     chronometrist-events)))
        (unless (zerop index) index)))))

;;;; Modifications
(defun chronometrist-sexp-create-file ()
  "Create `chronometrist-file' if it doesn't already exist."
  (unless (file-exists-p chronometrist-file)
    (with-current-buffer (find-file-noselect chronometrist-file)
      (write-file chronometrist-file))))

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

(defun chronometrist-sexp-delete-list (&optional arg)
  "Delete ARG lists after point."
  (let ((point-1 (point)))
    (forward-sexp (or arg 1))
    (delete-region point-1 (point))))

(defun chronometrist-sexp-replace-last (plist)
  "Replace the last s-expression in `chronometrist-file' with PLIST."
  (let ((buffer (find-file-noselect chronometrist-file)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (unless (and (bobp) (bolp))
        (insert "\n"))
      (backward-list 1)
      (chronometrist-sexp-delete-list)
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
      (chronometrist-sexp-delete-list)
      (when (looking-at "\n*")
        (delete-region (match-beginning 0)
                       (match-end 0)))
      (chronometrist-plist-pp expr (current-buffer))
      (insert "\n")
      (unless (eobp)
        (insert "\n")))))

(provide 'chronometrist-sexp)

;;; chronometrist-sexp.el ends here
