;;; chronometrist-sexp.el --- s-expression backend for Chronometrist -*- lexical-binding: t; -*-

;;; Commentary:
;;

(require 'chronometrist-backend)
(require 'chronometrist-custom)

;;; Code:

;; chronometrist-file (-custom)
;; chronometrist-events, chronometrist-events-maybe-split (-events)

(defclass chronometrist-sexp (chronometrist-backend) nil)
(defvar chronometrist-sexp-backend (make-instance chronometrist-sexp :name "sexp" :ext "sexp"))

(defcustom chronometrist-sexp-pretty-print-function
  (if (featurep 'ppp)
      (lambda (object &optional _stream)
        (insert (ppp-plist-to-string object)))
    #'pp)
  "Function used to pretty print plists in `chronometrist-file'.
Like `pp', it must accept an OBJECT and optionally a
STREAM (which is the value of `current-buffer').

Uses `ppp' if it is available (which displays plists more
neatly), or falls back to `pp' if it isn't."
  :type 'function)

(defmacro chronometrist-sexp-in-file (file &rest body)
  "Run BODY in a buffer visiting FILE, restoring point afterwards."
  (declare (indent defun) (debug t))
  `(with-current-buffer (find-file-noselect ,file)
     (save-excursion ,@body)))

;; # Migration #
(cl-defmethod chronometrist-backend-to-hash ((backend chronometrist-sexp) table)
  (clrhash table)
  (chronometrist-sexp-in-file (chronometrist-file-path)
    (goto-char (point-min))
    (let ((index 0) expr pending-expr)
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
               (new-value-date (s-left 10 (plist-get new-value :start)))
               (existing-value (gethash new-value-date chronometrist-events)))
          (unless pending-expr (cl-incf index))
          (puthash new-value-date
                   (if existing-value
                       (append existing-value
                               (list new-value))
                     (list new-value))
                   chronometrist-events)))
      (unless (zerop index) index))))

(cl-defmethod chronometrist-backend-from-hash ((backend chronometrist-sexp) table))

;; # Queries #
(cl-defmethod chronometrist-backend-open-file ((backend chronometrist-sexp))
  (find-file-other-window (chronometrist-file-path))
  (goto-char (point-max)))

(cl-defmethod chronometrist-backend-latest-record ((backend chronometrist-sexp))
  (chronometrist-sexp-in-file (chronometrist-file-path)
    (goto-char (point-max))
    (backward-list)
    (ignore-errors (read (current-buffer)))))

(cl-defmethod chronometrist-backend-current-task ((backend chronometrist-sexp))
  (let ((last-event (chronometrist-backend-latest-record backend)))
    (unless (plist-member last-event :stop)
      (plist-get last-event :name))))

;; # Modifications #
(cl-defmethod chronometrist-backend-create-file ((backend chronometrist-sexp))
  (unless (file-exists-p (chronometrist-file-path))
    (with-current-buffer (find-file-noselect (chronometrist-file-path))
      (write-file (chronometrist-file-path)))))

(cl-defmethod chronometrist-backend-new-record ((backend chronometrist-sexp) plist)
  (chronometrist-sexp-in-file (chronometrist-file-path)
    (goto-char (point-max))
    ;; If we're adding the first s-exp in the file, don't add a
    ;; newline before it
    (unless (bobp) (insert "\n"))
    (unless (bolp) (insert "\n"))
    (funcall chronometrist-sexp-pretty-print-function plist (current-buffer))
    ;; Update in-memory (`chronometrist-events', `chronometrist-task-list') too...
    (chronometrist-events-add plist)
    (chronometrist-task-list-add (plist-get plist :name))
    (chronometrist-tags-history-add plist)
    ;; ...so we can skip some expensive operations.
    (setq chronometrist--inhibit-read-p t)
    (save-buffer)))

(defun chronometrist-sexp-delete-list (&optional arg)
  "Delete ARG lists after point."
  (let ((point-1 (point)))
    (forward-sexp (or arg 1))
    (delete-region point-1 (point))))

(cl-defmethod chronometrist-backend-replace-last ((backend chronometrist-sexp) plist)
  (chronometrist-sexp-in-file (chronometrist-file-path)
    (goto-char (point-max))
    (unless (and (bobp) (bolp))
      (insert "\n"))
    (backward-list 1)
    (chronometrist-sexp-delete-list)
    (funcall chronometrist-sexp-pretty-print-function plist (current-buffer))
    (chronometrist-events-replace-last plist)
    ;; We assume here that this function will always be used to
    ;; replace something with the same :name. At the time of writing,
    ;; this is indeed the case. The reason for this is that if the
    ;; replaced plist is the only one in `chronometrist-file' with that :name, the
    ;; :name should be removed from `chronometrist-task-list', but to ascertain
    ;; that condition we would have to either read the entire file or
    ;; map over the hash table, defeating the optimization. Thus, we
    ;; don't update `chronometrist-task-list' here (unlike `chronometrist-backend-new-record')
    (chronometrist-tags-history-replace-last plist)
    (setq chronometrist--inhibit-read-p t)
    (save-buffer)))

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
      (funcall chronometrist-sexp-pretty-print-function expr (current-buffer))
      (insert "\n")
      (unless (eobp)
        (insert "\n")))))

(provide 'chronometrist-sexp)

;;; chronometrist-sexp.el ends here
