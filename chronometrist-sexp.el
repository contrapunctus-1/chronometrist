;;; chronometrist-sexp.el --- S-expression backend for Chronometrist

(require 'chronometrist-migrate)

;;; Commentary:
;;

(require 'dash)
(require 'seq)

;;; Code:

(defvar chronometrist-file "~/.emacs.d/chronometrist.sexp"
  "Default path and name of chronometrist database.")

(defun chronometrist-plist-remove (plist &rest keys)
  "Return PLIST with KEYS and their associated values removed."
  (let ((keys (--filter (plist-member plist it) keys)))
    (mapc (lambda (key)
            (let ((pos (seq-position plist key)))
              (setq plist (append (seq-take plist pos)
                                  (seq-drop plist (+ 2 pos))))))
          keys)
    plist))

(defun chronometrist-delete-list (&optional arg)
  "Delete ARG lists after point."
  (let ((point-1 (point)))
    (forward-sexp (or arg 1))
    (delete-region point-1 (point))))

(defun chronometrist-in (task &optional plist)
  "Add new time interval as an s-expression to `chronometrist-file'.

TASK is the name of the task, a string.

PLIST is a property list containing any other information about
this time interval that should be recorded."
  (let ((buffer (find-file-noselect chronometrist-file))
        (tags   (plist-get plist :tags)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (when (not (bobp)) (insert "\n"))
      (when (not (bolp)) (insert "\n"))
      (plist-pp (append `(:name ,task) (when tags `(:tags ,tags))
                        (chronometrist-plist-remove plist :tags)
                        `(:start ,(format-time-string "%FT%T%z")))
                buffer)
      (save-buffer))))

;; TODO - implement PLIST arg
(defun chronometrist-out (&optional plist)
  "Record current moment as stop time to last s-exp in `chronometrist-file'.

PLIST is a property list containing any other information about
this time interval that should be recorded."
  (let ((buffer (find-file-noselect chronometrist-file)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (unless (bobp) (insert "\n"))
      (backward-list 1)
      (--> (read buffer)
           (plist-put it :stop (chronometrist-current-time-iso8601))
           (progn
             (backward-list 1)
             (chronometrist-delete-list)
             (plist-pp it buffer)))
      (save-buffer))))

(provide 'chronometrist-sexp)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

;;; chronometrist-sexp.el ends here
