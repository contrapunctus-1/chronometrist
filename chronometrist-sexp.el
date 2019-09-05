;;; chronometrist-sexp.el --- S-expression backend for Chronometrist

;;; Commentary:
;;

(require 'dash)
(require 'seq)

;;; Code:

(defvar chronometrist-file "~/.emacs.d/chronometrist.sexp"
  "Default path and name of chronometrist database.")

(defvar chronometrist-current-backend 'timeclock)



;; Calling as a regular function doesn't work for interactive use,
;; unless we duplicate behaviour from timeclock-in/out.

;; `funcall-interactively' requires a recent Emacs.
(defun chronometrist-in (&optional arg project find-project)
  "Start tracking time for a task.

ARG, PROJECT, and FIND-PROJECT are used as in `timeclock-in'.

The actual function to be called is determined by `chronometrist-in-function'."
  (interactive "P")
  (funcall-interactively (-> chronometrist-current-backend
                             (alist-get chronometrist-backends-alist)
                             (plist-get :in-function))
                         arg project find-project))

(defun chronometrist-out (&optional arg reason find-reason)
  "Stop tracking time.

ARG, REASON, and FIND-REASON are used as in `timeclock-out'.

The actual function to be called is determined by `chronometrist-out-function'."
  (interactive "P")
  (funcall-interactively  (-> chronometrist-current-backend
                             (alist-get chronometrist-backends-alist)
                             (plist-get :out-function))
                          arg reason find-reason))

;; Ugh. We have to use this so the argument list for timeclock-in/out
;; and chronometrist-in-plist/out-plist remain the same :\
(defvar chronometrist--event-plist nil
  "Property list to be used by `chronometrist-in-plist'.")

(defun chronometrist-plist-remove (plist &rest keys)
  "Return PLIST with KEYS and their associated values removed."
  (let ((keys (--filter (plist-member plist it) keys)))
    (mapc (lambda (key)
            (let ((pos (seq-position plist key)))
              (setq plist (append (seq-take plist pos)
                                  (seq-drop plist (+ 2 pos))))))
          keys)
    plist))

(defun chronometrist-in-sexp (task plist)
  "Add new time interval as an s-expression to `chronometrist-file'.

TASK is the name of the task, a string.

PLIST is a property list containing any other information about
this time interval that should be recorded."
  (let ((buffer (find-file-noselect chronometrist-file))
        (tags   (plist-get plist :tags)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "\n")
      (plist-pp (append `(:task ,task) (when tags `(:tags ,tags))
                        (chronometrist-plist-remove plist :tags)
                        `(:start ,(format-time-string "%FT%T%z")))
                buffer)
      (save-buffer))))

(defun chronometrist-delete-list (&optional arg)
  (let ((point-1 (point)))
    (forward-sexp (or arg 1))
    (delete-region point-1 (point))))

;; TODO - implement PLIST arg
(defun chronometrist-out-sexp (&optional plist)
  "Record current moment as stop time to last s-exp in `chronometrist-file'.

PLIST is a property list containing any other information about
this time interval that should be recorded."
  (let ((buffer (find-file-noselect chronometrist-file)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (unless (bobp) (insert "\n"))
      (backward-list 1)
      (--> (read buffer)
          (plist-put it :stop (format-time-string "%FT%T%z"))
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
