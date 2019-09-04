(require 'dash)
(require 'seq)

(defvar chronometrist-file "~/.emacs.d/chronometrist.sexp"
  "Default path and name of chronometrist database.")

(defvar chronometrist-current-backend 'timeclock)

(defvar chronometrist-backends-alist
  '((timeclock . (:in-function  timeclock-in
                  :out-function timeclock-out
                  :file         timeclock-file))
    (sexp . (:in-function  chronometrist-in-sexp
             :out-function chronometrist-out-sexp
             :file         chronometrist-file))))

;;;; compatibility with timeclock.el
(defvar chronometrist-in-function 'timeclock-in
  "The function called by `chronometrist-in' to do its work.

It should accept the same arguments as `timeclock-in'.")

(defvar chronometrist-out-function 'timeclock-out
  "The function called by `chronometrist-out' to do its work.

It should accept the same arguments as `timeclock-out'.")

;; `call-interactively' doesn't work for programmatic use.

;; Calling as a regular function doesn't work for interactive use,
;; unless we duplicate behaviour from timeclock-in/out.

;; `funcall-interactively' requires a recent Emacs.
(defun chronometrist-in (&optional arg project find-project)
  "Start tracking time for a task.

`chronometrist-in-function' contains the actual function to be called."
  (interactive "P")
  (funcall-interactively (-> chronometrist-current-backend
                             (alist-get chronometrist-backends-alist)
                             (plist-get :in-function))
                         arg project find-project))

(defun chronometrist-out (&optional arg reason find-reason)
  "Stop tracking time.

`chronometrist-out-function' contains the actual function to be called."
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

(defun chronometrist-in-sexp (&optional prefix project find-project)
  "Add new time interval as an s-expression to `chronometrist-file'."
  (let ((buffer (find-file-noselect chronometrist-file))
        (tags   (plist-get plist :tags)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (when (not (bolp))
        (insert "\n"))
      (plist-pp
       (append `(:task ,task) (when tags `(:tags ,tags))
               (chronometrist-plist-remove plist :tags)
               `(:start ,(format-time-string "%FT%T%z")))
       buffer))))

(provide 'chronometrist-sexp)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:
