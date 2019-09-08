;;; chronometrist-timer.el --- Timer-related functions for Chronometrist -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defun chronometrist-timer ()
  "Refresh Chronometrist and related buffers.

Buffers will be refreshed only if they are visible and the user
is clocked in to a project."
  (when (chronometrist-current-task) ;; FIXME - This line is currently
    ;; resulting in no refresh at midnight. When `chronometrist-entries' is
    ;; optimized to consume less CPU and avoid unnecessary parsing,
    ;; remove this condition.
    (when (get-buffer-window chronometrist-buffer-name)
      (chronometrist-refresh))
    (when (get-buffer-window chronometrist-report-buffer-name)
      (chronometrist-report-refresh))
    (when (get-buffer-window chronometrist-statistics-buffer-name)
      (chronometrist-statistics-refresh))))

(defun chronometrist-stop-timer ()
  (interactive)
  (cancel-timer chronometrist--timer-object)
  (setq chronometrist--timer-object nil))

(defun chronometrist-maybe-start-timer (&optional interactive-test)
  "Start `chronometrist-timer' if `chronometrist--timer-object' is non-nil."
  (interactive "p")
  (unless chronometrist--timer-object
    (setq chronometrist--timer-object
          (run-at-time t chronometrist-update-interval #'chronometrist-timer))
    (when interactive-test
      (message "Timer started."))
    t))

(defun chronometrist-force-restart-timer ()
  (interactive)
  (when chronometrist--timer-object
    (cancel-timer chronometrist--timer-object))
  (setq chronometrist--timer-object
        (run-at-time t chronometrist-update-interval #'chronometrist-timer)))

(defun chronometrist-change-update-interval (arg)
  (interactive "NEnter new interval (in seconds): ")
  (cancel-timer chronometrist--timer-object)
  (setq chronometrist-update-interval arg
        chronometrist--timer-object nil)
  (chronometrist-maybe-start-timer))

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

(provide 'chronometrist-timer)

;;; chronometrist-timer.el ends here
