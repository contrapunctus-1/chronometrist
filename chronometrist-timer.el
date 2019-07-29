(defun chronometrist-timer ()
  "Refresh Chronometrist buffers if they are visible and the user
is clocked in to a project."
  (when (timeclock-currently-in-p) ;; This line is currently resulting
    ;; in no refresh at midnight. When `chronometrist-entries' is optimized to
    ;; consume less CPU and avoid unnecessary parsing, remove this
    ;; condition.
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

(defun chronometrist-maybe-start-timer ()
  "If `chronometrist--timer-object' is non-nil, add
`chronometrist-timer' to the list of active timers and return t,
else do nothing and return nil."
  (interactive)
  (unless chronometrist--timer-object
    (setq chronometrist--timer-object
          (run-at-time t chronometrist-update-interval #'chronometrist-timer))
    t))

(defun chronometrist-change-update-interval (arg)
  (interactive "NEnter new interval (in seconds): ")
  (cancel-timer chronometrist--timer-object)
  (setq chronometrist-update-interval arg
        chronometrist--timer-object nil)
  (chronometrist-maybe-start-timer))

(provide 'chronometrist-timer)
