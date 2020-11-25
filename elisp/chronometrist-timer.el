;;; chronometrist-timer.el --- Timer-related functions for Chronometrist -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; For more information, please refer to <https://unlicense.org>

;;; Commentary:
;;

;;; Code:
(require 'chronometrist)

(declare-function chronometrist-refresh "chronometrist.el")
(declare-function chronometrist-report-refresh "chronometrist-report.el")
(declare-function chronometrist-statistics-refresh "chronometrist-statistics.el")

(defvar chronometrist--timer-object nil)

(defun chronometrist-timer ()
  "Refresh Chronometrist and related buffers.

Buffers will be refreshed only if they are visible and the user
is clocked in to a task."
  (when (chronometrist-backend-current-task chronometrist-backend-current) ;; FIXME - This line is currently
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
  "Stop the timer for Chronometrist buffers."
  (interactive)
  (cancel-timer chronometrist--timer-object)
  (setq chronometrist--timer-object nil))

(defun chronometrist-maybe-start-timer (&optional interactive-test)
  "Start `chronometrist-timer' if `chronometrist--timer-object' is non-nil.

INTERACTIVE-TEST is used to determine if this has been called
interactively."
  (interactive "p")
  (unless chronometrist--timer-object
    (setq chronometrist--timer-object
          (run-at-time t chronometrist-update-interval #'chronometrist-timer))
    (when interactive-test
      (message "Timer started."))
    t))

(defun chronometrist-force-restart-timer ()
  "Restart the timer for Chronometrist buffers."
  (interactive)
  (when chronometrist--timer-object
    (cancel-timer chronometrist--timer-object))
  (setq chronometrist--timer-object
        (run-at-time t chronometrist-update-interval #'chronometrist-timer)))

(defun chronometrist-change-update-interval (arg)
  "Change the update interval for Chronometrist buffers.

ARG should be the new update interval, in seconds."
  (interactive "NEnter new interval (in seconds): ")
  (cancel-timer chronometrist--timer-object)
  (setq chronometrist-update-interval arg
        chronometrist--timer-object nil)
  (chronometrist-maybe-start-timer))

(provide 'chronometrist-timer)

;;; chronometrist-timer.el ends here
