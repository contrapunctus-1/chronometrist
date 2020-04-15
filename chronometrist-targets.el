;;; chronometrist-targets.el --- Adds support for time-goals to Chronometrist -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Keywords: calendar
;; Homepage: https://framagit.org/contrapunctus/chronometrist
;; Package-Requires: ((emacs "25.1") (cl-lib "1.0"))
;; Version: 0.1.0

(require 'alert)

;;; Commentary:
;;


(defcustom chronometrist-time-targets-list nil
  "List to specify daily time goals for each project.

Each element must be in the form (TARGET PROJECT *).

TARGET is an integer specifying number of minutes.

PROJECT is the project on which you would like spend TARGET time.

There can be more than one PROJECT, to specify that you would
like to spend TARGET time on any one of those projects."
  :group 'chronometrist)

;; TODO - if there are multiple tasks associated with a single time
;; goal (i.e. `(int "task1" "task2" ...)'), and the user has reached the
;; goal for one of those tasks, don't display the goal for the other
;; associated tasks
(defun chronometrist-get-target (task &optional targets-list)
  "Return time target for TASK from TARGETS-LIST.
Return value is minutes as an integer, or nil.

If TARGETS-LIST is not supplied, `chronometrist-time-targets-list' is used."
  (let ((targets-list (if targets-list
                          targets-list
                        chronometrist-time-targets-list)))
    (cl-loop for list in targets-list
             when (member task list)
             return (car list))))

(defvar chronometrist-approach-timer nil)
(defvar chronometrist-complete-timer nil)
(defvar chronometrist-exceed-timer nil)

(defun chronometrist-approach-alert (task)
  (alert (format "5 minutes remain for %s" (downcase task))))

(defun chronometrist-complete-alert (task)
  (alert (format "Target for %s reached" (downcase task))))

(defun chronometrist-exceed-alert (task)
  (alert (format "You are exceeding the time for %s!" (downcase task))
         :severity 'high))

(defun chronometrist-minutes-string (minutes)
  (format "%s minutes" minutes))

(defun chronometrist-run-alert-timers (task)
  "Run timers to alert the user of the time spent on TASK.
Add this to `chronometrist-after-in-functions' to use."
  (let ((target (chronometrist-get-target task)))
    (when target ;; don't run for tasks which don't have a target defined
      (setq chronometrist-approach-timer
            (run-at-time (chronometrist-minutes-string (- target 5))
                         nil
                         #'chronometrist-approach-alert
                         task)
            chronometrist-complete-timer
            (run-at-time (chronometrist-minutes-string target)
                         nil
                         #'chronometrist-complete-alert
                         task)
            chronometrist-exceed-timer
            (run-at-time (chronometrist-minutes-string (+ target 5))
                         nil
                         #'chronometrist-exceed-alert
                         task)))))

(provide 'chronometrist-targets)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

;;; chronometrist-targets.el ends here
