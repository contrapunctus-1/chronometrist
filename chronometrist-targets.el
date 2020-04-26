;;; chronometrist-targets.el --- Adds support for time-goals to Chronometrist -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Keywords: calendar
;; Homepage: https://framagit.org/contrapunctus/chronometrist
;; Package-Requires: ((emacs "25.1") (cl-lib "1.0") (alert "1.2"))
;; Version: 0.1.0

(require 'alert)

;;; Commentary:

;;; It is hoped that `chronometrist-timed-alert-functions' provides a good balance
;;; of flexibility and ease of use for the majority of use cases. A
;;; user desiring even greater control may define their own versions
;;; of `chronometrist-run-alert-timers' and `chronometrist-stop-alert-timers' (preferably using
;;; them as a template) and add them to the desired hooks.

;; TODO -
;; * clear notifications on file change event
;; * define types for custom variables
;; * behave differently when target has been reached and we're starting again!

(defcustom chronometrist-time-targets-list nil
  "List to specify daily time goals for each project.
Each element must be in the form (TARGET PROJECT *).

TARGET is an integer specifying number of minutes.

PROJECT is the project on which you would like spend TARGET time.

There can be more than one PROJECT, to specify that you would
like to spend TARGET time on any one of those projects."
  :group 'chronometrist)

(defun chronometrist-run-at-time (time repeat function &rest args)
  "Like `run-at-time', but append timers to `chronometrist--timers-list'."
  (->> (run-at-time time repeat function &rest args)
       (list)
       (append chronometrist--timers-list)
       (setq chronometrist--timers-list)))

(defun chronometrist-approach-alert (task goal)
  ;; TODO - don't run if current > goal
  (when goal
    (chronometrist-run-at-time (chronometrist-minutes-string (- goal 5))
                  nil
                  (lambda ()
                    (alert (format "5 minutes remain for %s" task))))))

(defun chronometrist-complete-alert (task goal)
  ;; TODO - don't run if current > goal
  (when goal
    (chronometrist-run-at-time (chronometrist-minutes-string goal)
                  nil
                  (lambda ()
                    (alert (format "Target for %s reached" task))))))

(defun chronometrist-exceed-alert (task goal)
  (when goal
    ;; TODO - if current > (+ 5 goal), run this _now_ instead
    (chronometrist-run-at-time (chronometrist-minutes-string (+ goal 5))
                  nil
                  (lambda ()
                    (alert (format "You are exceeding the goal for %s!" task)
                           :severity 'high)))))

(defun chronometrist-no-goal-alert (task goal)
  (unless goal
    (chronometrist-run-at-time t
                  (* 15 60) ;; every 15 minutes
                  (lambda ()
                    (alert (format "You have spent %s time on %s"
                                   (chronometrist-task-time-one-day task)
                                   task))))))

(defcustom chronometrist-timed-alert-functions
  '(chronometrist-approach-alert
    chronometrist-complete-alert
    chronometrist-exceed-alert
    chronometrist-no-goal-alert)
  "List to describe timed alerts.
Each element should be a function, which will be called with one
argument - the name of the current task (as a string) and the
goal time for that task (minutes as an integer).

Typically, each function in this list should call `run-at-time'
to run another function, which in turn should call `alert' to
notify the user.

The timer returned by `run-at-time' should also be appended to
`chronometrist--timers-list', so that it can later be stopped by
`chronometrist-stop-alert-timers'. `chronometrist-run-at-time'
will do that for you."
  :group 'chronometrist)

;; TODO - if there are multiple tasks associated with a single time goal (i.e. `(int "task1" "task2" ...)'), and the user has reached the goal for one of those tasks, don't display the goal for the other associated tasks
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

(defvar chronometrist--timers-list nil)

(defun chronometrist-minutes-string (minutes)
  (format "%s minutes" minutes))

(defun chronometrist-run-alert-timers (task)
  "Run timers to alert the user of the time spent on TASK.
To use, add this to `chronometrist-after-in-functions', and
`chronometrist-stop-alert-timers' to
`chronometrist-after-out-functions'."
  (let ((current (-> (chronometrist-task-time-one-day task)
                     (chronometrist-format-time)))
        (target  (chronometrist-get-target task)))
    (mapc (funcall it task target)
          chronometrist-timed-alert-functions)))

(defun chronometrist-stop-alert-timers (&optional _task)
  "Stop timers to alert the user of the time spent on TASK.
To use, add this to `chronometrist-after-out-functions', and
`chronometrist-run-alert-timers' to
`chronometrist-after-in-functions'."
  ;; in case of start task -> exit Emacs without stopping -> start Emacs -> stop task
  (and chronometrist--timers-list
       (mapc #'cancel-timer chronometrist--timers-list)
       (setq chronometrist--timers-list   nil)))

(provide 'chronometrist-targets)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

;;; chronometrist-targets.el ends here
