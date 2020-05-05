;;; chronometrist-goals.el --- Adds support for time-goals to Chronometrist -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Keywords: calendar
;; Homepage: https://framagit.org/contrapunctus/chronometrist
;; Package-Requires: ((emacs "25.1") (cl-lib "1.0") (alert "1.2") (chronometrist "0.4.3"))
;; Version: 0.1.0

(require 'chronometrist)
(require 'alert)

;;; Commentary:

;;; It is hoped that `chronometrist-goals-alert-functions' provides a good balance
;;; of flexibility and ease of use for the majority of use cases. A
;;; user desiring even greater control may define their own versions
;;; of `chronometrist-goals-run-alert-timers' and `chronometrist-goals-stop-alert-timers' (preferably using
;;; them as a template) and add them to the desired hooks.

;; TODO -
;; * clear notifications on file change event
;; * define types for custom variables
;; * behave differently when goal has been reached and we're starting again!

(defcustom chronometrist-goals-list nil
  "List to specify daily time goals for each task.
Each element must be in the form (GOAL TASK *).

GOAL is an integer specifying number of minutes.

TASK is the task on which you would like spend GOAL time.

There can be more than one TASK, to specify that you would
like to spend GOAL time on any one of those tasks."
  :group 'chronometrist
  :type '(repeat
          (list integer :value 15
                (repeat :inline t string))))

(defun chronometrist-run-at-time (time repeat function &rest args)
  "Like `run-at-time', but append timers to `chronometrist--timers-list'."
  (->> (apply #'run-at-time time repeat function args)
       (list)
       (append chronometrist--timers-list)
       (setq chronometrist--timers-list)))

(defun chronometrist-seconds->alert-string (seconds)
  "Convert SECONDS to a string suitable for displaying in alerts."
  (let ((m (% (/ seconds 60) 60))
        (h (/ seconds 3600)))
    (if (zerop h)
        (format "%s minutes" m)
      (format "%s hours and %s minutes" h m))))

(defun chronometrist-approach-alert (task goal)
  "Alert the user when they are 5 minutes away from reaching GOAL for TASK."
  ;; TODO - don't run if current > goal
  (when goal
    ;; FIXME - don't run after (- goal 5); run after (- goal 5 spent)
    (chronometrist-run-at-time (chronometrist-minutes-string (- goal 5))
                  nil
                  (lambda ()
                    (alert (format "5 minutes remain for %s" task))))))

(defun chronometrist-complete-alert (task goal)
  "Alert the user when they have reached the GOAL for TASK."
  ;; TODO - don't run if current > goal
  (when goal
    (chronometrist-run-at-time (chronometrist-minutes-string goal)
                  nil
                  (lambda ()
                    (alert (format "Goal for %s reached" task))))))

(defun chronometrist-exceed-alert (task goal)
  "Alert the user when they have exceeded the GOAL for TASK."
  (when goal
    ;; TODO - if current > (+ 5 goal), run this _now_ instead
    (chronometrist-run-at-time (chronometrist-minutes-string (+ goal 5))
                  nil
                  (lambda ()
                    (alert (format "You are exceeding the goal for %s!" task)
                           :severity 'high)))))

(defun chronometrist-no-goal-alert (task goal)
  "If TASK has no GOAL, regularly remind the user of the time they have spent on it."
  (unless goal
    (chronometrist-run-at-time (chronometrist-minutes-string 15)
                  (* 15 60) ;; repeat every 15 minutes
                  (lambda ()
                    (alert (format "You have spent %s on %s"
                                   (chronometrist-seconds->alert-string
                                    (chronometrist-task-time-one-day task))
                                   task))))))

(defcustom chronometrist-goals-alert-functions
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
`chronometrist-goals-stop-alert-timers'. `chronometrist-run-at-time'
will do that for you."
  :group 'chronometrist)

;; TODO - if there are multiple tasks associated with a single time goal (i.e. `(int "task1" "task2" ...)'), and the user has reached the goal for one of those tasks, don't display the goal for the other associated tasks
(defun chronometrist-get-goal (task &optional goals-list)
  "Return time goal for TASK from GOALS-LIST.
Return value is minutes as an integer, or nil.

If GOALS-LIST is not supplied, `chronometrist-time-goals-list' is used."
  (let ((goals-list (if goals-list
                        goals-list
                      chronometrist-goals-list)))
    (cl-loop for list in goals-list
             when (member task list)
             return (car list))))

(defvar chronometrist--timers-list nil)

(defun chronometrist-minutes-string (minutes)
  (format "%s minutes" minutes))

(defun chronometrist-goals-run-alert-timers (task)
  "Run timers to alert the user of the time spent on TASK.
To use, add this to `chronometrist-after-in-functions', and
`chronometrist-goals-stop-alert-timers' to
`chronometrist-after-out-functions'."
  (let ((goal  (chronometrist-get-goal task)))
    (add-hook 'chronometrist-file-change-hook #'chronometrist-goals-on-file-change)
    (mapc (lambda (f)
            (funcall f task goal))
          chronometrist-goals-alert-functions)))

(defun chronometrist-goals-stop-alert-timers (&optional _task)
  "Stop timers to alert the user of the time spent on TASK.
To use, add this to `chronometrist-after-out-functions', and
`chronometrist-goals-run-alert-timers' to
`chronometrist-after-in-functions'."
  (and chronometrist--timers-list ;; in case of start task -> exit Emacs without stopping -> start Emacs -> stop task
       (mapc #'cancel-timer chronometrist--timers-list)
       (setq chronometrist--timers-list   nil)))

(defun chronometrist-goals-on-file-change ()
  "Manage timed alerts when `chronometrist-file' changes."
  (let ((last (chronometrist-last-expr)))
    (chronometrist-goals-stop-alert-timers)
    ;; if there's a task running, start timed alerts for it
    (unless (plist-get last :stop)
      (chronometrist-goals-run-alert-timers (plist-get last :name)))))

(provide 'chronometrist-goals)

;; Local Variables:
;; nameless-current-name: "chronometrist-goals"
;; End:

;;; chronometrist-goals.el ends here
