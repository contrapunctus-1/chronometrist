;;; chronometrist-targets.el --- functions to implement time-targets

;;; Commentary:
;;

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

(provide 'chronometrist-targets)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

;;; chronometrist-targets.el ends here
