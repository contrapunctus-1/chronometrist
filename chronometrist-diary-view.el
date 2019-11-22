;;; chronometrist-diary-view.el --- A diary-like view for Chronometrist -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>

;; TODO - add forward/backward, current date display (and don't scroll
;; past the actual data)

;; TODO - when the reason is "-", add that interval to the next reason

;; TODO - permit switching between time formats
;; - hours and minutes - "X hours, Y minutes"
;; - minutes only - "80 minutes"
;; - relaxed - "(almost|slightly over) 1 hour"
;; - strict time periods (using `chronometrist-seconds-to-hms')
;; - period start/end ("HH:MM to HH:MM")
;; Add variable to store format functions as list (first one is
;; default), command to cycle between them


;;; Commentary:
;;

;;; Code:

(defvar chronometrist-diary-buffer-name "*Chronometrist-Diary*"
  "Name for the buffer created by `chronometrist-diary'.")

(defvar chronometrist-diary--current-date nil
  "Stores the date for the buffer.")

(defun chronometrist-intervals-on (date)
  "Return a list of all time intervals on DATE.

DATE should be a list in the form \"YYYY-MM-DD\"

Each time interval is a string as returned by `chronometrist-seconds-to-hms'."
  (->> (gethash date chronometrist-events)
       (chronometrist-events->time-list)
       ;; Why were we calling `-partition' here?
       ;; (-partition 2)
       (--map (time-subtract (cadr it) (car it)))
       (--map (chronometrist-seconds-to-hms (cadr it)))))

;; "X minutes on PROJECT (REASON)"

;; TODO - think of a better way to show details concisely, ideally
;; combining tags and key-values
(defun chronometrist-diary-tasks-reasons-on (date)
  "Return a list of projects and reasons on DATE."
  (mapcar (lambda (plist)
            (let ((task    (plist-get plist :name))
                  (reason  (or (plist-get plist :comment) "")))
              (concat " on " task
                      (unless (equal reason "")
                        (concat " (" reason ")"))
                      "\n")))
          (gethash date chronometrist-events)))

(defun chronometrist-diary-refresh (&optional _ignore-auto _noconfirm date)
  "Refresh the `chronometrist-diary' buffer.

This does not re-read `chronometrist-file'.

Optional argument DATE should be a list in the form
\"YYYY-MM-DD\". If not supplied, today's date is used.

The optional arguments _IGNORE-AUTO and _NOCONFIRM are ignored,
and are present solely for the sake of using this function as a
value of `revert-buffer-function'."
  (let* ((date              (if date date (chronometrist-date)))
         (intervals         (->> (chronometrist-intervals-on date)
                                 (mapcar #'chronometrist-format-time)))
         (projects-reasons  (chronometrist-diary-tasks-reasons-on date))
         (inhibit-read-only t))
    (setq chronometrist-diary--current-date date)
    (chronometrist-common-clear-buffer chronometrist-diary-buffer-name)
    (seq-mapn #'insert intervals projects-reasons)))

(define-derived-mode chronometrist-diary-view-mode special-mode "Chronometrist-Diary"
  "A mode to view your activity today like a diary."
  (setq revert-buffer-function #'chronometrist-diary-refresh))

(defun chronometrist-diary-view (&optional date)
  "Display today's Chronometrist data in a diary-like view."
  (interactive)
  (switch-to-buffer
   (get-buffer-create chronometrist-diary-buffer-name))
  (chronometrist-diary-view-mode)
  (chronometrist-diary-refresh nil nil date))

;; Local Variables:
;; nameless-current-name: "chronometrist-diary"
;; End:

(provide 'chronometrist-diary-view)

;;; chronometrist-diary-view.el ends here
