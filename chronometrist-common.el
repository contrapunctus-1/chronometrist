;;; chronometrist-common.el --- Common definitions for Chronometrist -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>

;;; Commentary:
;;

(require 'dash)
(require 'cl-lib)
(require 'chronometrist-custom)
(require 'chronometrist-report-custom)
(require 'chronometrist-time)

;; ## VARIABLES ##
;;; Code:

(defvar chronometrist-empty-time-string "-")

(defvar chronometrist-date-re "[0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\}")

(defvar chronometrist-time-re-ui
  (rx-to-string
   `(or
     (and (repeat 0 2
                  (optional (repeat 1 2 digit) ":"))
          (repeat 1 2 digit))
     ,chronometrist-empty-time-string))
  "Regular expression to represent a timestamp in `chronometrist'.
This is distinct from `chronometrist-time-re-file' (which see) -
`chronometrist-time-re-ui' is meant for the user interface, and
must correspond to the output from `chronometrist-format-time'.")

(defvar chronometrist-time-re-file "[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}"
  "Regular expression to represent a timestamp in the file `timeclock-file'.
This is distinct from `chronometrist-time-re-ui' (which see).")

(defvar chronometrist-task-list nil
  "List of tasks in `chronometrist-file', as returned by `chronometrist-tasks-from-table'.")

(defun chronometrist-buffer-exists? (buffer-name)
  "Return non-nil if BUFFER-NAME exists."
  (--> (buffer-list)
       (mapcar #'buffer-name it)
       (member buffer-name it)))

(defun chronometrist-buffer-visible? (buffer-or-buffer-name)
  "Return t if BUFFER-OR-BUFFER-NAME is visible to user."
  ;; It'd be simpler to use only the windows of the current frame (-->
  ;; (selected-frame) (window-list it) ...) - but it wouldn't be
  ;; robust, because it is possible that a frame partially covers
  ;; another and the buffer is visible to the user from the latter.
  (let ((result (-->
                 (visible-frame-list)
                 (mapcar #'window-list it)
                 (mapcar (lambda (list)
                           (mapcar #'window-buffer list))
                         it)
                 (mapcar (lambda (list)
                           (mapcar (lambda (buffer)
                                     (if (bufferp buffer-or-buffer-name)
                                         (equal buffer-or-buffer-name buffer)
                                       (equal (buffer-name buffer)
                                              buffer-or-buffer-name)))
                                   list))
                         it)
                 (mapcar (lambda (list)
                           (-filter #'identity list))
                         it)
                 (mapcar #'car it)
                 (car it))))
    (if result t nil)))

(defun chronometrist-get-end-time (target-date)
  "Return the timestamp of the next clock-out event after point.

This is meant to be run in `timeclock-file'.

If there is no clock-out event after point, return the current
date and time.

If the clock-out time is past midnight, return the date in
TARGET-DATE with the time at midnight (\"24:00:00\").

Return value is a string in the form \"YYYY/MM/DD HH:MM:SS\"

TARGET-DATE must be a date in the form \"YYYY/MM/DD\"

Point must be on a clock-in event having the same date as
TARGET-DATE."
  (declare (obsolete nil "Chronometrist v0.3.0"))
  (let* ((date-time (if (progn
                          (forward-line)
                          (beginning-of-line)
                          (looking-at-p "^o "))
                        (progn
                          (re-search-forward "o ")
                          (buffer-substring-no-properties (point)
                                                          (+ 10 1 8 (point))))
                      (format-time-string "%Y/%m/%d %T")))
         (date-time-list   (chronometrist-timestamp->list date-time))
         (target-date-list (chronometrist-timestamp->list target-date)))
    (if (chronometrist-time-interval-span-midnight? target-date-list date-time-list)
        (concat target-date " 24:00:00")
      date-time)))

;; tests -
;; (mapcar #'chronometrist-format-time
;;         '((0 0 0) (0 0 1) (0 0 10) (0 1 10) (0 10 10) (1 10 10) (10 10 10)))
;; => ("" "00:01" "00:10" "01:10" "10:10" "01:10:10" "10:10:10")
;; (mapcar #'chronometrist-format-time
;;         '([0 0 0] [0 0 1] [0 0 10] [0 1 10] [0 10 10] [1 10 10] [10 10 10]))
;; => ("" "00:01" "00:10" "01:10" "10:10" "01:10:10" "10:10:10")
(defun chronometrist-format-time (duration)
  "Format DURATION as a string suitable for display in Chronometrist buffers.
DURATION must be a vector or a list of the form [HOURS MINUTES
SECONDS] or (HOURS MINUTES SECONDS)."
  (let ((h (elt duration 0))
        (m (elt duration 1))
        (s (elt duration 2))
        (blank "   "))
    (if (and (zerop h) (zerop m) (zerop s))
        "       -"
      (let ((h (if (zerop h)
                   blank
                 (format "%2d:" h)))
            (m (cond ((and (zerop h)
                           (zerop m))
                      blank)
                     ((zerop h)
                      (format "%2d:" m))
                     (t
                      (format "%02d:" m))))
            (s (if (and (zerop h)
                        (zerop m))
                   (format "%2d" s)
                 (format "%02d" s))))
        (concat h m s)))))

(defun chronometrist-open-file (&optional _button)
  "Open `chronometrist-file' in another window.

Argument _BUTTON is for the purpose of using this command as a
button action."
  (interactive)
  (find-file-other-window chronometrist-file)
  (goto-char (point-max)))

(defun chronometrist-common-create-chronometrist-file ()
  "Create `chronometrist-file' if it doesn't already exist."
  (unless (file-exists-p chronometrist-file)
    (with-current-buffer (find-file-noselect chronometrist-file)
      (write-file chronometrist-file))))

(defun chronometrist-common-file-empty-p (file)
  "Return t if FILE is empty."
  (let ((size (elt (file-attributes file) 7)))
    (if (zerop size) t nil)))

(defun chronometrist-common-clear-buffer (buffer)
  "Clear the contents of BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (delete-region (point-min) (point-max))))

(defun chronometrist-date-op-internal (seconds minutes hours day month year operator count)
  "Helper function for `chronometrist-date-op'."
  (declare (obsolete nil "Chronometrist v0.3.0"))
  (-->
   (encode-time seconds minutes hours day month year)
   (funcall (cond ((equal operator '+) 'time-add)
                  ((equal operator '-) 'time-subtract)
                  (t (error "Unknown operator %s" operator)))
            it
            (list 0 (* 86400 count)))
   (decode-time it)))

(defun chronometrist-format-keybinds (command map &optional firstonly)
  "Return the keybindings for COMMAND in MAP as a string.
If FIRSTONLY is non-nil, return only the first keybinding found."
  (if firstonly
      (key-description
       (where-is-internal command map firstonly))
    (->> (where-is-internal command map)
         (mapcar #'key-description)
         (-take 2)
         (-interpose ", ")
         (apply #'concat))))

(defvar chronometrist--fs-watch nil
  "Filesystem watch object.

Used to prevent more than one watch being added for the same
file.")

(defun chronometrist-events->time-list (events)
  "Convert EVENTS to a list of time values.

EVENTS must be a list of valid Chronometrist property lists (see
`chronometrist-file').

For each event, a list of two time values is returned.

For time value format, see (info \"(elisp)Time of Day\")."
  (let ((index 0)
        (length (length events))
        result)
    (while (not (= index length))
      (let* ((elt       (elt events index))
             (start-iso (parse-iso8601-time-string (plist-get elt :start)))
             (stop      (plist-get elt :stop))
             (stop-iso  (if stop
                            (parse-iso8601-time-string stop)
                          (current-time))))
        (cl-incf index)
        (setq result (append result `((,start-iso ,stop-iso))))))
    result))

(defun chronometrist-time-list->sum-of-intervals (time-value-lists)
  "From a list of start/end timestamps TIME-VALUES, get the total time interval.

TIME-VALUE-LISTS is a list in the form
\((START STOP) ...)
where START and STOP are time values (see (info \"(elisp)Time of Day\")).

This function obtains the intervals between them, and adds the
intervals to return a single time value.

If TIME-VALUES is nil, return '(0 0)."
  (if time-value-lists
      (->> time-value-lists
           (--map (time-subtract (cadr it) (car it)))
           (-reduce #'time-add))
    '(0 0)))

(defun chronometrist-delete-list (&optional arg)
  "Delete ARG lists after point."
  (let ((point-1 (point)))
    (forward-sexp (or arg 1))
    (delete-region point-1 (point))))

(defun chronometrist-previous-week-start (date-string)
  "Find the previous `chronometrist-report-week-start-day' from DATE-STRING.

Return the time value of said day's beginning.

If the day of DATE is the same as the
`chronometrist-report-week-start-day', return DATE.

DATE-STRING must be in the form \"YYYY-MM-DD\"."
  (let* ((date-time  (chronometrist-iso-date->timestamp date-string))
         (date-unix  (parse-iso8601-time-string date-time))
         (date-list  (decode-time date-unix))
         (day        (elt date-list 6)) ;; 0-6, where 0 = Sunday
         (week-start (chronometrist-day-of-week->number chronometrist-report-week-start-day))
         (gap        (cond ((> day week-start) (- day week-start))
                           ((< day week-start) (+ day (- 7 week-start))))))
    (if gap
        (time-subtract date-unix `(0 ,(* gap 86400)))
      date-unix)))

(defun chronometrist-current-task ()
  "Return the name of the currently clocked-in task, or nil if not clocked in."
  (let ((last-event (chronometrist-last-expr)))
    (if (plist-member last-event :stop)
        nil
      (plist-get last-event :name))))

;; Local Variables:
;; nameless-current-name: "chronometrist-common"
;; End:

(provide 'chronometrist-common)

;;; chronometrist-common.el ends here
