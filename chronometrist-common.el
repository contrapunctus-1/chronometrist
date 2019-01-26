(require 'timeclock)
(require 'dash)
(require 'cl-lib)

;; ## VARIABLES ##
(defvar chronometrist-empty-time-string "-")
(defvar chronometrist-date-re "[0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\}")
(defvar chronometrist-time-re-ui
  (rx-to-string
   `(or
     (and (repeat 0 2
                  (optional (repeat 1 2 digit) ":"))
          (repeat 1 2 digit))
     ,chronometrist-empty-time-string))
  "Regular expression to represent a timestamp in
`chronometrist'. This is distinct from
`chronometrist-time-re-file' (which see) -
`chronometrist-time-re-ui' is meant for the user interface, and
must correspond to the output from `chronometrist-format-time'.")
(defvar chronometrist-time-re-file "[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}"
  "Regular expression to represent a timestamp in the file
`timeclock-file'. This is distinct from
`chronometrist-time-re-ui' (which see).")

(defun chronometrist-buffer-exists? (buffer-name)
  (--> (buffer-list)
       (mapcar #'buffer-name it)
       (member buffer-name it)))

(defun chronometrist-buffer-visible? (buffer-or-buffer-name)
  "Returns t if BUFFER-OR-BUFFER-NAME is visible to user."
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

(defun chronometrist-timestamp->list (date-time-string)
  "Convert a string timestamp to a list of integers."
  (--> date-time-string
       (split-string it "[-/ :]")
       (mapcar #'string-to-number it)))

(defun chronometrist-timestamp-list->seconds (date-time-list)
  "Convert DATE-TIME (which must be a list in the form (YEAR
MONTH DAY HOURS MINUTES SECONDS), as returned by
`timestamp->list') to seconds since the UNIX epoch
(see (info \"(elisp)Time of Day\"))."
  (->> date-time-list
       (reverse)
       (apply #'encode-time)))

(defun chronometrist-timestamp->seconds (date-time-string)
  "Convert a string timestamp in the form \"YYYY/MM/SS HH:MM:SS\" to seconds
since the UNIX epoch (see (info \"(elisp)Time of Day\"))."
  (chronometrist-timestamp-list->seconds
   (chronometrist-timestamp->list date-time-string)))

(defun chronometrist-time-interval-span-midnight? (t1 t2)
  "Return t if time range T1 to T2 extends across midnight.

T1 and T2 must be lists in the form (YEAR MONTH DAY HOURS MINUTES
SECONDS), as returned by `timestamp->list'. T2 must be
chronologically more recent than T1."
  (let* ((day-1   (elt t1 2))
         (day-2   (elt t2 2))
         (month-1 (elt t1 1))
         (month-2 (elt t2 1)))
    ;; not Absolutely Perfect™, but should do for most situations
    (or (= day-2   (1+ day-1))
        (= month-2 (1+ month-1)))))

(defun chronometrist-first-event-spans-midnight? (target-date project)
  "Return t if the first event of PROJECT on TARGET-DATE in
`timeclock-file' spans midnight (i.e. its clock-in time was
before midnight), else nil.

TARGET-DATE must be a date in the form \"YYYY/MM/DD\", and
PROJECT a string."
  ;; Go to the first event for TARGET-DATE, ignoring the project
  (goto-char (point-min))
  (re-search-forward target-date nil t)
  (beginning-of-line)
  ;; Is it a clock-out event?
  (when (looking-at-p "^o ")
    ;; We have a midnight-spanning range. Is it a range for PROJECT?
    (forward-line -1)
    (beginning-of-line)
    (looking-at-p (concat "i " chronometrist-date-re " " chronometrist-time-re-file " " project))))

(defun chronometrist-get-end-time (target-date)
  "Return the date and time of the next clock-out event after
point in the file `timeclock-file'.

If there is no clock-out event after point, return the current
date and time.

If the clock-out time is past midnight, return the date in
TARGET-DATE with the time at midnight (\"24:00:00\").

Return value is a string in the form \"YYYY/MM/DD HH:MM:SS\"

TARGET-DATE must be a date in the form \"YYYY/MM/DD\"

Point must be on a clock-in event having the same date as
TARGET-DATE."
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

(defun chronometrist-project-time-one-day (project &optional date)
  "Read `timeclock-file' and return total time spent on PROJECT
today or (if supplied) on DATE.

DATE must be a list containing calendrical information (see (info
\"(elisp)Time Conversion\")).

The return value is a vector in the form [HOURS MINUTES SECONDS]"
  (let* ((target-date (case (length date)
                        (3 date)
                        (t (cl-destructuring-bind (_ _ _ day month year _ _ _)
                               (if date date (decode-time))
                             (list year month day))))))
    (->> (chronometrist-project-events-in-day project target-date)
         (chronometrist-events->time-list)
         (chronometrist-time-list->sum-of-intervals)
         (cadr)
         (chronometrist-seconds-to-hms))))

;; tests -
;; (mapcar #'chronometrist-format-time
;;         '((0 0 0) (0 0 1) (0 0 10) (0 1 10) (0 10 10) (1 10 10) (10 10 10)))
;; => ("" "00:01" "00:10" "01:10" "10:10" "01:10:10" "10:10:10")
;; (mapcar #'chronometrist-format-time
;;         '([0 0 0] [0 0 1] [0 0 10] [0 1 10] [0 10 10] [1 10 10] [10 10 10]))
;; => ("" "00:01" "00:10" "01:10" "10:10" "01:10:10" "10:10:10")
(defun chronometrist-format-time (time)
  "Format and display TIME as a string, where TIME is a vector or
a list of the form [HOURS MINUTES SECONDS] or (HOURS MINUTES
SECONDS)."
  (let ((h (elt time 0))
        (m (elt time 1))
        (s (elt time 2))
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

(defun chronometrist-open-timeclock-file (&optional button)
  (interactive)
  (find-file-other-window timeclock-file)
  (goto-char (point-max)))

(defun chronometrist-common-create-timeclock-file ()
  "Create `timeclock-file' if it doesn't already exist."
  (unless (file-exists-p timeclock-file)
    (with-current-buffer (find-file-noselect timeclock-file)
      (write-file timeclock-file))))

(defun chronometrist-common-file-empty-p (file)
  (let ((size (elt (file-attributes file) 7)))
    (if (zerop size) t nil)))

(defun chronometrist-common-clear-buffer (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (delete-region (point-min) (point-max))))

(defun chronometrist-date-op-internal (seconds minutes hours day month year operator count)
  "Helper function for `chronometrist-date-op'."
  (-->
   (encode-time seconds minutes hours day month year)
   (funcall (cond ((equal operator '+) 'time-add)
                  ((equal operator '-) 'time-subtract)
                  (t (error "Unknown operator %s" operator)))
            it
            (list 0 (* 86400 count)))
   (decode-time it)))

;; TODO - remove operator argument - use negative/positive integers
;; instead, and rename it to date-add
(defun chronometrist-date-op (date operator &optional count)
  "Return DATE incremented or decremented by COUNT days (1 if not
supplied).

DATE must be calendrical information (see (info \"(elisp)Time Conversion\"))

OPERATOR must be either '+ or '-

COUNT must be a positive integer."
  (let ((count (if count count 1)))
    (case (length date)
      (3 (cl-destructuring-bind (year month day)
             date
           (-> (chronometrist-date-op-internal 0 0 0
                                  day month year
                                  operator count)
               (chronometrist-calendrical->date))))
      (t (cl-destructuring-bind (s m h day month year _ _ _)
             date
           (chronometrist-date-op-internal s m h
                              day month year
                              operator count))))))

(defun chronometrist-time->seconds (time)
  "TIME must be a vector in the form [HOURS MINUTES SECONDS]."
  (-let [[h m s] time]
    (+ (* h 60 60)
       (* m 60)
       s)))

(provide 'chronometrist-common)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:
