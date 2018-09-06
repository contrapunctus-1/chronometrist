(require 'timeclock)
(require 'dash)
(require 'cl-lib)

(defun tcl/buffer-exists? (buffer-name)
  (--> (buffer-list)
       (mapcar #'buffer-name it)
       (member buffer-name it)))

(defun tcl/buffer-visible? (buffer-or-buffer-name)
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

(defun tcl/timestamp->list (date-time-string)
  "Convert a string timestamp to a list of integers."
  (--> date-time-string
       (split-string it "[-/ :]")
       (mapcar #'string-to-number it)))

(defun tcl/timestamp-list->seconds (date-time-list)
  "Convert DATE-TIME (which must be a list in the form (YEAR
MONTH DAY HOURS MINUTES SECONDS), as returned by
`timestamp->list') to seconds since the UNIX epoch
(see (info \"(elisp)Time of Day\"))."
  (->> date-time-list
       (reverse)
       (apply #'encode-time)))

(defun tcl/timestamp->seconds (date-time-string)
  "Convert a string timestamp in the form \"HH:MM:SS\" to seconds
since the UNIX epoch (see (info \"(elisp)Time of Day\"))."
  (tcl/timestamp-list->seconds
   (tcl/timestamp->list date-time-string)))

(defun tcl/time-interval-span-midnight? (t1 t2)
  "Return t if time range T1 to T2 extends across midnight.

T1 and T2 must be lists in the form (YEAR MONTH DAY HOURS MINUTES
SECONDS), as returned by `timestamp->list'. T2 must be more
recent than T1 in chronological order."
  (let* ((day-1          (elt t1 2))
         (day-2          (elt t2 2))
         (month-1        (elt t1 1))
         (month-2        (elt t2 1)))
         ;; not Absolutely Perfect™, but should do for most situations
    (or (= day-2   (1+ day-1))
        (= month-2 (1+ month-1)))))

;; fixing the midnight bug (next day)
;; find first event for the day (clock-in _or_ out; no project involved)
;; if it's an out -
;; - get the previous event (clock-in)
;; - check if the project is the one we want
;;   - if it is, make the start time 00:00:00
;;   - if not, ignore it, go to the next (clock-in) event for the required project
(defun tcl/get-end-time (target-date)
  "Return the date and time of the next clock-out event after
point in the file `timeclock-file'.

If there is no clock-out event after point, return the current
date and time.

If the clock-out time is past midnight, return the date in
TARGET-DATE with the time at midnight (\"24:00:00\").

Return value is a string with the form \"YYYY/MM/DD HH:MM:SS\".

TARGET-DATE must be a date in the form \"YYYY/MM/DD\"

Point must be on a clock-in event having the same date as
TARGET-DATE."
  (let* ((date-time        (if (progn
                                 (forward-line)
                                 (beginning-of-line)
                                 (looking-at-p "^o "))
                               (progn
                                 (re-search-forward "o ")
                                 (buffer-substring-no-properties (point)
                                                                 (+ 10 1 8 (point))))
                             (format-time-string "%Y/%m/%d %T")))
         (date-time-list   (tcl/timestamp->list date-time))
         (target-date-list (tcl/timestamp->list target-date)))
    (if (tcl/time-interval-span-midnight? target-date-list date-time-list)
        (concat target-date " 24:00:00")
      date-time)))

;; The multiple calls to re-search-forward/backward to get point at
;; the right spot are just ugly :\ (See if you can use match data
;; instead)
;;
;; Could be refactored - one function to get ranges for an activity,
;; one to convert them to seconds, one to subtract them (get an
;; interval from two timestamps), and one to output the time vector
;; from tcl/seconds-to-hms in the desired format
(defun tcl/project-time-one-day (project &optional date)
  "Read `timeclock-file' and return total time spent on a project
in one day. If DATE is a string in the form \"YYYY-MM-DD\", the
time for that date is shown, otherwise calculate time for that
day.

The return value is a vector in the form [HOURS MINUTES SECONDS]"
  (if (not (member project timeclock-project-list))
      (error (concat "Unknown project: " project))
    (let* ((target-date   (if date
                              (replace-regexp-in-string "-" "/" date) ;; should probably validate it...
                            (format-time-string "%Y/%m/%d")))
           (search-re     (concat target-date " " time-re " " project))
           (interval-list nil))
      (with-current-buffer (find-file-noselect timeclock-file)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward (concat "i " search-re) nil t)
            (re-search-backward target-date nil t)
            (let* ((start-time         (buffer-substring-no-properties
                                        (point)
                                        (+ 10 1 8 (point))))
                   (end-time           (tcl/get-end-time target-date))
                   (start-time-s       (tcl/timestamp->seconds start-time))
                   (end-time-s         (tcl/timestamp->seconds end-time))
                   (interval           (elt (time-subtract end-time-s
                                                           start-time-s)
                                            1)))
              (setq interval-list
                    (append interval-list (list interval)))))
          (->>
           (-reduce #'+ interval-list)
           (tcl/seconds-to-hms)))))))

;; tests -
;; (mapcar #'tcl/format-time
;;         '((0 0 0) (0 0 1) (0 0 10) (0 1 10) (0 10 10) (1 10 10) (10 10 10)))
;; => ("" "00:01" "00:10" "01:10" "10:10" "01:10:10" "10:10:10")
;; (mapcar #'tcl/format-time
;;         '([0 0 0] [0 0 1] [0 0 10] [0 1 10] [0 10 10] [1 10 10] [10 10 10]))
;; => ("" "00:01" "00:10" "01:10" "10:10" "01:10:10" "10:10:10")
(defun tcl/format-time (time)
  "Format and display TIME as a string, where time is a vector or
a list of the form [HOURS MINUTES SECONDS] or (HOURS MINUTES
SECONDS)."
  (let ((h (elt time 0))
        (m (elt time 1))
        (s (elt time 2)))
    (if (and (zerop h) (zerop m) (zerop s))
        "-"
      (let ((h      (if (zerop h)
                        ""
                      ;; Can't change this just yet or all commands break spectacularly.
                      ;; Maybe it's best this way too? Looks uniform.
                      ;; What if I pad with space?
                      (format "%02d:" h)))
            (m      (format "%02d:" m))
            (s      (format "%02d" s)))
        (concat h m s)))))

(defun tcl/open-timeclock-file ()
  (interactive)
  (find-file-other-window timeclock-file)
  (goto-char (point-max)))

(provide 'timeclock-ui-lib)
