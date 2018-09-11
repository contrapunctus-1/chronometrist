(require 'timeclock)
(require 'dash)
(require 'cl-lib)

;; ## VARIABLES ##
(defvar timeclock-ui-empty-time-string "-")
(defvar timeclock-ui-date-re "[0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\}")
(defvar timeclock-ui-time-re-ui
  (rx-to-string
   `(or
     (and (optional (repeat 1 2 digit) ":")
          (and (repeat 1 2 digit) ":" (repeat 2 digit)))
     ,timeclock-ui-empty-time-string))
  "Regular expression to represent a timestamp in
`timeclock-list'. This is distinct from
`timeclock-ui-time-re-file' (which see) -
`timeclock-ui-time-re-ui' is meant for the user interface, and
must correspond to the output from `timeclock-ui-format-time'.")
(defvar timeclock-ui-time-re-file "[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}"
  "Regular expression to represent a timestamp in the file
`timeclock-file'. This is distinct from
`timeclock-ui-time-re-ui' (which see).")

(defun timeclock-ui-buffer-exists? (buffer-name)
  (--> (buffer-list)
       (mapcar #'buffer-name it)
       (member buffer-name it)))

(defun timeclock-ui-buffer-visible? (buffer-or-buffer-name)
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

(defun timeclock-ui-timestamp->list (date-time-string)
  "Convert a string timestamp to a list of integers."
  (--> date-time-string
       (split-string it "[-/ :]")
       (mapcar #'string-to-number it)))

(defun timeclock-ui-timestamp-list->seconds (date-time-list)
  "Convert DATE-TIME (which must be a list in the form (YEAR
MONTH DAY HOURS MINUTES SECONDS), as returned by
`timestamp->list') to seconds since the UNIX epoch
(see (info \"(elisp)Time of Day\"))."
  (->> date-time-list
       (reverse)
       (apply #'encode-time)))

(defun timeclock-ui-timestamp->seconds (date-time-string)
  "Convert a string timestamp in the form \"HH:MM:SS\" to seconds
since the UNIX epoch (see (info \"(elisp)Time of Day\"))."
  (timeclock-ui-timestamp-list->seconds
   (timeclock-ui-timestamp->list date-time-string)))

(defun timeclock-ui-time-interval-span-midnight? (t1 t2)
  "Return t if time range T1 to T2 extends across midnight.

T1 and T2 must be lists in the form (YEAR MONTH DAY HOURS MINUTES
SECONDS), as returned by `timestamp->list'. T2 must be
chronologically more recent than T1."
  (let* ((day-1          (elt t1 2))
         (day-2          (elt t2 2))
         (month-1        (elt t1 1))
         (month-2        (elt t2 1)))
    ;; not Absolutely Perfect™, but should do for most situations
    (or (= day-2   (1+ day-1))
        (= month-2 (1+ month-1)))))

(defun timeclock-ui-first-event-spans-midnight? (target-date project)
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
  (and (looking-at-p "^o ")
       ;; We have a midnight-spanning range. Is it a range for PROJECT?
       (progn
         (forward-line -1)
         (beginning-of-line)
         (looking-at-p (concat "i " timeclock-ui-date-re " " timeclock-ui-time-re-file " " project)))))

(defun timeclock-ui-get-end-time (target-date)
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
  (let* ((date-time        (if (progn
                                 (forward-line)
                                 (beginning-of-line)
                                 (looking-at-p "^o "))
                               (progn
                                 (re-search-forward "o ")
                                 (buffer-substring-no-properties (point)
                                                                 (+ 10 1 8 (point))))
                             (format-time-string "%Y/%m/%d %T")))
         (date-time-list   (timeclock-ui-timestamp->list date-time))
         (target-date-list (timeclock-ui-timestamp->list target-date)))
    (if (timeclock-ui-time-interval-span-midnight? target-date-list date-time-list)
        (concat target-date " 24:00:00")
      date-time)))

;; The multiple calls to re-search-forward/backward to get point at
;; the right spot are just ugly :\ (See if you can use match data
;; instead)
;;
;; Could be refactored - one function to get ranges for an activity,
;; one to convert them to seconds, one to subtract them (get an
;; interval from two timestamps), and one to output the time vector
;; from timeclock-list-seconds-to-hms in the desired format
;;
;; Better idea - get all events for target-date as a list of strings,
;; one per line. Operate on that. Will probably make for much nicer
;; code.

(defun timeclock-ui-project-time-one-day (project &optional date)
  "Read `timeclock-file' and return total time spent on a project
in one day. If DATE is a list containing calendrical
information (see (info \"(elisp)Time Conversion\")), the time for
that date is shown, otherwise calculate time for today.

The return value is a vector in the form [HOURS MINUTES SECONDS]"
  (if (not (member project timeclock-project-list))
      (error (concat "Unknown project: " project))
    (with-current-buffer (find-file-noselect timeclock-file)
      (save-excursion
        (let* ((target-date          (if date
                                         (format "%04d/%02d/%02d"
                                                 (elt date 5)
                                                 (elt date 4)
                                                 (elt date 3))
                                       (format-time-string "%Y/%m/%d")))
               (search-re            (concat target-date " " timeclock-ui-time-re-file " " project))
               (interval-list        nil)
               (first-event-midnight (timeclock-ui-first-event-spans-midnight? target-date
                                                                   project)))
          ;; (goto-char (point-min))
          (while (if first-event-midnight
                     (re-search-forward timeclock-ui-time-re-file nil t 2)
                   (re-search-forward (concat "i " search-re) nil t))
            (re-search-backward target-date nil t)
            (let* ((start-time         (if first-event-midnight
                                           (concat target-date " 00:00:00")
                                         (buffer-substring-no-properties
                                          (point)
                                          (+ 10 1 8 (point)))))
                   (end-time           (if first-event-midnight
                                           (buffer-substring-no-properties
                                            (point)
                                            (+ 10 1 8 (point)))
                                         (timeclock-ui-get-end-time target-date)))
                   (start-time-s       (timeclock-ui-timestamp->seconds start-time))
                   (end-time-s         (timeclock-ui-timestamp->seconds end-time))
                   (interval           (elt (time-subtract end-time-s
                                                           start-time-s)
                                            1)))
              (setq interval-list
                    (append interval-list (list interval))
                    ;; quick hack
                    first-event-midnight nil)))
          (->>
           interval-list
           (-reduce #'+)
           (timeclock-list-seconds-to-hms)))))))

;; tests -
;; (mapcar #'timeclock-ui-format-time
;;         '((0 0 0) (0 0 1) (0 0 10) (0 1 10) (0 10 10) (1 10 10) (10 10 10)))
;; => ("" "00:01" "00:10" "01:10" "10:10" "01:10:10" "10:10:10")
;; (mapcar #'timeclock-ui-format-time
;;         '([0 0 0] [0 0 1] [0 0 10] [0 1 10] [0 10 10] [1 10 10] [10 10 10]))
;; => ("" "00:01" "00:10" "01:10" "10:10" "01:10:10" "10:10:10")
(defun timeclock-ui-format-time (time)
  "Format and display TIME as a string, where TIME is a vector or
a list of the form [HOURS MINUTES SECONDS] or (HOURS MINUTES
SECONDS)."
  (let ((h (elt time 0))
        (m (elt time 1))
        (s (elt time 2)))
    (if (and (zerop h) (zerop m) (zerop s))
        "       -"
      (let ((h      (if (zerop h)
                        "   "
                      ;; Can't change this just yet or all commands break spectacularly.
                      ;; Maybe it's best this way too? Looks uniform.
                      ;; What if I pad with space?
                      (format "% 2d:" h)))
            (m      (cond ((and (zerop h) (zerop m))            "   ")
                          ((and (zerop h) (< m 10)) (format "% 2d:" m))
                          (t                        (format "%02d:" m))))
            (s      (format "%02d" s)))
        (concat h m s)))))

(defun timeclock-ui-open-timeclock-file ()
  (interactive)
  (find-file-other-window timeclock-file)
  (goto-char (point-max)))

(provide 'timeclock-ui-lib)

;; Local Variables:
;; nameless-current-name: "timeclock-ui"
;; End:
