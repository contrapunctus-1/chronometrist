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
                           (seq-filter #'identity list))
                         it)
                 (mapcar #'car it)
                 (car it))))
    (if result t nil)))

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
                              (replace-regexp-in-string "-" "/" date)    ;; should probably validate it...
                              (format-time-string "%Y/%m/%d")))
           (search-re     (concat target-date " " time-re " " project))
           (interval-list nil))
      (with-current-buffer (find-file-noselect timeclock-file)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward (concat "i " search-re) nil t)
            (re-search-backward target-date nil t)
            (let* ((start-time (buffer-substring-no-properties
                                (point)
                                (+ 10 1 8 (point))))
                   (end-time   (progn
                                 (if (re-search-forward (concat "o " target-date) nil t)
                                     (buffer-substring-no-properties (- (point) 10)
                                                                     (+ 9 (point)))
                                   ;; if the user hasn't clocked out
                                   ;; from the project, the timelog does
                                   ;; not have an ending time yet, so we
                                   ;; use the current time
                                   (format-time-string "%Y/%m/%d %T"))))
                   (interval   (-->
                                (time-subtract (tcl/timestamp->seconds end-time)
                                               (tcl/timestamp->seconds start-time))
                                (elt it 1))))
              (setq interval-list
                    (append interval-list (list interval)))))
          (->>
           (seq-reduce #'+ interval-list 0)
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
