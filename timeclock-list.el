(require 'timeclock)
(require 'dash)

;; 2018-08-27T12:45:03+0530
(define-derived-mode timeclock-list-mode tabulated-list-mode "Timeclock-List"
  "Display projects from timeclock.el and the time spent on each
  today."
  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format [("Project" 25 t) ("Time" 5 t)])

  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'tclist/entries)

  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Project" . nil))

  (tabulated-list-init-header))

(defun tclist/toggle-project ()
  "Start or stop the project at point."
  (interactive)
  (if (timeclock-currently-in-p)
      (if ()))
  ;; if we're clocked in to anything -
  ;; - if yes and it's at point, clock out
  ;; - else, error
  ;; else - start project at point
  ;;
)

;; listing command
;; 1. show projects and time spent on them today
;; 2. hit enter to start/stop project
;; 3. update buffer when starting/stopping/idle/other events/possibly also on a
;;    timer.
(defun timeclock-list ()
  (interactive)
  (let ((buffer (get-buffer-create "*Timeclock-List*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (timeclock-list-mode)
      (tabulated-list-print)
      (switch-to-buffer-other-window buffer))))

(defun tclist/entries ()
  (mapcar (lambda (item)
            (list nil
                  (vector item (tclist/project-time-one-day item))))
          timeclock-project-list))

(defun tclist/timestamp->seconds (date-time)
  "Converts a timestamp to seconds since 00:00"
  (--> date-time
       (split-string it "[/ :]")
       (mapcar #'string-to-number it)
       (reverse it)
       (apply #'encode-time it)))

;; tests -
;; (tclist/seconds-to-hms 1) => [0 0 1]
;; (tclist/seconds-to-hms 60) => [0 1 0]
;; (tclist/seconds-to-hms 61) => [0 1 1]
;; (tclist/seconds-to-hms 3600) => [1 0 0]
;; (tclist/seconds-to-hms 3660) => [1 1 0]
;; (tclist/seconds-to-hms 3661) => [1 1 1]
(defun tclist/seconds-to-hms (seconds)
  (let* ((hours   (if (>= seconds 3600)
                      (/ seconds 3600)
                    0))
         (minutes (if (>= (% seconds 3600)
                          60)
                      (/ (% seconds 3600) 60)
                    0))
         (seconds (if (or (> hours 0) (> minutes 0))
                      (% (% seconds 3600) 60)
                    seconds)))
    (vector hours minutes seconds)))

;; The multiple calls to re-search-forward/backward to get point at
;; the right spot are so...inelegant :\
(defun tclist/project-time-one-day (project)
  (if (not (member project timeclock-project-list))
      (error (concat "Unknown project: " project))
    (let* ((current-date  (format-time-string "%Y/%m/%d"))
           (time-re       "[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}")
           (search-re     (concat current-date " " time-re " " project))
           (interval-list nil))
      (with-current-buffer (find-file-noselect timeclock-file)
        (goto-char (point-min))
        (while (re-search-forward (concat "i " search-re) nil t)
          (re-search-backward current-date nil t)
          (let* ((start-time (buffer-substring-no-properties
                              (point)
                              (+ 10 1 8 (point))))
                 (end-time   (progn
                               (if (re-search-forward (concat "o " current-date) nil t)
                                   (buffer-substring-no-properties (- (point) 10)
                                                                   (+ 9 (point)))
                                 ;; if the user hasn't clocked out
                                 ;; from the project, the timelog does
                                 ;; not have an ending time yet, so we
                                 ;; use the current time
                                 (format-time-string "%Y/%m/%d %T"))))
                 (interval   (-->
                              (time-subtract (tclist/timestamp->seconds end-time)
                                             (tclist/timestamp->seconds start-time))
                              (elt it 1))))
            (setq interval-list
                  (append interval-list (list interval)))))
        (let* ((time-vector (->>
                             (seq-reduce #'+ interval-list 0)
                             (tclist/seconds-to-hms)))
               (time-h      (elt time-vector 0))
               (time-m      (elt time-vector 1))
               (time-s      (elt time-vector 2)))
          (concat (number-to-string time-h)
                  ":"
                  (format "%02d" time-m)
                  ":"
                  (format "%02d" time-s)))))))

(provide 'timeclock-list)
