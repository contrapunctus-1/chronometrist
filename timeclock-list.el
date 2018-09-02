(require 'timeclock)
(require 'dash)
(require 'cl-lib)
(require 'timeclock-report)

;; 2018-08-27T12:45:03+0530

;; TODO
;; 1. Refresh when you select the list buffer (impossible? make-thread
;;    in v26? Use emacs-async library?)
;; 2. Add support for prefix args to tclist/toggle-project
;; 3. Add variable to let user control prompting-for-reason behaviour
;; 4. Option to use a specific time to define when a day starts/ends.
;;    e.g. 08:00 will mean a day starts and ends at 08:00 instead of
;;    the usual 24:00/00:00. Helpful for late sleepers.
;; 5. Give each line a number - press the number to clock in/out
;;    - shortcuts derived from the first alphabet of each project
;;      could be even nicer, but the code to generate them from
;;      similarly-named projects would be somewhat complex
;; 6. Make clocked-in project row bold, either in addition to the
;;    star, or replacing it.
;; 7. Show currently active project + time spent on it so far in the
;;    mode-line (see timeclock-mode-line-display)
;; 8. The default reason suggested is the last one used. Can't even
;;    begin to explain how nonsensical that is. (might be an ido
;;    problem)
;; 9. Show shortcuts message by using the keymap rather than a
;;    hardcoded string.

;; BUGS
;; 1. (goto-char (point-max)) -> RET -> the time spent on the last
;;    project in the list will be the first new project suggestion.

;; Style issues
;; 1. Uses Scheme-style ? and x->y naming conventions instead of
;;    Elisp/CL-style "-p" and "x-to-y"
;;    - ido uses ? for 'completion help', so you can't type ? unless
;;      you unset that o\
;; 2. Should use *earmuffs* for global variables for clarity
;; 3. Should names of major modes (timeclock-list-mode,
;;    timeclock-report-mode) end with -major-mode ?

;; Limitations of timeclock.el
;; 1. Concurrent tasks not permitted
;; 2. timeclock-project-list contains only the projects found in the
;;    timeclock-file - no way for a user to specify tasks beforehand.
;; 3. Uses non-standard slashes in the date instead of dashes (e.g.
;;    "2018/01/01" instead of "2018-01-01") and a space for the
;;    date-time separator instead of T
;;
;; Limitations of tabulated-list-mode
;; 1. Can't mix tabulated and non-tabulated data!!! What if I want
;;    some buttons, separate from the data but part of the same
;;    buffer?!

;; ## IDLE TIMER ##
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
  (-->
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
   (if (car it) t nil)))

(defun tcl/timer-fn ()
  (when (and (tcl/buffer-exists? timeclock-list-buffer-name)
             (tcl/buffer-visible? timeclock-list-buffer-name))
    (with-current-buffer timeclock-list-buffer-name
      (tabulated-list-print t))))

;; ## VARIABLES ##
(defvar time-re "[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}")
(defvar empty-time-string "-")
(defvar time-re-list (rx-to-string
                      `(or
                        (and (optional (repeat 1 2 digit) ":")
                             (and (repeat 1 2 digit) ":" (repeat 2 digit)))
                        ,empty-time-string)))
(defvar timeclock-list-buffer-name "*Timeclock-List*")

;; ## FUNCTIONS ##

;; tests -
;; (mapcar #'tcr/format-time
;;         '((0 0 0) (0 0 1) (0 0 10) (0 1 10) (0 10 10) (1 10 10) (10 10 10)))
;; => ("" "00:01" "00:10" "01:10" "10:10" "01:10:10" "10:10:10")
;; (mapcar #'tcr/format-time
;;         '([0 0 0] [0 0 1] [0 0 10] [0 1 10] [0 10 10] [1 10 10] [10 10 10]))
;; => ("" "00:01" "00:10" "01:10" "10:10" "01:10:10" "10:10:10")
(defun tcl/format-time (time)
  "Formats and displays TIME, where time is a vector or a list of
the form [HOURS MINUTES SECONDS] or (HOURS MINUTES SECONDS)."
  (let ((h (elt time 0))
        (m (elt time 1))
        (s (elt time 2)))
    (if (and (zerop h) (zerop m) (zerop s))
        "-"
      (let ((h      (if (zerop h)
                        ""
                      (format "%02d:" h))) ;; can't change this just yet or all commands break
            (m      (format "%02d:" m))
            (s      (format "%02d" s)))
        (concat h m s)))))

(defun tcl/current-project ()
  "Returns the name of the currently clocked-in project, or nil
 if the user is not clocked in."
  (if (not (timeclock-currently-in-p))
      nil
    (with-current-buffer (find-file-noselect timeclock-file)
      (save-excursion
        (goto-char (point-max))
        (forward-line -1)
        (re-search-forward (concat time-re " ") nil t)
        (buffer-substring-no-properties (point) (point-at-eol))))))

(defun tcl/project-active? (project)
  "Returns t if PROJECT is currently clocked in, else nil."
  (equal (tcl/current-project) project))

(defun tcl/timestamp->seconds (date-time)
  "Converts a timestamp to seconds since 00:00"
  (--> date-time
       (split-string it "[/ :]")
       (mapcar #'string-to-number it)
       (reverse it)
       (apply #'encode-time it)))

;; tests -
;; (mapcar #'tcl/seconds-to-hms '(1 60 61 3600 3660 3661))
;; => ([0 0 1] [0 1 0] [0 1 1] [1 0 0] [1 1 0] [1 1 1])
(defun tcl/seconds-to-hms (seconds)
  (setq seconds (truncate seconds))
  (let* ((s (% seconds 60))
         (m (% (/ seconds 60) 60))
         (h (/ seconds 3600)))
    (vector h m s)))

;; The multiple calls to re-search-forward/backward to get point at
;; the right spot are just ugly :\
;;
;; Could be refactored - one function to get ranges for an activity,
;; one to convert them to seconds, one to subtract them (get an
;; interval from two timestamps), and one to output the time vector
;; from tcl/seconds-to-hms in the desired format
(defun tcl/project-time-one-day (project &optional date)
  "Read `timeclock-file' and return total time spent on a project
in one day. If DATE is a string in the form \"YYYY-MM-DD\", the
time for that date is shown, otherwise calculate time for that
day."
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
           (tcl/seconds-to-hms)
           (tcl/format-time)))))))

(defun tcl/entries ()
  "Creates entries to be displayed in the buffer created by
`timeclock-list'."
  (timeclock-reread-log)
  (->> timeclock-project-list
       (-sort #'string-lessp)
       (--map-indexed (list it
                            (vector (number-to-string (1+ it-index))
                                    it
                                    (tcl/project-time-one-day it)
                                    (if (tcl/project-active? it)
                                        "*" ""))))))

(defun tcl/project-at-point ()
  (save-excursion
    (beginning-of-line)
    (--> (buffer-substring-no-properties
          (re-search-forward "[0-9]+ +")
          (progn
            (re-search-forward time-re-list nil t)
            (match-beginning 0)))
         (replace-regexp-in-string "[ \t]*$" "" it))))

;; ## MAJOR-MODE ##
(define-derived-mode timeclock-list-mode tabulated-list-mode "Timeclock-List"
  "Major mode for `timeclock-list'."
  (timeclock-reread-log)

  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format [("#" 3 t)
                               ("Project" 25 t)
                               ("Time" 10 t)
                               ("Active" 3 t)])

  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'tcl/entries)

  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Project" . nil))

  (tabulated-list-init-header)

  (run-with-idle-timer 3 t #'tcl/timer-fn)
  (define-key timeclock-list-mode-map (kbd "RET") 'tcl/toggle-project)
  (define-key timeclock-list-mode-map (kbd "l") 'tcl/open-timeclock-file)
  (define-key timeclock-list-mode-map (kbd "r") 'timeclock-report))

;; ## COMMANDS ##

(defun tcl/toggle-project ()
  "In a `timeclock-list' buffer, start or stop the project at point."
  (interactive)
  (let ((project-at-point (tcl/project-at-point))
        (current-project  (tcl/current-project)))
    ;; When changing projects/clocking in, suggest the project at point
    (cl-letf (((symbol-function 'timeclock-ask-for-project)
               (lambda ()
                 (timeclock-completing-read
                  (format "Clock into which project (default %s): "
                          project-at-point)
                  (mapcar 'list timeclock-project-list)
                  project-at-point))))
      ;; If we're clocked in to anything - clock out or change projects
      (if current-project
          (if (equal project-at-point current-project)
              (timeclock-out nil nil t)
            ;; We don't use timeclock-change because it doesn't prompt for the reason
            (progn
              (timeclock-out nil nil t)
              (timeclock-in nil nil t)))
        ;; Otherwise, run timeclock-in with project at point as default
        ;; suggestion
        (timeclock-in nil nil t)))
    (timeclock-reread-log) ;; required when we create a new activity
    ;; Trying to update partially doesn't update the activity indicator. Why?
    (tabulated-list-print t nil)))

(defun tcl/open-timeclock-file ()
  (interactive)
  (find-file-other-window timeclock-file)
  (goto-char (point-max)))

(defun timeclock-list (&optional arg)
  "Displays a list of the user's timeclock.el projects and the
time spent on each today, based on their timelog file
`timeclock-file'. The user can hit RET to start/stop projects.
This is the 'listing command' for timeclock-list-mode."
  (interactive "P")
  (if arg
      (timeclock-report)
    (let ((buffer (get-buffer-create timeclock-list-buffer-name)))
      (if (tcl/buffer-visible? timeclock-list-buffer-name)
          (kill-buffer timeclock-list-buffer-name)
        (with-current-buffer buffer
          (timeclock-list-mode)
          (tabulated-list-print)

          ;; place point on the last or current project
          (goto-char (point-min))
          (re-search-forward timeclock-last-project nil t)
          (beginning-of-line)

          (switch-to-buffer buffer)
          (message "RET - clock in/out, r - see weekly report, l - open log file"))))))

(provide 'timeclock-list)
