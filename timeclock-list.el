(require 'timeclock)
(require 'dash)
(require 'cl-lib)
(require 'timeclock-report)

;; 2018-08-27T12:45:03+0530
;; has not yet been tested with comments in the timelog

;; TODO
;; 1. Refresh when you select the list buffer (impossible? make-thread
;;    in v26? Use emacs-async library?)
;; 2. Add support for prefix args to tclist/toggle-project
;; 3. Add variable to let user control prompting-for-reason behaviour
;; 4. Make "?" show help; show message saying "Press ? for help" in
;;    minibuffer when running M-x timeclock-list
;;    - Not really necessary, now that we only use one key to do
;;      everything?
;;    - Maybe only "Press RET to clock in/out, r to view a project's
;;      weekly report"?
;; 5. Option to use a specific time to define when a day starts/ends.
;;    e.g. 08:00 will mean a day starts and ends at 08:00 instead of
;;    the usual 24:00/00:00. Helpful for late sleepers.
;; 6. Project weekly report mode
;; 7. If buffer is already visible, calling timeclock-list again
;;    shouldn't open it in the other window (Kill and recreate it?
;;    Close it?)
;; 8. When stopped, put cursor on the last activity.
;;    - or better yet, give each line a number - press the number to
;;      clock in/out
;;    - shortcuts derived from the first alphabet of each project
;;      could be even nicer, but the code to generate them from
;;      similarly-named projects would be somewhat complex
;; 9. Make clocked-in project row bold, either in addition to the
;;    star, or replacing it.
;; 10. Show currently active project + time spent on it so far in the
;;     mode-line (see timeclock-mode-line-display)
;; 11. The default reason suggested is the last one used. Can't even
;;     begin to explain how nonsensical that is. (might be an ido
;;     problem)
;; 12. Replace tclist/ prefix with tl/

;; BUGS
;; 1. RET -> create new project -> the idle timer will not update it
;;    until you re-create the buffer
;; 2. (goto-char (point-max)) -> RET -> the time spent on the last
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

;; ## IDLE TIMER ##
(defun tclist/buffer-exists? (buffer-name)
  (--> (buffer-list)
       (mapcar #'buffer-name it)
       (member buffer-name it)))

(defun tclist/buffer-visible? (buffer-or-buffer-name)
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

(defun tclist/timer-fn ()
  (when (and (tclist/buffer-exists? timeclock-list-buffer-name)
             (tclist/buffer-visible? timeclock-list-buffer-name))
    (timeclock-reread-log)
    (with-current-buffer timeclock-list-buffer-name
      (tabulated-list-print t t))))

;; ## VARIABLES ##
(defvar time-re "[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}")
(defvar timeclock-list-buffer-name "*Timeclock-List*")

;; ## FUNCTIONS ##
(defun tclist/current-project ()
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

(defun tclist/project-active? (project)
  "Returns t if PROJECT is currently clocked in, else nil."
  (equal (tclist/current-project) project))

(defun tclist/timestamp->seconds (date-time)
  "Converts a timestamp to seconds since 00:00"
  (--> date-time
       (split-string it "[/ :]")
       (mapcar #'string-to-number it)
       (reverse it)
       (apply #'encode-time it)))

;; tests -
;; (mapcar #'tclist/seconds-to-hms '(1 60 61 3600 3660 3661))
;; => ([0 0 1] [0 1 0] [0 1 1] [1 0 0] [1 1 0] [1 1 1])
(defun tclist/seconds-to-hms (seconds)
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
;; from tclist/seconds-to-hms in the desired format
(defun tclist/project-time-one-day (project &optional date)
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
            (concat (format "%02d" time-h)
                    ":"
                    (format "%02d" time-m)
                    ":"
                    (format "%02d" time-s))))))))

(defun tclist/entries ()
  "Creates entries to be displayed in the buffer created by
`timeclock-list'."
  (mapcar (lambda (project-name)
            (list project-name
                  (vector project-name
                          (tclist/project-time-one-day project-name)
                          (if (tclist/project-active? project-name)
                              "*" ""))))
          timeclock-project-list))

;; ## MAJOR-MODE ##
(define-derived-mode timeclock-list-mode tabulated-list-mode "Timeclock-List"
  "Major mode for `timeclock-list'."
  (timeclock-reread-log)

  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format [("Project" 25 t) ("Time" 10 t) ("Active" 3 t)])

  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'tclist/entries)

  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Project" . nil))

  (tabulated-list-init-header)

  (run-with-idle-timer 3 t #'tclist/timer-fn)
  (define-key timeclock-list-mode-map (kbd "RET") 'tclist/toggle-project)
  (define-key timeclock-list-mode-map (kbd "l") 'tclist/open-timeclock-file))

;; ## COMMANDS ##

(defun tclist/toggle-project ()
  "In a `timeclock-list' buffer, start or stop the project at point."
  (interactive)
  (let ((project-at-point (progn
                            (beginning-of-line)
                            (--> (buffer-substring-no-properties
                                  (point)
                                  (progn
                                    (end-of-line)
                                    (re-search-backward time-re nil t)))
                                 (replace-regexp-in-string "[ \t]*$" "" it))))
        (current-project  (tclist/current-project)))
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

(defun tclist/open-timeclock-file ()
  (interactive)
  (find-file-other-window timeclock-file)
  (goto-char (point-max)))

(defun timeclock-list ()
  "Displays a list of the user's timeclock.el projects and the
time spent on each today, based on their timelog file
`timeclock-file'. The user can hit RET to start/stop projects.
This is the 'listing command' for timeclock-list-mode."
  (interactive)
  (let ((buffer (get-buffer-create timeclock-list-buffer-name)))
    ;; if we're in a timeclock-list buffer, kill it
    ;; FIXME - kill buffer if it's visible, too
    (if (and (equal (buffer-name) (buffer-name buffer))
             (equal major-mode 'timeclock-list-mode))
        (kill-this-buffer)
      ;; otherwise create the buffer
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (timeclock-list-mode)
        (tabulated-list-print)
        ;; using switch-to-buffer instead until we can preserve
        ;; position of point across successive calls to timeclock-list
        ;; (switch-to-buffer-other-window buffer)
        (switch-to-buffer buffer)))))

(provide 'timeclock-list)
