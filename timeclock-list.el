(require 'timeclock)
(require 'dash)
(require 'cl-lib)

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
;;    the usual 24:00/00:00.
;; 6. Project weekly report mode
;; 7. If buffer already exists, don't open it in the other window
;;    (kill and recreate it?)
;;
;; BUGS
;; 1. RET -> create new project -> the idle timer will not update it
;;    until you re-create the buffer

;; Style issues
;; 1. Uses Scheme-style ? instead of Elisp/CL -p convention
;; 2. Should use *earmuffs* for global variables

;; Limitations of timeclock.el
;; 1. Concurrent tasks not permitted

(defun tclist/buffer-exists? (buffer-name)
  (--> (buffer-list)
       (mapcar #'buffer-name it)
       (member buffer-name it)))

(defun tclist/buffer-visible? (buffer-or-buffer-name)
  "Returns t if BUFFER-OR-BUFFER-NAME is visible to user."
  (-->
   ;; It'd be simpler to start with this, but because it is possible
   ;; that a frame partially covers another, and that frame has a
   ;; buffer visible to the user; thus, using only the current frame
   ;; isn't robust.
   ;; (selected-frame)
   ;; (window-list it)
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
   (if it t nil)))

(defun tclist/timer-fn ()
  (when (and (tclist/buffer-exists? timeclock-list-buffer-name)
             (tclist/buffer-visible? timeclock-list-buffer-name))
    (with-current-buffer timeclock-list-buffer-name
      (tabulated-list-print t t))))

(define-derived-mode timeclock-list-mode tabulated-list-mode "timeclock-list"
  "Display projects from timeclock.el and the time spent on each
  today."
  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format [("Project" 25 t) ("Time" 10 t) ("Active" 3 t)])

  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'tclist/entries)

  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Project" . nil))

  (tabulated-list-init-header)

  (run-with-idle-timer 3 t #'tclist/timer-fn)
  (define-key timeclock-list-mode-map (kbd "RET") 'tclist/toggle-project))

(defvar time-re "[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}")
(defvar timeclock-list-buffer-name "*Timeclock-List*")

(defun tclist/current-project ()
  "Returns the name of the currently clocked-in project, or nil
 if the user is not clocked in."
  (if (not (timeclock-currently-in-p))
      nil
    (with-current-buffer (find-file-noselect timeclock-file)
      (goto-char (point-max))
      (forward-line -1)
      (re-search-forward (concat time-re " ") nil t)
      (buffer-substring-no-properties (point) (point-at-eol)))))

(defun tclist/project-active? (project)
  "Returns t if PROJECT is currently clocked in, else nil."
  (equal (tclist/current-project) project))

(defun tclist/toggle-project ()
  "Start or stop the project at point."
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
    ;; If we're clocked in to anything (current-project non-nil) and
    ;; it's the project at point, clock out
    (if (equal project-at-point current-project)
        (timeclock-out nil nil t)
      ;; Otherwise, run timeclock-in with project at point as default
      ;; suggestion
      ;; (let ((timeclock-get-project-function #'tclist/ask-for-project))
      (cl-letf (((symbol-function 'timeclock-ask-for-project)
                 (lambda ()
                   (timeclock-completing-read
                    (format "Clock into which project (default %s): "
                            project-at-point)
                    (mapcar 'list timeclock-project-list)
                    project-at-point))))
        (timeclock-in nil nil t))
    ;; Trying to update partially doesn't update the activity
    ;; indicator. Why?
    (tabulated-list-print t nil))))

;; listing command
;; 1. show projects and time spent on them today
;; 2. hit enter to start/stop project
;; 3. update buffer when starting/stopping/idle/other events/possibly also on a
;;    timer.
(defun timeclock-list ()
  (interactive)
  (let ((buffer (get-buffer-create timeclock-list-buffer-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (timeclock-list-mode)
      (tabulated-list-print)
      (switch-to-buffer-other-window buffer))))

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
;; the right spot are so...inelegant :\
(defun tclist/project-time-one-day (project)
  (if (not (member project timeclock-project-list))
      (error (concat "Unknown project: " project))
    (let* ((current-date  (format-time-string "%Y/%m/%d"))
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
          (concat (format "%02d" time-h)
                  ":"
                  (format "%02d" time-m)
                  ":"
                  (format "%02d" time-s)))))))

(defun tclist/buffer-visible? (buffer-or-buffer-name)
  "Returns t if BUFFER-OR-BUFFER-NAME is visible to user."
  (-->
   ;; It'd be simpler to start with this, but because it is possible
   ;; that a frame partially covers another, and that frame has a
   ;; buffer visible to the user; thus, using only the current frame
   ;; isn't robust.
   ;; (selected-frame)
   ;; (window-list it)
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
   (if it t nil)))

(defun tclist/timer-fn ()
  (when (tclist/buffer-visible? timeclock-list-buffer-name)
    (with-current-buffer timeclock-list-buffer-name
      (tabulated-list-print t t))))

(run-with-idle-timer 3 t #'tclist/timer-fn)

(provide 'timeclock-list)
