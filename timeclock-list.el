(require 'timeclock-ui-lib)

;; 2018-08-27T12:45:03+0530

;; BUGS
;; 1. (goto-char (point-max)) -> RET -> the time spent on the last
;;    project in the list will be the first new project suggestion.
;; 2. Start a project before midnight -> after midnight,
;;    timeclock-list will display it as active, but the time spent will
;;    be '-' (zero)
;; 3. Create (and start) a _new_ project -> kill buffer -> run
;;    timeclock-list -> cursor is not at the new project
;;    - can't reproduce it?

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

;; Limitations of tabulated-list-mode
;; 1. Can't mix tabulated and non-tabulated data!!! What if I want
;;    some buttons, separate from the data but part of the same
;;    buffer?!
;;    - adding non-tabular data after calling `tabulated-list-print' -
;;      as we do - works, but is hacky and doesn't always print (e.g.
;;      it vanishes when you sort). Then, you have to ensure you call
;;      it after each time you call `tabulated-list-print' :\
;;    - a post-print hook could help
;;    - maybe use advice?
;; 2. Can't have multi-line headers

;; ## VARIABLES ##
(defvar timeclock-list-buffer-name "*Timeclock-List*")
(defvar timeclock-list-hide-cursor nil
  "If non-nil, hide the cursor and only highlight the current
line in the `timeclock-list' buffer.")

;; ## IDLE TIMER ##
(defun timeclock-list-idle-timer ()
  (when (and (timeclock-ui-buffer-exists? timeclock-list-buffer-name)
             (timeclock-ui-buffer-visible? timeclock-list-buffer-name))
    ;; (message "timeclock-list-idle-timer run at %s" (format-time-string "%T"))
    (with-current-buffer timeclock-list-buffer-name
      (tabulated-list-print t)
      (timeclock-list-print-non-tabular)
      (timeclock-list-goto-last-project))))

;; ## FUNCTIONS ##
(defun timeclock-list-current-project ()
  "Return the name of the currently clocked-in project, or nil if
 the user is not clocked in."
  (if (not (timeclock-currently-in-p))
      nil
    (with-current-buffer (find-file-noselect timeclock-file)
      (save-excursion
        (goto-char (point-max))
        (forward-line -1)
        (re-search-forward (concat timeclock-ui-time-re-file " ") nil t)
        (buffer-substring-no-properties (point) (point-at-eol))))))

(defun timeclock-list-project-active? (project)
  "Return t if PROJECT is currently clocked in, else nil."
  (equal (timeclock-list-current-project) project))

;; tests -
;; (mapcar #'timeclock-list-seconds-to-hms '(1 60 61 3600 3660 3661))
;; => ([0 0 1] [0 1 0] [0 1 1] [1 0 0] [1 1 0] [1 1 1])
(defun timeclock-list-seconds-to-hms (seconds)
  (setq seconds (truncate seconds))
  (let* ((s (% seconds 60))
         (m (% (/ seconds 60) 60))
         (h (/ seconds 3600)))
    (vector h m s)))

(defun timeclock-list-entries ()
  "Create entries to be displayed in the buffer created by
`timeclock-list'."
  (timeclock-reread-log)
  (->> timeclock-project-list
       (-sort #'string-lessp)
       (--map-indexed (list it
                            (vector (number-to-string (1+ it-index))
                                    it
                                    (-> (timeclock-ui-project-time-one-day it)
                                        (timeclock-ui-format-time))
                                    (if (timeclock-list-project-active? it)
                                        "*" ""))))))

(defun timeclock-list-project-at-point ()
  "Get the project at point in the `timeclock-list' buffer."
  (save-excursion
    (beginning-of-line)
    (--> (buffer-substring-no-properties
          (re-search-forward "[0-9]+ +")
          (progn
            (re-search-forward timeclock-ui-time-re-ui nil t)
            (match-beginning 0)))
         (replace-regexp-in-string "[ \t]*$" "" it))))

(defun timeclock-list-goto-last-project ()
  (goto-char (point-min))
  (re-search-forward timeclock-last-project nil t)
  (beginning-of-line))

(defun timeclock-list-time-add (a b)
  "Add two vectors in the form [HOURS MINUTES SECONDS] and
return a vector in the same form."
  (let ((h1 (elt a 0))
        (h2 (elt b 0))
        (m1 (elt a 1))
        (m2 (elt b 1))
        (s1 (elt a 2))
        (s2 (elt b 2)))
    (timeclock-list-seconds-to-hms (+ (* h1 3600) (* h2 3600)
                        (* m1 60) (* m2 60)
                        s1 s2))))

(defun timeclock-list-total-time-one-day (&optional date)
  "Calculate the total time clocked today, or on DATE if non-nil."
  (->>
   timeclock-project-list
   (--map (timeclock-ui-project-time-one-day it date))
   (-reduce #'timeclock-list-time-add)))

(defun timeclock-list-print-non-tabular ()
  "Print the non-tabular part of the buffer in `timeclock-list'."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (-->
     (timeclock-list-total-time-one-day)
     (timeclock-ui-format-time it)
     (format "\n    %- 26s%s" "Total" it)
     (concat it
             "\n\n    RET - clock in/out"
             "\n    <numeric argument N> RET - clock in/out from <N>th project"
             "\n    r - see weekly report"
             "\n    l - open log file")
     (insert it))))

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
  (setq tabulated-list-entries 'timeclock-list-entries)

  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Project" . nil))

  (tabulated-list-init-header)

  (run-with-idle-timer 3 t #'timeclock-list-idle-timer)
  (define-key timeclock-list-mode-map (kbd "RET") 'timeclock-list-toggle-project)
  (define-key timeclock-list-mode-map (kbd "l") 'timeclock-ui-open-timeclock-file)
  (define-key timeclock-list-mode-map (kbd "r") 'timeclock-report))

;; ## COMMANDS ##

(defun timeclock-list-toggle-project (&optional arg)
  "In a `timeclock-list' buffer, start or stop the project at point."
  (interactive "P")
  (let ((target-project (progn
                          (when arg
                            (goto-char (point-min))
                            (re-search-forward (format "^%d" arg) nil t))
                          (timeclock-list-project-at-point)))
        (current-project  (timeclock-list-current-project)))
    ;; We change this function so it suggests the project at point
    (cl-letf (((symbol-function 'timeclock-ask-for-project)
               (lambda ()
                 (timeclock-completing-read
                  (format "Clock into which project (default %s): "
                          target-project)
                  (mapcar 'list timeclock-project-list)
                  target-project))))
      ;; If we're clocked in to anything - clock out or change projects
      (if current-project
          (if (equal target-project current-project)
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
    (tabulated-list-print t nil)
    (timeclock-list-print-non-tabular)
    (timeclock-list-goto-last-project)))

(defun timeclock-list (&optional arg)
  "Displays a list of the user's timeclock.el projects and the
time spent on each today, based on their timelog file
`timeclock-file'. The user can hit RET to start/stop projects.
This is the 'listing command' for timeclock-list-mode."
  (interactive "P")
  (if arg
      (timeclock-report)
    (let ((buffer (get-buffer-create timeclock-list-buffer-name)))
      (if (timeclock-ui-buffer-visible? timeclock-list-buffer-name)
          (kill-buffer timeclock-list-buffer-name)
        (with-current-buffer buffer
          (timeclock-list-mode)
          (tabulated-list-print)

          (when timeclock-list-hide-cursor
            (make-local-variable 'cursor-type)
            (setq cursor-type nil)
            (hl-line-mode))
          (switch-to-buffer buffer)
          (timeclock-list-print-non-tabular)
          (timeclock-list-goto-last-project))))))

(provide 'timeclock-list)
