(require 'chronometrist-lib)
(require 'chronometrist-custom)
(require 'chronometrist-report)

;; TODO - don't suggest nil when asking for first project on first run

;; TODO - use variables instead of hardcoded numbers to determine spacing

;; TODO - remove repetitive calls to (format "%04d-%02d-%02d" (elt seq a) (elt seq b) (elt seq c))

;; Maybe we should just make the projects into buttons (links),
;; instead of hacking the same functionality in `chronometrist-toggle-project' with
;; a possibly stranger UI (no visual hint that only projects will do
;; something when clicked/hit RET on)

;; BUGS

;; 1. Start a project before midnight -> after midnight, chronometrist
;;    will display it as active, but the time spent will be '-' (zero)
;;    _until you clock out._ Probably a `chronometrist-project-time-one-day' bug.

;; 2. timer function makes line highlight vanish

;; 3. (goto-char (point-max)) -> RET -> the time spent on the last
;;    project in the list will be the first new project suggestion.

;; 4. Create (and start) a _new_ project -> kill buffer -> run
;;    chronometrist -> cursor is not at the new project
;;    - can't reproduce it?

;; Style issues
;; 1. Uses Scheme-style ? and x->y naming conventions instead of
;;    Elisp/CL-style "-p" and "x-to-y"
;;    - ido uses ? for 'completion help', so you can't type ? unless
;;      you unset that o\
;; 2. Should use *earmuffs* for global variables for clarity
;; 3. Should names of major modes (chronometrist-mode,
;;    chronometrist-report-mode) end with -major-mode ?

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
(defvar chronometrist--timer-object nil)

;; ## TIMER ##
(defun chronometrist-timer ()
  (when (and (chronometrist-buffer-exists? chronometrist-buffer-name)
             (chronometrist-buffer-visible? chronometrist-buffer-name))
    ;; (message "chronometrist-idle-timer run at %s" (format-time-string "%T"))
    (with-current-buffer chronometrist-buffer-name
      (let ((position (point)))
        (tabulated-list-print t)
        (chronometrist-print-non-tabular)
        (goto-char position)))))

(defun chronometrist-maybe-start-timer ()
  (unless chronometrist--timer-object
    (setq chronometrist--timer-object
          (run-at-time t chronometrist-update-interval #'chronometrist-timer))))

(defun chronometrist-change-update-interval (arg)
  (interactive "NEnter new interval (in seconds): ")
  (cancel-timer chronometrist--timer-object)
  (setq chronometrist-update-interval arg
        chronometrist--timer-object nil)
  (chronometrist-maybe-start-timer))

;; ## FUNCTIONS ##
(defun chronometrist-current-project ()
  "Return the name of the currently clocked-in project, or nil if
 the user is not clocked in."
  (if (not (timeclock-currently-in-p))
      nil
    (with-current-buffer (find-file-noselect timeclock-file)
      (save-excursion
        (goto-char (point-max))
        (forward-line -1)
        (re-search-forward (concat chronometrist-time-re-file " ") nil t)
        (buffer-substring-no-properties (point) (point-at-eol))))))

(defun chronometrist-project-active? (project)
  "Return t if PROJECT is currently clocked in, else nil."
  (equal (chronometrist-current-project) project))

(defun chronometrist-seconds-to-hms (seconds)
  "Convert SECONDS to a vector in the form [HOURS MINUTES
SECONDS]. SECONDS must be a positive integer."
  (setq seconds (truncate seconds))
  (let* ((s (% seconds 60))
         (m (% (/ seconds 60) 60))
         (h (/ seconds 3600)))
    (vector h m s)))

(defun chronometrist-entries ()
  "Create entries to be displayed in the buffer created by
`chronometrist'."
  (timeclock-reread-log)
  (->> timeclock-project-list
       (-sort #'string-lessp)
       (--map-indexed
        (list it
              (vector (number-to-string (1+ it-index))
                      (list it
                            'action 'chronometrist-toggle-project-button
                            'follow-link t)
                      (-> (chronometrist-project-time-one-day it)
                          (chronometrist-format-time))
                      (if (chronometrist-project-active? it)
                          "*" ""))))))

(defun chronometrist-project-at-point ()
  "Return the project at point in the `chronometrist' buffer, or
nil if there is no project at point."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "[0-9]+ +" nil t)
        (--> (buffer-substring-no-properties
              (point)
              (progn
                (re-search-forward chronometrist-time-re-ui nil t)
                (match-beginning 0)))
             (replace-regexp-in-string "[ \t]*$" "" it))
      nil)))

(defun chronometrist-goto-last-project ()
  (goto-char (point-min))
  (re-search-forward timeclock-last-project nil t)
  (beginning-of-line))

(defun chronometrist-time-add (a b)
  "Add two vectors in the form [HOURS MINUTES SECONDS] and
return a vector in the same form."
  (let ((h1 (elt a 0))
        (m1 (elt a 1))
        (s1 (elt a 2))
        (h2 (elt b 0))
        (m2 (elt b 1))
        (s2 (elt b 2)))
    (chronometrist-seconds-to-hms (+ (* h1 3600) (* h2 3600)
                        (* m1 60) (* m2 60)
                        s1 s2))))

(defun chronometrist-total-time-one-day (&optional date)
  "Return the total time clocked on DATE (if non-nil) or
 today, as a vector in the form [HOURS MINUTES SECONDS].

DATE must be calendrical information calendrical
information (see (info \"(elisp)Time Conversion\"))."
  (->> timeclock-project-list
       (--map (chronometrist-project-time-one-day it date))
       (-reduce #'chronometrist-time-add)))

(defun chronometrist-print-non-tabular ()
  "Print the non-tabular part of the buffer in `chronometrist'."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (-->
     (chronometrist-total-time-one-day)
     (chronometrist-format-time it)
     (format "\n    %- 26s%s" "Total" it)
     (concat it
             "\n\n    RET or [mouse-1] - clock in/out"
             "\n    <numeric argument N> RET - clock in/out from <N>th project"
             "\n    r - see weekly report"
             "\n    l - open log file")
     (insert it))))

(defun chronometrist-get-nth-project (n)
  "Return the Nth project in a `chronometrist' buffer, or nil if
there is no corresponding project."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (format "^%d" n) nil t)
        (chronometrist-project-at-point)
      nil)))

;; ## MAJOR-MODE ##
(define-derived-mode chronometrist-mode tabulated-list-mode "Chronometrist"
  "Major mode for `chronometrist'."
  (timeclock-reread-log)

  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format [("#"       3  t)
                               ("Project" 25 t)
                               ("Time"    10 t)
                               ("Active"  3  t)])

  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'chronometrist-entries)

  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Project" . nil))

  (tabulated-list-init-header)

  (define-key chronometrist-mode-map (kbd "RET") #'chronometrist-toggle-project)
  (define-key chronometrist-mode-map (kbd "l")   #'chronometrist-open-timeclock-file)
  (define-key chronometrist-mode-map (kbd "r")   #'chronometrist-report)
  (define-key chronometrist-mode-map [mouse-1]   #'chronometrist-toggle-project))

;; ## COMMANDS ##

;; Duplication between this function and `chronometrist-toggle-project's logic
(defun chronometrist-toggle-project-button (button)
  (let ((current-project  (chronometrist-current-project))
        (project-at-point (chronometrist-project-at-point)))
    ;; If we're clocked in to anything - clock out or change projects
    ;; Otherwise, just clock in
    (if current-project
        (if (equal project-at-point current-project)
            (timeclock-out nil nil t)
          ;; We don't use timeclock-change because it doesn't prompt for the reason
          (progn
            (timeclock-out nil nil t)
            (timeclock-in  nil project-at-point nil)))
      (timeclock-in nil project-at-point nil))
    ;; Trying to update partially doesn't update the activity indicator. Why?
    (tabulated-list-print t nil)
    (chronometrist-print-non-tabular)
    (chronometrist-goto-last-project)
    (chronometrist-maybe-start-timer)))

(defun chronometrist-toggle-project (&optional arg)
  "In a `chronometrist' buffer, start or stop the project at
point. If there is no project at point, do nothing.

With a numeric prefix argument, toggle the Nth project. If there
is no corresponding project, do nothing."
  (interactive "P")
  (let* ((target-project   (when arg (chronometrist-get-nth-project arg)))
         (project-at-point (chronometrist-project-at-point))
         (suggested-project (or target-project project-at-point)))
    (cond ((chronometrist-common-file-empty-p timeclock-file)
           (timeclock-in nil nil t))
          ;; What should we do if the user provides an invalid argument? Currently - nothing.
          ((and arg (not target-project)))
          (suggested-project ;; do nothing if there's no project at point
           (let ((current-project (chronometrist-current-project)))
             ;; We redefine this function so it suggests the project at point
             (cl-letf (((symbol-function 'timeclock-ask-for-project)
                        (lambda ()
                          (timeclock-completing-read
                           (format "Clock into which project (default %s): "
                                   suggested-project)
                           (mapcar 'list timeclock-project-list)
                           suggested-project))))
               ;; If we're clocked in to anything - clock out or change projects
               (if current-project
                   (if (equal suggested-project current-project)
                       (timeclock-out nil nil t)
                     ;; We don't use timeclock-change because it doesn't prompt for the reason
                     (progn
                       (timeclock-out nil nil t)
                       (timeclock-in nil nil t)))
                 ;; Otherwise, run timeclock-in with project at point as default suggestion
                 (timeclock-in nil nil t))
               (timeclock-reread-log) ;; required when we create a new activity
               ;; Trying to update partially doesn't update the activity indicator. Why?
               (tabulated-list-print t nil)
               (chronometrist-print-non-tabular)
               (chronometrist-goto-last-project)
               (chronometrist-maybe-start-timer)))))))

(defun chronometrist (&optional arg)
  "Displays a list of the user's timeclock.el projects and the
time spent on each today, based on their timelog file
`timeclock-file'. The user can hit RET to start/stop projects.
This is the 'listing command' for chronometrist-mode."
  (interactive "P")
  (if arg
      (chronometrist-report)
    (let ((buffer (get-buffer-create chronometrist-buffer-name)))
      (if (chronometrist-buffer-visible? chronometrist-buffer-name)
          (kill-buffer chronometrist-buffer-name)
        (with-current-buffer buffer
          (if (or (not (file-exists-p timeclock-file))
                  (chronometrist-common-file-empty-p timeclock-file))
              (progn
                (chronometrist-common-create-timeclock-file)
                (let ((inhibit-read-only t))
                  (chronometrist-common-clear-buffer buffer)
                  (insert "Welcome to Chronometrist! Hit RET to create a new task and start logging time.")
                  (chronometrist-mode)
                  (switch-to-buffer buffer)))
            (progn
              (chronometrist-mode)
              (tabulated-list-print)
              (when chronometrist-hide-cursor
                (make-local-variable 'cursor-type)
                (setq cursor-type nil)
                (hl-line-mode))
              (switch-to-buffer buffer)
              (chronometrist-print-non-tabular)
              (chronometrist-goto-last-project)
              (chronometrist-maybe-start-timer))))))))

(provide 'chronometrist)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:
