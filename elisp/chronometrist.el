;;; chronometrist.el --- A time tracker with a nice interface -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Keywords: calendar
;; Homepage: https://github.com/contrapunctus-1/chronometrist
;; Package-Requires: ((emacs "25.1") (dash "2.16.0") (seq "2.20") (ts "0.2"))
;; Version: 0.6.5

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; For more information, please refer to <https://unlicense.org>

;;; Commentary:
;;
;; A time tracker in Emacs with a nice interface

;; Largely modelled after the Android application, [A Time Tracker](https://github.com/netmackan/ATimeTracker)

;; * Benefits
;;   1. Extremely simple and efficient to use
;;   2. Displays useful information about your time usage
;;   3. Support for both mouse and keyboard
;;   4. Human errors in tracking are easily fixed by editing a plain text file
;;   5. Hooks to let you perform arbitrary actions when starting/stopping tasks

;; * Limitations
;;   1. No support (yet) for adding a task without clocking into it.
;;   2. No support for concurrent tasks.

;; ## Comparisons
;; ### timeclock.el
;; Compared to timeclock.el, Chronometrist
;; * stores data in an s-expression format rather than a line-based one
;; * supports attaching tags and arbitrary key-values to time intervals
;; * has commands to shows useful summaries
;; * has more hooks

;; ### Org time tracking
;; Chronometrist and Org time tracking seem to be equivalent in terms of capabilities, approaching the same ends through different means.
;; * Chronometrist doesn't have a mode line indicator at the moment. (planned)
;; * Chronometrist doesn't have Org's sophisticated querying facilities. (an SQLite backend is planned)
;; * Org does so many things that keybindings seem to necessarily get longer. Chronometrist has far fewer commands than Org, so most of the keybindings are single keys, without modifiers.
;; * Chronometrist's UI makes keybindings discoverable - they are displayed in the buffers themselves.
;; * Chronometrist's UI is cleaner, since the storage is separate from the display. It doesn't show tasks as trees like Org, but it uses tags and key-values to achieve that. Additionally, navigating a flat list takes fewer user operations than navigating a tree.
;; * Chronometrist data is just s-expressions (plists), and may be easier to parse than a complex text format with numerous use-cases.

;; For information on usage and customization, see https://github.com/contrapunctus-1/chronometrist/blob/master/README.md

;;; Code:
(require 'dash)
(require 'ts)

(require 'cl-lib)
(require 'seq)
(require 'filenotify)
(require 'subr-x)
(require 'parse-time)

(eval-when-compile
  (defvar chronometrist-mode-map)
  (require 'subr-x))

(defcustom chronometrist-sexp-pretty-print-function #'chronometrist-plist-pp
  "Function used to pretty print plists in `chronometrist-file'.
Like `pp', it must accept an OBJECT and optionally a
STREAM (which is the value of `current-buffer')."
  :type 'function
  :group 'chronometrist)

(define-derived-mode chronometrist-sexp-mode
  ;; fundamental-mode
  emacs-lisp-mode
  "chronometrist-sexp")

(defmacro chronometrist-sexp-in-file (file &rest body)
  "Run BODY in a buffer visiting FILE, restoring point afterwards."
  (declare (indent defun) (debug t))
  `(with-current-buffer (find-file-noselect ,file)
     (save-excursion ,@body)))

(defmacro chronometrist-loop-file (for expr in file &rest loop-clauses)
  "`cl-loop' LOOP-CLAUSES over s-expressions in FILE, in reverse.
VAR is bound to each s-expression."
  (declare (indent defun)
           (debug nil)
           ;; FIXME
           ;; (debug ("for" form "in" form &rest &or sexp form))
           )
  `(chronometrist-sexp-in-file ,file
     (goto-char (point-max))
     (cl-loop with ,expr
       while (and (not (bobp))
                  (backward-list)
                  (or (not (bobp))
                      (not (looking-at-p "^[[:blank:]]*;")))
                  (setq ,expr (ignore-errors (read (current-buffer))))
                  (backward-list))
       ,@loop-clauses)))

(defun chronometrist-sexp-open-log ()
  "Open `chronometrist-file' in another window."
  (find-file-other-window chronometrist-file)
  (goto-char (point-max)))

(defun chronometrist-sexp-last ()
  "Return last s-expression from `chronometrist-file'."
  (chronometrist-sexp-in-file chronometrist-file
    (goto-char (point-max))
    (backward-list)
    (ignore-errors (read (current-buffer)))))

(defun chronometrist-sexp-current-task ()
  "Return the name of the currently clocked-in task, or nil if not clocked in."
  (let ((last-event (chronometrist-sexp-last)))
    (if (plist-member last-event :stop)
        nil
      (plist-get last-event :name))))

(defun chronometrist-sexp-events-populate ()
  "Populate hash table `chronometrist-events'.
The data is acquired from `chronometrist-file'.

Return final number of events read from file, or nil if there
were none."
  (chronometrist-sexp-in-file chronometrist-file
    (goto-char (point-min))
    (let ((index 0) expr pending-expr)
      (while (or pending-expr
                 (setq expr (ignore-errors (read (current-buffer)))))
        ;; find and split midnight-spanning events during deserialization itself
        (let* ((split-expr (chronometrist-events-maybe-split expr))
               (new-value  (cond (pending-expr
                                  (prog1 pending-expr
                                    (setq pending-expr nil)))
                                 (split-expr
                                  (setq pending-expr (cl-second split-expr))
                                  (cl-first split-expr))
                                 (t expr)))
               (new-value-date (--> (plist-get new-value :start)
                                    (substring it 0 10)))
               (existing-value (gethash new-value-date chronometrist-events)))
          (unless pending-expr (cl-incf index))
          (puthash new-value-date
                   (if existing-value
                       (append existing-value
                               (list new-value))
                     (list new-value))
                   chronometrist-events)))
      (unless (zerop index) index))))

(defun chronometrist-sexp-create-file ()
  "Create `chronometrist-file' if it doesn't already exist."
  (unless (file-exists-p chronometrist-file)
    (with-current-buffer (find-file-noselect chronometrist-file)
      (goto-char (point-min))
      (insert ";;; -*- mode: chronometrist-sexp; -*-")
      (write-file chronometrist-file))))

(cl-defun chronometrist-sexp-new (plist)
  "Add new PLIST at the end of `chronometrist-file'."
  (chronometrist-sexp-in-file chronometrist-file
    (goto-char (point-max))
    ;; If we're adding the first s-exp in the file, don't add a
    ;; newline before it
    (unless (bobp) (insert "\n"))
    (unless (bolp) (insert "\n"))
    (funcall chronometrist-sexp-pretty-print-function plist (current-buffer))
    (save-buffer)))

(defun chronometrist-sexp-delete-list (&optional arg)
  "Delete ARG lists after point."
  (let ((point-1 (point)))
    (forward-sexp (or arg 1))
    (delete-region point-1 (point))))

(defun chronometrist-sexp-replace-last (plist)
  "Replace the last s-expression in `chronometrist-file' with PLIST."
  (chronometrist-sexp-in-file chronometrist-file
    (goto-char (point-max))
    (unless (and (bobp) (bolp)) (insert "\n"))
    (backward-list 1)
    (chronometrist-sexp-delete-list)
    (funcall chronometrist-sexp-pretty-print-function plist (current-buffer))
    (save-buffer)))

(defun chronometrist-sexp-reindent-buffer ()
  "Reindent the current buffer.
This is meant to be run in `chronometrist-file' when using the s-expression backend."
  (interactive)
  (let (expr)
    (goto-char (point-min))
    (while (setq expr (ignore-errors (read (current-buffer))))
      (backward-list)
      (chronometrist-sexp-delete-list)
      (when (looking-at "\n*")
        (delete-region (match-beginning 0) (match-end 0)))
      (funcall chronometrist-sexp-pretty-print-function expr (current-buffer))
      (insert "\n")
      (unless (eobp) (insert "\n")))))

(defun chronometrist-last ()
  "Return the last entry from `chronometrist-file' as a plist."
  (chronometrist-sexp-last))

(defun chronometrist-task-list ()
  "Return a list of tasks from `chronometrist-file'."
  (--> (chronometrist-loop-file for plist in chronometrist-file collect (plist-get plist :name))
       (cl-remove-duplicates it :test #'equal)
       (sort it #'string-lessp)))


(defvar chronometrist--file-state nil
  "List containing the state of `chronometrist-file'.
`chronometrist-refresh-file' sets this to a plist in the form

\(:last (LAST-START LAST-END) :rest (REST-START REST-END HASH))

\(see `chronometrist-file-hash')

LAST-START and LAST-END represent the start and the end of the
last s-expression.

REST-START and REST-END represent the start of the file and the
end of the second-last s-expression.")

(defun chronometrist-file-hash (&optional start end hash)
  "Calculate hash of `chronometrist-file' between START and END.
START can be
a number or marker,
:before-last - the position at the start of the last s-expression
nil or any other value - the value of `point-min'.

END can be
a number or marker,
:before-last - the position at the end of the second-last s-expression,
nil or any other value - the position at the end of the last s-expression.

Return (START END) if HASH is nil, else (START END HASH).

Return a list in the form (A B HASH), where A and B are markers
in `chronometrist-file' describing the region for which HASH was calculated."
  (chronometrist-sexp-in-file chronometrist-file
    (let* ((start (cond ((number-or-marker-p start) start)
                        ((eq :before-last start)
                         (goto-char (point-max))
                         (backward-list))
                        (t (point-min))))
           (end   (cond ((number-or-marker-p end) end)
                        ((eq :before-last end)
                         (goto-char (point-max))
                         (backward-list 2)
                         (forward-list))
                        (t (goto-char (point-max))
                           (backward-list)
                           (forward-list)))))
      (if hash
          (--> (buffer-substring-no-properties start end)
               (secure-hash 'sha1 it)
               (list start end it))
        (list start end)))))


(defun chronometrist-read-from (position)
  (chronometrist-sexp-in-file chronometrist-file
    (goto-char (if (number-or-marker-p position)
                   position
                 (funcall position)))
    (ignore-errors (read (current-buffer)))))

(defun chronometrist-file-change-type (state)
  "Determine the type of change made to `chronometrist-file'.
STATE must be a plist. (see `chronometrist--file-state')

Return
:append  if a new s-expression was added to the end,
:modify  if the last s-expression was modified,
:remove  if the last s-expression was removed,
    nil  if the contents didn't change, and
      t  for any other change."
  (-let*
      (((last-start last-end)           (plist-get state :last))
       ((rest-start rest-end rest-hash) (plist-get state :rest))
       (last-expr-file  (chronometrist-read-from last-start))
       (last-expr-ht    (chronometrist-events-last))
       (last-same-p     (equal last-expr-ht last-expr-file))
       (file-new-length (chronometrist-sexp-in-file chronometrist-file (point-max)))
       (rest-same-p     (unless (< file-new-length rest-end)
                          (--> (chronometrist-file-hash rest-start rest-end t)
                            (cl-third it)
                            (equal rest-hash it)))))
    ;; (message "chronometrist - last-start\nlast-expr-file - %S\nlast-expr-ht - %S"
    ;;          last-expr-file
    ;;          last-expr-ht)
    ;; (message "chronometrist - last-same-p - %S, rest-same-p - %S"
    ;;          last-same-p rest-same-p)
    (cond ((not rest-same-p) t)
          (last-same-p
           (when (chronometrist-read-from last-end) :append))
          ((not (chronometrist-read-from last-start))
           :remove)
          ((not (chronometrist-read-from
                 (lambda ()
                   (progn (goto-char last-start)
                          (forward-list)))))
           :modify))))


(defun chronometrist-plist-pp-normalize-whitespace ()
  "Remove whitespace following point, and insert a space.
Point is placed at the end of the space."
  (when (looking-at "[[:blank:]]+")
    (delete-region (match-beginning 0) (match-end 0))
    (insert " ")))

(defun chronometrist-plist-pp-column ()
  "Return column point is on, as an integer.
0 means point is at the beginning of the line."
  (- (point) (point-at-bol)))

(defun chronometrist-plist-pp-pair-p (cons)
  "Return non-nil if CONS is a pair, i.e. (CAR . CDR)."
  (and (listp cons) (not (listp (cdr cons)))))

(defun chronometrist-plist-pp-alist-p (list)
  "Return non-nil if LIST is an association list.
If even a single element of LIST is a pure cons cell (as
determined by `chronometrist-plist-pp-pair-p'), this function
considers it an alist."
  (when (listp list)
    (cl-loop for elt in list thereis (chronometrist-plist-pp-pair-p elt))))

(defun chronometrist-plist-pp-plist-p (list)
  "Return non-nil if LIST is a property list, i.e. (:KEYWORD VALUE ...)"
  (while (consp list)
    (setq list (if (and (keywordp (car list))
                        (consp (cdr list)))
                   (cddr list)
                 'not-plist)))
  (null list))

(defun chronometrist-plist-pp-longest-keyword-length ()
  "Find the length of the longest keyword in a plist.
This assumes there is a single plist in the current buffer, and
that point is after the first opening parenthesis."
  (save-excursion
    (cl-loop with sexp
      while (setq sexp (ignore-errors (read (current-buffer))))
      when (keywordp sexp)
      maximize (length (symbol-name sexp)))))

(cl-defun chronometrist-plist-pp-indent-sexp (sexp &optional (right-indent 0))
  "Return a string indenting SEXP by RIGHT-INDENT spaces."
  (format (concat "% -" (number-to-string right-indent) "s")
          sexp))

(cl-defun chronometrist-plist-pp-buffer (&optional inside-sublist-p)
  "Recursively indent the alist, plist, or a list of plists after point.
The list must be on a single line, as emitted by `prin1'."
  (if (not (looking-at-p (rx (or ")" line-end))))
      (progn
        (setq sexp (save-excursion (read (current-buffer))))
        (cond
         ((chronometrist-plist-pp-plist-p sexp)
          (chronometrist-plist-pp-buffer-plist inside-sublist-p)
          (chronometrist-plist-pp-buffer inside-sublist-p))
         ((chronometrist-plist-pp-alist-p sexp)
          (chronometrist-plist-pp-buffer-alist)
          (unless inside-sublist-p (chronometrist-plist-pp-buffer)))
         ((chronometrist-plist-pp-pair-p sexp)
          (forward-sexp)
          (chronometrist-plist-pp-buffer inside-sublist-p))
         ((listp sexp)
          (down-list)
          (chronometrist-plist-pp-buffer t))
         (t (forward-sexp)
            (chronometrist-plist-pp-buffer inside-sublist-p))))
    ;; we're before a ) - is it a lone paren on its own line?
    (let ((pos (point))
          (bol (point-at-bol)))
      (goto-char bol)
      (if (string-match "^[[:blank:]]*$" (buffer-substring bol pos))
          ;; join the ) to the previous line by deleting the newline and whitespace
          (delete-region (1- bol) pos)
        (goto-char pos))
      (when (not (eobp))
        (forward-char)))))

(defun chronometrist-plist-pp-buffer-plist (&optional inside-sublist-p)
  "Indent a single plist after point."
  (down-list)
  (let ((left-indent  (1- (chronometrist-plist-pp-column)))
        (right-indent (chronometrist-plist-pp-longest-keyword-length))
        (first-p t) sexp)
    (while (not (looking-at-p ")"))
      (chronometrist-plist-pp-normalize-whitespace)
      (setq sexp (save-excursion (read (current-buffer))))
      (cond ((keywordp sexp)
             (chronometrist-sexp-delete-list)
             (insert (if first-p
                         (progn (setq first-p nil) "")
                       (make-string left-indent ?\ ))
                     (chronometrist-plist-pp-indent-sexp sexp right-indent)))
            ;; not a keyword = a value
            ((chronometrist-plist-pp-plist-p sexp)
             (chronometrist-plist-pp-buffer-plist))
            ((and (listp sexp)
                  (not (chronometrist-plist-pp-pair-p sexp)))
             (chronometrist-plist-pp-buffer t)
             (insert "\n"))
            (t (forward-sexp)
               (insert "\n"))))
    (when (bolp) (delete-char -1))
    (up-list)
    ;; we have exited the plist, but might still be in a list with more plists
    (unless (eolp) (insert "\n"))
    (when inside-sublist-p
      (insert (make-string (1- left-indent) ?\ )))))

(defun chronometrist-plist-pp-buffer-alist ()
  "Indent a single alist after point."
  (down-list)
  (let ((indent (chronometrist-plist-pp-column)) (first-p t) sexp)
    (while (not (looking-at-p ")"))
      (setq sexp (save-excursion (read (current-buffer))))
      (chronometrist-sexp-delete-list)
      (insert (if first-p
                  (progn (setq first-p nil) "")
                (make-string indent ?\ ))
              (format "%S\n" sexp)))
    (when (bolp) (delete-char -1))
    (up-list)))

(defun chronometrist-plist-pp-to-string (object)
  "Convert OBJECT to a pretty-printed string."
  (with-temp-buffer
    (lisp-mode-variables nil)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (let ((print-quoted t))
      (prin1 object (current-buffer)))
    (goto-char (point-min))
    (chronometrist-plist-pp-buffer)
    (buffer-string)))

(defun chronometrist-plist-pp (object &optional stream)
  "Pretty-print OBJECT and output to STREAM (see `princ')."
  (princ (chronometrist-plist-pp-to-string object)
         (or stream standard-output)))

(defvar chronometrist-migrate-table (make-hash-table))

(defun chronometrist-migrate-populate (in-file)
  "Read data from IN-FILE to `chronometrist-migrate-table'.
IN-FILE should be a file in the format supported by timeclock.el.
See `timeclock-log-data' for a description."
  (clrhash chronometrist-migrate-table)
  (with-current-buffer (find-file-noselect in-file)
    (save-excursion
      (goto-char (point-min))
      (let ((key-counter 0))
        (while (not (eobp))
          (let* ((event-string (buffer-substring-no-properties (point-at-bol)
                                                               (point-at-eol)))
                 (event-list   (split-string event-string "[ /:]"))
                 (code         (cl-first event-list))
                 (date-time    (--> event-list
                                    (seq-drop it 1)
                                    (seq-take it 6)
                                    (mapcar #'string-to-number it)
                                    (reverse it)
                                    (apply #'encode-time it)
                                    (chronometrist-format-time-iso8601 it)))
                 (project-or-comment
                  (replace-regexp-in-string
                   (rx (and (or "i" "o") " "
                            (and (= 4 digit) "/" (= 2 digit) "/" (= 2 digit) " ")
                            (and (= 2 digit) ":" (= 2 digit) ":" (= 2 digit))
                            (opt " ")))
                   ""
                   event-string)))
            (pcase code
              ("i"
               (cl-incf key-counter)
               (puthash key-counter
                        `(:name ,project-or-comment :start ,date-time)
                        chronometrist-migrate-table))
              ("o"
               (--> (gethash key-counter chronometrist-migrate-table)
                    (append it
                            `(:stop ,date-time)
                            (when (and (stringp project-or-comment)
                                       (not
                                        (string= project-or-comment "")))
                              `(:comment ,project-or-comment)))
                    (puthash key-counter it chronometrist-migrate-table)))))
          (forward-line)
          (goto-char (point-at-bol))))
      nil)))

(defvar timeclock-file)

(defun chronometrist-migrate-timelog-file-to-sexp-file (&optional in-file out-file)
  "Migrate your existing `timeclock-file' to the Chronometrist file format.
IN-FILE and OUT-FILE, if provided, are used as input and output
file names respectively."
  (interactive `(,(if (featurep 'timeclock)
                      (read-file-name (concat "timeclock file (default: "
                                              timeclock-file
                                              "): ")
                                      user-emacs-directory
                                      timeclock-file t)
                    (read-file-name (concat "timeclock file: ")
                                    user-emacs-directory
                                    nil t))
                 ,(read-file-name (concat "Output file (default: "
                                          (locate-user-emacs-file "chronometrist.sexp")
                                          "): ")
                                  user-emacs-directory
                                  (locate-user-emacs-file "chronometrist.sexp"))))
  (when (if (file-exists-p out-file)
            (yes-or-no-p (concat "Output file "
                                 out-file
                                 " already exists - overwrite? "))
          t)
    (let ((output (find-file-noselect out-file)))
      (with-current-buffer output
        (chronometrist-common-clear-buffer output)
        (chronometrist-migrate-populate in-file)
        (maphash (lambda (_key value)
                   (chronometrist-plist-pp value output)
                   (insert "\n\n"))
                 chronometrist-migrate-table)
        (save-buffer)))))

(defun chronometrist-migrate-check ()
  "Offer to import data from `timeclock-file' if `chronometrist-file' does not exist."
  (when (and (bound-and-true-p timeclock-file)
             (not (file-exists-p chronometrist-file)))
    (if (yes-or-no-p (format (concat "Chronometrist v0.3+ uses a new file format;"
                                     " import data from %s ? ")
                             timeclock-file))
        (chronometrist-migrate-timelog-file-to-sexp-file timeclock-file chronometrist-file)
      (message "You can migrate later using `chronometrist-migrate-timelog-file-to-sexp-file'."))))

(defun chronometrist-reset ()
  "Reset Chronometrist's internal state."
  (interactive)
  (chronometrist-reset-task-list)
  (chronometrist-events-populate)
  (setq chronometrist--file-state nil)
  (chronometrist-refresh))

(defvar chronometrist-events (make-hash-table :test #'equal)
  "Each key is a date in the form (YEAR MONTH DAY).
Values are lists containing events, where each event is a list in
the form (:name \"NAME\" :tags (TAGS) <key value pairs> ...
:start TIME :stop TIME).")

(defun chronometrist-apply-time (time timestamp)
  "Return TIMESTAMP with time modified to TIME.
TIME must be a string in the form \"HH:MM:SS\"

TIMESTAMP must be a time string in the ISO-8601 format.

Return value is a ts struct (see `ts.el')."
  (-let [(h m s) (mapcar #'string-to-number (split-string time ":"))]
    (ts-apply :hour h :minute m :second s
              (chronometrist-iso-timestamp-to-ts timestamp))))


(defun chronometrist-events-maybe-split (event)
  "Split EVENT if it spans midnight.
Return a list of two events if EVENT was split, else nil."
  (when (plist-get event :stop)
    (let ((split-time (chronometrist-midnight-spanning-p (plist-get event :start)
                                             (plist-get event :stop)
                                             chronometrist-day-start-time)))
      (when split-time
        (let ((first-start  (plist-get (cl-first  split-time) :start))
              (first-stop   (plist-get (cl-first  split-time) :stop))
              (second-start (plist-get (cl-second split-time) :start))
              (second-stop  (plist-get (cl-second split-time) :stop))
              ;; plist-put modifies lists in-place. The resulting bugs
              ;; left me puzzled for a while.
              (event-1      (cl-copy-list event))
              (event-2      (cl-copy-list event)))
          (list (-> event-1
                    (plist-put :start first-start)
                    (plist-put :stop  first-stop))
                (-> event-2
                    (plist-put :start second-start)
                    (plist-put :stop  second-stop))))))))


(defun chronometrist-events-populate ()
  "Clear hash table `chronometrist-events' (which see) and populate it.
The data is acquired from `chronometrist-file'.

Return final number of events read from file, or nil if there
were none."
  (clrhash chronometrist-events)
  (chronometrist-sexp-events-populate))

(defun chronometrist-events-update (plist &optional replace)
  "Add PLIST to the end of `chronometrist-events'.
If REPLACE is non-nil, replace the last event with PLIST."
  (let* ((date (->> (plist-get plist :start)
                    (chronometrist-iso-timestamp-to-ts )
                    (ts-format "%F" )))
         (events-today (gethash date chronometrist-events)))
    (--> (if replace (-drop-last 1 events-today) events-today)
         (append it (list plist))
         (puthash date it chronometrist-events))))

(defun chronometrist-events-last-date ()
  (--> (hash-table-keys chronometrist-events)
       (last it)
       (car it)))

(defun chronometrist-events-last ()
  "Return the last plist from `chronometrist-events'."
  (--> (gethash (chronometrist-events-last-date) chronometrist-events)
       (last it)
       (car it)))

(defun chronometrist-events-subset (start end)
  "Return a subset of `chronometrist-events'.
The subset will contain values between dates START and END (both
inclusive).

START and END must be ts structs (see `ts.el'). They will be
treated as though their time is 00:00:00."
  (let ((subset (make-hash-table :test #'equal))
        (start  (chronometrist-date start))
        (end    (chronometrist-date end)))
    (maphash (lambda (key value)
               (when (ts-in start end (chronometrist-iso-date-to-ts key))
                 (puthash key value subset)))
             chronometrist-events)
    subset))

(cl-defun chronometrist-task-events-in-day (task &optional (ts (ts-now)))
  "Get events for TASK on TS.
TS should be a ts struct (see `ts.el').

Returns a list of events, where each event is a property list in
the form (:name \"NAME\" :start START :stop STOP ...), where
START and STOP are ISO-8601 time strings.

This will not return correct results if TABLE contains records
which span midnights."
  (->> (gethash (ts-format "%F" ts) chronometrist-events)
       (mapcar (lambda (event)
                 (when (equal task (plist-get event :name))
                   event)))
       (seq-filter #'identity)))

(cl-defun chronometrist-task-time-one-day (task &optional (ts (ts-now)))
  "Return total time spent on TASK today or (if supplied) on timestamp TS.
The data is obtained from `chronometrist-file', via `chronometrist-events'.

TS should be a ts struct (see `ts.el').

The return value is seconds, as an integer."
  (let ((task-events (chronometrist-task-events-in-day task ts)))
    (if task-events
        (->> (chronometrist-events-to-durations task-events)
             (-reduce #'+)
             (truncate))
      ;; no events for this task on TS, i.e. no time spent
      0)))

(cl-defun chronometrist-active-time-one-day (&optional (ts (ts-now)))
  "Return the total active time on TS (if non-nil) or today.
TS must be a ts struct (see `ts.el')

Return value is seconds as an integer."
  (->> (--map (chronometrist-task-time-one-day it ts) chronometrist-task-list)
       (-reduce #'+)
       (truncate)))

(cl-defun chronometrist-statistics-count-active-days (task &optional (table chronometrist-events))
  "Return the number of days the user spent any time on TASK.
  TABLE must be a hash table - if not supplied, `chronometrist-events' is used.

  This will not return correct results if TABLE contains records
which span midnights."
  (cl-loop for events being the hash-values of table
    count (seq-find (lambda (event)
                      (equal task (plist-get event :name)))
                    events)))

(defvar chronometrist-task-list nil
  "List of tasks in `chronometrist-file'.")

(defun chronometrist-reset-task-list ()
  (setq chronometrist-task-list (chronometrist-task-list)))

(defun chronometrist-add-to-task-list (task)
  (unless (cl-member task chronometrist-task-list :test #'equal)
    (setq chronometrist-task-list
          (sort (cons task chronometrist-task-list) #'string-lessp))))

(defun chronometrist-remove-from-task-list (task)
  (let ((count (cl-loop with count = 0
                 for intervals being the hash-values of chronometrist-events
                 do (cl-loop for interval in intervals
                      do (cl-incf count))
                 finally return count))
        (position (cl-loop with count = 0
                    for intervals being the hash-values of chronometrist-events
                    when (cl-loop for interval in intervals
                           do (cl-incf count)
                           when (equal task (plist-get interval :name))
                           return t)
                    return count)))
    (when (or (not position)
              (= position count))
      ;; The only interval for TASK is the last expression
      (setq chronometrist-task-list (remove task chronometrist-task-list)))))

(defvar chronometrist--fs-watch nil
  "Filesystem watch object.
Used to prevent more than one watch being added for the same
file.")

(defun chronometrist-current-task ()
  "Return the name of the currently clocked-in task, or nil if not clocked in."
  (chronometrist-sexp-current-task))

(cl-defun chronometrist-format-time (seconds &optional (blank "   "))
  "Format SECONDS as a string suitable for display in Chronometrist buffers.
SECONDS must be a positive integer.

BLANK is a string to display in place of blank values. If not
supplied, 3 spaces are used."
  (-let [(h m s) (chronometrist-seconds-to-hms seconds)]
    (if (and (zerop h) (zerop m) (zerop s))
        "       -"
      (let ((h (if (zerop h) blank (format "%2d:" h)))
            (m (cond ((and (zerop h) (zerop m))  blank)
                     ((zerop h)  (format "%2d:" m))
                     (t  (format "%02d:" m))))
            (s (if (and (zerop h) (zerop m))
                   (format "%2d" s)
                 (format "%02d" s))))
        (concat h m s)))))

(defun chronometrist-common-file-empty-p (file)
  "Return t if FILE is empty."
  (zerop (nth 7 (file-attributes file))))

(defun chronometrist-common-clear-buffer (buffer)
  "Clear the contents of BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (delete-region (point-min) (point-max))))

(defun chronometrist-format-keybinds (command map &optional firstonly)
  "Return the keybindings for COMMAND in MAP as a string.
If FIRSTONLY is non-nil, return only the first keybinding found."
  (if firstonly
      (key-description
       (where-is-internal command map firstonly))
    (->> (where-is-internal command map)
         (mapcar #'key-description)
         (-take 2)
         (-interpose ", ")
         (apply #'concat))))

(defun chronometrist-events-to-durations (events)
  "Convert EVENTS into a list of durations in seconds.
EVENTS must be a list of valid Chronometrist property lists (see
`chronometrist-file').

Return 0 if EVENTS is nil."
  (if events
      (cl-loop for plist in events collect
        (let* ((start-ts (chronometrist-iso-timestamp-to-ts
                          (plist-get plist :start)))
               (stop-iso (plist-get plist :stop))
               ;; Add a stop time if it does not exist.
               (stop-ts  (if stop-iso
                             (chronometrist-iso-timestamp-to-ts stop-iso)
                           (ts-now))))
          (ts-diff stop-ts start-ts)))
    0))

(defun chronometrist-previous-week-start (ts)
  "Find the previous `chronometrist-report-week-start-day' from TS.
Return a ts struct for said day's beginning.

If the day of TS is the same as the
`chronometrist-report-week-start-day', return TS.

TS must be a ts struct (see `ts.el')."
  (cl-loop with week-start = (alist-get chronometrist-report-week-start-day
                                        chronometrist-report-weekday-number-alist
                                        nil nil #'equal)
    until (= week-start (ts-dow ts))
    do (ts-decf (ts-day ts))
    finally return ts))

(defun chronometrist-iso-timestamp-to-ts (timestamp)
  "Convert TIMESTAMP to a TS struct. (see `ts.el')
TIMESTAMP must be the ISO-8601 format, as handled by `parse-iso8601-time-string'."
  (-let [(second minute hour day month year dow _dst utcoff)
         (decode-time
          (parse-iso8601-time-string timestamp))]
    (ts-update
     (make-ts :hour hour :minute minute :second second
              :day day   :month month   :year year
              :dow dow   :tz-offset utcoff))))

(defun chronometrist-iso-date-to-ts (date)
  "Return a ts struct (see `ts.el') representing DATE.
DATE should be an ISO-8601 date string (\"YYYY-MM-DD\")."
  (-let [(year month day) (mapcar #'string-to-number
                                  (split-string date "-"))]
    (ts-update
     (make-ts :hour 0 :minute 0 :second 0
              :day day :month month :year year))))

(cl-defun chronometrist-date (&optional (ts (ts-now)))
  "Return a ts struct representing the time 00:00:00 on today's date.
If TS is supplied, use that date instead of today.
TS should be a ts struct (see `ts.el')."
  (ts-apply :hour 0 :minute 0 :second 0 ts))

(defun chronometrist-format-time-iso8601 (&optional unix-time)
  "Return current date and time as an ISO-8601 timestamp.
Optional argument UNIX-TIME should be a time value (see
`current-time') accepted by `format-time-string'."
  (format-time-string "%FT%T%z" unix-time))

;; Note - this assumes that an event never crosses >1 day. This seems
;; sufficient for all conceivable cases.

(defun chronometrist-midnight-spanning-p (start-time stop-time day-start-time)
  "Return non-nil if START-TIME and STOP-TIME cross a midnight.
START-TIME and STOP-TIME must be ISO-8601 timestamps.

DAY-START-TIME must be a string in the form \"HH:MM:SS\" (see
`chronometrist-day-start-time')

Return a list in the form
\((:start START-TIME
  :stop <day-start time on initial day>)
 (:start <day start time on second day>
  :stop STOP-TIME))"
  ;; FIXME - time zones are ignored; may cause issues with
  ;; time-zone-spanning events

  ;; The time on which the first provided day starts (according to `chronometrist-day-start-time')
  (let* ((start-ts        (chronometrist-iso-timestamp-to-ts start-time))
         (stop-ts         (chronometrist-iso-timestamp-to-ts stop-time))
         (first-day-start (chronometrist-apply-time day-start-time start-time))
         (next-day-start  (ts-adjust 'hour 24 first-day-start)))
    ;; Does the event stop time exceed the next day start time?
    (when (ts< next-day-start stop-ts)
      (list `(:start ,start-time
                     :stop  ,(ts-format "%FT%T%z" next-day-start))
            `(:start ,(ts-format "%FT%T%z" next-day-start)
                     :stop  ,stop-time)))))


(defun chronometrist-seconds-to-hms (seconds)
  "Convert SECONDS to a vector in the form [HOURS MINUTES SECONDS].
SECONDS must be a positive integer."
  (let* ((seconds (truncate seconds))
         (s       (% seconds 60))
         (m       (% (/ seconds 60) 60))
         (h       (/ seconds 3600)))
    (list h m s)))

(defun chronometrist-interval (event)
  "Return the period of time covered by EVENT as a time value.
EVENT should be a plist (see `chronometrist-file')."
  (let ((start (plist-get event :start))
        (stop  (plist-get event :stop)))
    (time-subtract (parse-iso8601-time-string stop)
                   (parse-iso8601-time-string start))))

(defvar chronometrist--timer-object nil)

(defcustom chronometrist-timer-hook nil
  "Functions run by `chronometrist-timer'.")

(defun chronometrist-timer ()
  "Refresh Chronometrist and related buffers.
Buffers will be refreshed only if they are visible and the user
is clocked in to a task."
  (when (chronometrist-current-task)
    (when (get-buffer-window chronometrist-buffer-name)
      (chronometrist-refresh))
    (run-hooks 'chronometrist-timer-hook)))

(defun chronometrist-stop-timer ()
  "Stop the timer for Chronometrist buffers."
  (interactive)
  (cancel-timer chronometrist--timer-object)
  (setq chronometrist--timer-object nil))

(defun chronometrist-maybe-start-timer (&optional interactive-test)
  "Start `chronometrist-timer' if `chronometrist--timer-object' is non-nil.
INTERACTIVE-TEST is used to determine if this has been called
interactively."
  (interactive "p")
  (unless chronometrist--timer-object
    (setq chronometrist--timer-object
          (run-at-time t chronometrist-update-interval #'chronometrist-timer))
    (when interactive-test
      (message "Timer started."))
    t))

(defun chronometrist-force-restart-timer ()
  "Restart the timer for Chronometrist buffers."
  (interactive)
  (when chronometrist--timer-object
    (cancel-timer chronometrist--timer-object))
  (setq chronometrist--timer-object
        (run-at-time t chronometrist-update-interval #'chronometrist-timer)))

(defun chronometrist-change-update-interval (arg)
  "Change the update interval for Chronometrist buffers.

ARG should be the new update interval, in seconds."
  (interactive "NEnter new interval (in seconds): ")
  (cancel-timer chronometrist--timer-object)
  (setq chronometrist-update-interval arg
        chronometrist--timer-object nil)
  (chronometrist-maybe-start-timer))

(defgroup chronometrist nil
  "A time tracker with a nice UI."
  :group 'applications)

(defcustom chronometrist-file
  (locate-user-emacs-file "chronometrist.sexp")
  "Default path and name of the Chronometrist database.

It should be a text file containing plists in the form -
\(:name \"task name\"
 [:tags TAGS]
 [:comment \"comment\"]
 [KEY-VALUE-PAIR ...]
 :start \"TIME\"
 :stop \"TIME\"\)

Where -

TAGS is a list. It can contain any strings and symbols.

KEY-VALUE-PAIR can be any keyword-value pairs. Currently,
Chronometrist ignores them.

TIME must be an ISO-8601 time string.

\(The square brackets here refer to optional elements, not
vectors.\)"
  :type 'file)

(defcustom chronometrist-buffer-name "*Chronometrist*"
  "The name of the buffer created by `chronometrist'."
  :type 'string)

(defcustom chronometrist-hide-cursor nil
  "If non-nil, hide the cursor and only highlight the current line in the `chronometrist' buffer."
  :type 'boolean)

(defcustom chronometrist-update-interval 5
  "How often the `chronometrist' buffer should be updated, in seconds.

This is not guaranteed to be accurate - see (info \"(elisp)Timers\")."
  :type 'integer)

(defcustom chronometrist-activity-indicator "*"
  "How to indicate that a task is active.
Can be a string to be displayed, or a function which returns this string.
The default is \"*\""
  :type '(choice string function))

(defcustom chronometrist-day-start-time "00:00:00"
  "The time at which a day is considered to start, in \"HH:MM:SS\".

The default is midnight, i.e. \"00:00:00\"."
  :type 'string)

(defvar chronometrist--point nil)

(defun chronometrist-open-log (&optional _button)
  "Open `chronometrist-file' in another window.

Argument _BUTTON is for the purpose of using this command as a
button action."
  (interactive)
  (chronometrist-sexp-open-log))

(defun chronometrist-common-create-file ()
  "Create `chronometrist-file' if it doesn't already exist."
  (chronometrist-sexp-create-file))

(defun chronometrist-task-active? (task)
  "Return t if TASK is currently clocked in, else nil."
  (equal (chronometrist-current-task) task))

(defun chronometrist-activity-indicator ()
  "Return a string to indicate that a task is active.
See custom variable `chronometrist-activity-indicator'."
  (if (functionp chronometrist-activity-indicator)
      (funcall chronometrist-activity-indicator)
    chronometrist-activity-indicator))

(defun chronometrist-run-transformers (transformers arg)
  "Run TRANSFORMERS with ARG.
TRANSFORMERS should be a list of functions (F₁ ... Fₙ), each of
which should accept a single argument.

Call F₁ with ARG, with each following function being called with
the return value of the previous function.

Return the value returned by Fₙ."
  (if transformers
      (dolist (fn transformers arg)
        (setq arg (funcall fn arg)))
    arg))

(defun chronometrist-entries ()
  "Create entries to be displayed in the buffer created by `chronometrist', in the format specified by `tabulated-list-entries'."
  (->> (-sort #'string-lessp chronometrist-task-list)
       (--map-indexed
        (let* ((task        it)
               (index       (number-to-string (1+ it-index)))
               (task-button `(,task action chronometrist-toggle-task-button follow-link t))
               (task-time   (chronometrist-format-time (chronometrist-task-time-one-day task)))
               (indicator   (if (chronometrist-task-active? task) (chronometrist-activity-indicator) "")))
          (--> (vector index task-button task-time indicator)
               (list task it)
               (chronometrist-run-transformers chronometrist-entry-transformers it))))))

(defun chronometrist-task-at-point ()
  "Return the task at point in the `chronometrist' buffer, or nil if there is no task at point."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "[0-9]+ +" nil t)
      (get-text-property (point) 'tabulated-list-id))))

(defun chronometrist-goto-last-task ()
  "In the `chronometrist' buffer, move point to the line containing the last active task."
  (goto-char (point-min))
  (re-search-forward (plist-get (chronometrist-last) :name) nil t)
  (beginning-of-line))

(defun chronometrist-print-keybind (command &optional description firstonly)
  "Insert the keybindings for COMMAND.
If DESCRIPTION is non-nil, insert that too.
If FIRSTONLY is non-nil, return only the first keybinding found."
  (insert
   (format "\n% 18s - %s"
           (chronometrist-format-keybinds command chronometrist-mode-map firstonly)
           (if description description ""))))

(defun chronometrist-print-non-tabular ()
  "Print the non-tabular part of the buffer in `chronometrist'."
  (with-current-buffer chronometrist-buffer-name
    (let ((inhibit-read-only t)
          (w "\n    ")
          ;; (keybind-start-new (chronometrist-format-keybinds 'chronometrist-add-new-task chronometrist-mode-map))
          (keybind-toggle    (chronometrist-format-keybinds 'chronometrist-toggle-task chronometrist-mode-map t)))
      (goto-char (point-max))
      (--> (chronometrist-active-time-one-day)
           (chronometrist-format-time it)
           (format "%s%- 26s%s" w "Total" it)
           (insert it))
      (insert "\n")
      (insert w (format "% 17s" "Keys") w (format "% 17s" "----"))
      (chronometrist-print-keybind 'chronometrist-add-new-task)
      (insert-text-button "start a new task" 'action #'chronometrist-add-new-task-button 'follow-link t)
      (chronometrist-print-keybind 'chronometrist-toggle-task "toggle task at point")
      (chronometrist-print-keybind 'chronometrist-toggle-task-no-hooks "toggle without running hooks")
      (insert "\n " (format "%s %s - %s" "<numeric argument N>" keybind-toggle "toggle <N>th task"))
      (chronometrist-print-keybind 'chronometrist-report)
      (insert-text-button "see weekly report" 'action #'chronometrist-report 'follow-link t)
      (chronometrist-print-keybind 'chronometrist-open-log)
      (insert-text-button "view/edit log file" 'action #'chronometrist-open-log 'follow-link t)
      (insert "\n"))))

(defun chronometrist-goto-nth-task (n)
  "Move point to the line containing the Nth task.
Return the task at point, or nil if there is no corresponding
task. N must be a positive integer."
  (goto-char (point-min))
  (when (re-search-forward (format "^%d" n) nil t)
    (beginning-of-line)
    (chronometrist-task-at-point)))

(defun chronometrist-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the `chronometrist' buffer, without re-reading `chronometrist-file'.
The optional arguments _IGNORE-AUTO and _NOCONFIRM are ignored,
and are present solely for the sake of using this function as a
value of `revert-buffer-function'."
  (let* ((window (get-buffer-window chronometrist-buffer-name t))
         (point  (window-point window)))
    (when window
      (with-current-buffer chronometrist-buffer-name
        (tabulated-list-print t nil)
        (chronometrist-print-non-tabular)
        (chronometrist-maybe-start-timer)
        (set-window-point window point)))))

(defvar chronometrist--file-state nil
  "List containing the state of `chronometrist-file'.
`chronometrist-refresh-file' sets this to a plist in the form

\(:last (LAST-START LAST-END) :rest (REST-START REST-END HASH))

\(see `chronometrist-file-hash')

LAST-START and LAST-END represent the start and the end of the
last s-expression.

REST-START and REST-END represent the start of the file and the
end of the second-last s-expression.")

(defun chronometrist-file-hash (&optional start end hash)
  "Calculate hash of `chronometrist-file' between START and END.
START can be
a number or marker,
:before-last - the position at the start of the last s-expression
nil or any other value - the value of `point-min'.

END can be
a number or marker,
:before-last - the position at the end of the second-last s-expression,
nil or any other value - the position at the end of the last s-expression.

Return (START END) if HASH is nil, else (START END HASH).

Return a list in the form (A B HASH), where A and B are markers
in `chronometrist-file' describing the region for which HASH was calculated."
  (chronometrist-sexp-in-file chronometrist-file
    (let* ((start (cond ((number-or-marker-p start) start)
                        ((eq :before-last start)
                         (goto-char (point-max))
                         (backward-list))
                        (t (point-min))))
           (end   (cond ((number-or-marker-p end) end)
                        ((eq :before-last end)
                         (goto-char (point-max))
                         (backward-list 2)
                         (forward-list))
                        (t (goto-char (point-max))
                           (backward-list)
                           (forward-list)))))
      (if hash
          (--> (buffer-substring-no-properties start end)
               (secure-hash 'sha1 it)
               (list start end it))
        (list start end)))))

(defun chronometrist-read-from (position)
  (chronometrist-sexp-in-file chronometrist-file
    (goto-char (if (number-or-marker-p position)
                   position
                 (funcall position)))
    (ignore-errors (read (current-buffer)))))

(defun chronometrist-file-change-type (state)
  "Determine the type of change made to `chronometrist-file'.
STATE must be a plist. (see `chronometrist--file-state')

Return
:append  if a new s-expression was added to the end,
:modify  if the last s-expression was modified,
:remove  if the last s-expression was removed,
    nil  if the contents didn't change, and
      t  for any other change."
  (-let*
      (((last-start last-end)           (plist-get state :last))
       ((rest-start rest-end rest-hash) (plist-get state :rest))
       (last-expr-file  (chronometrist-read-from last-start))
       (last-expr-ht    (chronometrist-events-last))
       (last-same-p     (equal last-expr-ht last-expr-file))
       (file-new-length (chronometrist-sexp-in-file chronometrist-file (point-max)))
       (rest-same-p     (unless (< file-new-length rest-end)
                          (--> (chronometrist-file-hash rest-start rest-end t)
                            (cl-third it)
                            (equal rest-hash it)))))
    ;; (message "chronometrist - last-start\nlast-expr-file - %S\nlast-expr-ht - %S"
    ;;          last-expr-file
    ;;          last-expr-ht)
    ;; (message "chronometrist - last-same-p - %S, rest-same-p - %S"
    ;;          last-same-p rest-same-p)
    (cond ((not rest-same-p) t)
          (last-same-p
           (when (chronometrist-read-from last-end) :append))
          ((not (chronometrist-read-from last-start))
           :remove)
          ((not (chronometrist-read-from
                 (lambda ()
                   (progn (goto-char last-start)
                          (forward-list)))))
           :modify))))

(defun chronometrist-refresh-file (fs-event)
  "Re-read `chronometrist-file' and refresh the `chronometrist' buffer.
Argument _FS-EVENT is ignored."
  (run-hooks 'chronometrist-file-change-hook)
  ;; (message "chronometrist - file %s" fs-event)
  ;; `chronometrist-file-change-type' must be run /before/ we update `chronometrist--file-state'
  ;; (the latter represents the old state of the file, which
  ;; `chronometrist-file-change-type' compares with the newer current state)
  (-let* (((descriptor action file ...) fs-event)
          (change      (when chronometrist--file-state
                         (chronometrist-file-change-type chronometrist--file-state)))
          (reset-watch (or (eq action 'deleted) (eq action 'renamed))))
    ;; (message "chronometrist - file change type is %s" change)
    ;; if only the last plist changed, update `chronometrist-events'
    ;; and `chronometrist-task-list'
    (cond ((or reset-watch (not chronometrist--file-state) (eq change t))
           (when reset-watch
             (file-notify-rm-watch chronometrist--fs-watch)
             (setq chronometrist--fs-watch nil chronometrist--file-state nil))
           (chronometrist-events-populate)
           (chronometrist-reset-task-list))
          (chronometrist--file-state
           (let* ((old-plist (chronometrist-events-last))
                  (old-task  (plist-get old-plist :name))
                  (new-task  (plist-get (chronometrist-sexp-last) :name)))
             (pcase change
               (:append
                (chronometrist-events-update (chronometrist-sexp-last))
                (chronometrist-add-to-task-list new-task))
               (:modify
                (chronometrist-events-update (chronometrist-sexp-last) t)
                (chronometrist-remove-from-task-list old-task)
                (chronometrist-add-to-task-list new-task))
               (:remove
                (let ((date (chronometrist-events-last-date)))
                  (chronometrist-remove-from-task-list old-task)
                  (--> (gethash date chronometrist-events)
                    (-drop-last 1 it)
                    (puthash date it chronometrist-events))))
               ((pred null) nil)))))
    (setq chronometrist--file-state
          (list :last (chronometrist-file-hash :before-last nil)
                :rest (chronometrist-file-hash nil :before-last t)))
    ;; REVIEW - can we move most/all of this to the `chronometrist-file-change-hook'?
    (chronometrist-refresh)))

(defun chronometrist-query-stop ()
  "Ask the user if they would like to clock out."
  (let ((task (chronometrist-current-task)))
    (and task
         (yes-or-no-p (format "Stop tracking time for %s? " task))
         (chronometrist-out))
    t))

(defun chronometrist-in (task &optional _prefix)
  "Clock in to TASK; record current time in `chronometrist-file'.
TASK is the name of the task, a string. PREFIX is ignored."
  (interactive "P")
  (let ((plist `(:name ,task :start ,(chronometrist-format-time-iso8601))))
    (chronometrist-sexp-new plist)
    (chronometrist-refresh)))

(defun chronometrist-out (&optional _prefix)
  "Record current moment as stop time to last s-exp in `chronometrist-file'.
PREFIX is ignored."
  (interactive "P")
  (let ((plist (plist-put (chronometrist-last) :stop (chronometrist-format-time-iso8601))))
    (chronometrist-sexp-replace-last plist)))

(defvar chronometrist-mode-hook nil
  "Normal hook run at the very end of `chronometrist-mode'.")

(defvar chronometrist-list-format-transformers nil
  "List of functions to transform `tabulated-list-format' (which see).
This is called with `chronometrist-run-transformers' in `chronometrist-mode', which see.

Extensions using `chronometrist-list-format-transformers' to
increase the number of columns will also need to modify the value
of `tabulated-list-entries' by using
`chronometrist-entry-transformers'.")

(defvar chronometrist-entry-transformers nil
  "List of functions to transform each entry of `tabulated-list-entries'.
This is called with `chronometrist-run-transformers' in `chronometrist-entries', which see.

Extensions using `chronometrist-entry-transformers' to increase
the number of columns will also need to modify the value of
`tabulated-list-format' by using
`chronometrist-list-format-transformers'.")

(defvar chronometrist-before-in-functions nil
  "Functions to run before a task is clocked in.
Each function in this hook must accept a single argument, which
is the name of the task to be clocked-in.

The commands `chronometrist-toggle-task-button',
`chronometrist-add-new-task-button', `chronometrist-toggle-task',
and `chronometrist-add-new-task' will run this hook.")

(defvar chronometrist-after-in-functions nil
  "Functions to run after a task is clocked in.
Each function in this hook must accept a single argument, which
is the name of the task to be clocked-in.

The commands `chronometrist-toggle-task-button',
`chronometrist-add-new-task-button', `chronometrist-toggle-task',
and `chronometrist-add-new-task' will run this hook.")

(defvar chronometrist-before-out-functions nil
  "Functions to run before a task is clocked out.
Each function in this hook must accept a single argument, which
is the name of the task to be clocked out of.

The task will be stopped only if all functions in this list
return a non-nil value.")

(defvar chronometrist-after-out-functions nil
  "Functions to run after a task is clocked out.
Each function in this hook must accept a single argument, which
is the name of the task to be clocked out of.")

(defvar chronometrist-file-change-hook nil
  "Functions to be run after `chronometrist-file' is changed on disk.")

(defun chronometrist-run-functions-and-clock-in (task)
  "Run hooks and clock in to TASK."
  (run-hook-with-args 'chronometrist-before-in-functions task)
  (chronometrist-in task)
  (run-hook-with-args 'chronometrist-after-in-functions task))

(defun chronometrist-run-functions-and-clock-out (task)
  "Run hooks and clock out of TASK."
  (when (run-hook-with-args-until-failure 'chronometrist-before-out-functions task)
    (chronometrist-out)
    (run-hook-with-args 'chronometrist-after-out-functions task)))

(defvar chronometrist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")   #'chronometrist-toggle-task)
    (define-key map (kbd "M-RET") #'chronometrist-toggle-task-no-hooks)
    (define-key map (kbd "l")     #'chronometrist-open-log)
    (define-key map (kbd "r")     #'chronometrist-report)
    (define-key map [mouse-1]     #'chronometrist-toggle-task)
    (define-key map [mouse-3]     #'chronometrist-toggle-task-no-hooks)
    (define-key map (kbd "a")     #'chronometrist-add-new-task)
    map)
  "Keymap used by `chronometrist-mode'.")

(define-derived-mode chronometrist-mode tabulated-list-mode "Chronometrist"
  "Major mode for `chronometrist'."
  (make-local-variable 'tabulated-list-format)
  (--> [("#" 3 t) ("Task" 25 t) ("Time" 10 t) ("Active" 10 t)]
        (chronometrist-run-transformers chronometrist-list-format-transformers it)
        (setq tabulated-list-format it))
  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'chronometrist-entries)
  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Task" . nil))
  (tabulated-list-init-header)
  (setq revert-buffer-function #'chronometrist-refresh)
  (run-hooks 'chronometrist-mode-hook))

(defun chronometrist-toggle-task-button (_button)
  "Button action to toggle a task.
Argument _BUTTON is for the purpose of using this as a button
action, and is ignored."
  (when current-prefix-arg
    (chronometrist-goto-nth-task (prefix-numeric-value current-prefix-arg)))
  (let ((current  (chronometrist-current-task))
        (at-point (chronometrist-task-at-point)))
    ;; clocked in + point on current    = clock out
    ;; clocked in + point on some other task = clock out, clock in to task
    ;; clocked out = clock in
    (when current
      (chronometrist-run-functions-and-clock-out current))
    (unless (equal at-point current)
      (chronometrist-run-functions-and-clock-in at-point))))

(defun chronometrist-add-new-task-button (_button)
  "Button action to add a new task.
Argument _BUTTON is for the purpose of using this as a button
action, and is ignored."
  (let ((current (chronometrist-current-task)))
    (when current
      (chronometrist-run-functions-and-clock-out current))
    (let ((task (read-from-minibuffer "New task name: " nil nil nil nil nil t)))
      (chronometrist-run-functions-and-clock-in task))))

;; TODO - if clocked in and point not on a task, just clock out
(defun chronometrist-toggle-task (&optional prefix inhibit-hooks)
  "Start or stop the task at point.

If there is no task at point, do nothing.

With numeric prefix argument PREFIX, toggle the Nth task in
the buffer. If there is no corresponding task, do nothing.

If INHIBIT-HOOKS is non-nil, the hooks
`chronometrist-before-in-functions',
`chronometrist-after-in-functions',
`chronometrist-before-out-functions', and
`chronometrist-after-out-functions' will not be run."
  (interactive "P")
  (let* ((empty-file   (chronometrist-common-file-empty-p chronometrist-file))
         (nth          (when prefix (chronometrist-goto-nth-task prefix)))
         (at-point     (chronometrist-task-at-point))
         (target       (or nth at-point))
         (current      (chronometrist-current-task))
         (in-function  (if inhibit-hooks
                           #'chronometrist-in
                         #'chronometrist-run-functions-and-clock-in))
         (out-function (if inhibit-hooks
                           #'chronometrist-out
                         #'chronometrist-run-functions-and-clock-out)))
    ;; do not run hooks - chronometrist-add-new-task will do it
    (cond (empty-file (chronometrist-add-new-task))
          ;; What should we do if the user provides an invalid
          ;; argument? Currently - nothing.
          ((and prefix (not nth)))
          (target ;; do nothing if there's no task at point
           ;; clocked in + target is current = clock out
           ;; clocked in + target is some other task = clock out, clock in to task
           ;; clocked out = clock in
           (when current
             (funcall out-function current))
           (unless (equal target current)
             (funcall in-function target))))))

(defun chronometrist-toggle-task-no-hooks (&optional prefix)
  "Like `chronometrist-toggle-task', but don't run hooks.

With numeric prefix argument PREFIX, toggle the Nth task. If there
is no corresponding task, do nothing."
  (interactive "P")
  (chronometrist-toggle-task prefix t))

(defun chronometrist-add-new-task ()
  "Add a new task."
  (interactive)
  (chronometrist-add-new-task-button nil))

;;;###autoload
(defun chronometrist (&optional arg)
  "Display the user's tasks and the time spent on them today.

Based on their timelog file `chronometrist-file'. This is the
'listing command' for `chronometrist-mode'.

If numeric argument ARG is 1, run `chronometrist-report'.
If numeric argument ARG is 2, run `chronometrist-statistics'."
  (interactive "P")
  (chronometrist-migrate-check)
  (let ((buffer (get-buffer-create chronometrist-buffer-name))
        (w      (save-excursion
                  (get-buffer-window chronometrist-buffer-name t))))
    (cond
     (arg (cl-case arg
            (1 (chronometrist-report))
            (2 (chronometrist-statistics))))
     (w (with-current-buffer buffer
          (setq chronometrist--point (point))
          (kill-buffer chronometrist-buffer-name)))
     (t (with-current-buffer buffer
          (cond ((or (not (file-exists-p chronometrist-file))
                     (chronometrist-common-file-empty-p chronometrist-file))
                 ;; first run
                 (chronometrist-common-create-file)
                 (let ((inhibit-read-only t))
                   (chronometrist-common-clear-buffer buffer)
                   (insert "Welcome to Chronometrist! Hit RET to ")
                   (insert-text-button "start a new task."
                                       'action #'chronometrist-add-new-task-button
                                       'follow-link t)
                   (chronometrist-mode)
                   (switch-to-buffer buffer)))
                (t (chronometrist-mode)
                   (when chronometrist-hide-cursor
                     (make-local-variable 'cursor-type)
                     (setq cursor-type nil)
                     (hl-line-mode))
                   (switch-to-buffer buffer)
                   (if (hash-table-keys chronometrist-events)
                       (chronometrist-refresh)
                     (chronometrist-refresh-file nil))
                   (if chronometrist--point
                       (goto-char chronometrist--point)
                     (chronometrist-goto-last-task))))
          (unless chronometrist--fs-watch
            (setq chronometrist--fs-watch
                  (file-notify-add-watch chronometrist-file '(change) #'chronometrist-refresh-file))))))))

(defgroup chronometrist-report nil
  "Weekly report for the `chronometrist' time tracker."
  :group 'chronometrist)

(defcustom chronometrist-report-buffer-name "*Chronometrist-Report*"
  "The name of the buffer created by `chronometrist-report'."
  :type 'string)

(defcustom chronometrist-report-week-start-day "Sunday"
  "The day used for start of week by `chronometrist-report'."
  :type 'string)

(defcustom chronometrist-report-weekday-number-alist
  '(("Sunday"    . 0)
    ("Monday"    . 1)
    ("Tuesday"   . 2)
    ("Wednesday" . 3)
    ("Thursday"  . 4)
    ("Friday"    . 5)
    ("Saturday"  . 6))
  "Alist in the form (\"NAME\" . NUMBER), where \"NAME\" is the name of a weekday and NUMBER its associated number."
  :type 'alist)

(defvar chronometrist-report--ui-date nil
  "The first date of the week displayed by `chronometrist-report'.
A value of nil means the current week. Otherwise, it must be a
date in the form \"YYYY-MM-DD\".")

(defvar chronometrist-report--ui-week-dates nil
  "List of dates currently displayed by `chronometrist-report'.
Each date is a list containing calendrical information (see (info \"(elisp)Time Conversion\"))")

(defvar chronometrist-report--point nil)

(defun chronometrist-report-date-to-dates-in-week (first-date-in-week)
  "Return a list of dates in a week, starting from FIRST-DATE-IN-WEEK.
Each date is a ts struct (see `ts.el').

FIRST-DATE-IN-WEEK must be a ts struct representing the first date."
  (cl-loop for i from 0 to 6 collect
           (ts-adjust 'day i first-date-in-week)))

(defun chronometrist-report-date-to-week-dates ()
  "Return dates in week as a list.
Each element is a ts struct (see `ts.el').

The first date is the first occurrence of
`chronometrist-report-week-start-day' before the date specified in
`chronometrist-report--ui-date' (if non-nil) or the current date."
  (->> (or chronometrist-report--ui-date (chronometrist-date))
       (chronometrist-previous-week-start)
       (chronometrist-report-date-to-dates-in-week)))

(defun chronometrist-report-entries ()
  "Create entries to be displayed in the `chronometrist-report' buffer."
  (cl-loop
    ;; `chronometrist-report-date-to-week-dates' uses today if chronometrist-report--ui-date is nil
    with week-dates = (progn (setq chronometrist-report--ui-week-dates week-dates)
                             (chronometrist-report-date-to-week-dates))
    for task in chronometrist-task-list collect
    (let* ((durations        (--map (chronometrist-task-time-one-day task (chronometrist-date it))
                                    week-dates))
           (duration-strings (mapcar #'chronometrist-format-time durations))
           (total-duration   (->> (-reduce #'+ durations)
                                  (chronometrist-format-time)
                                  (vector))))
      (list task
            (vconcat
             (vector task)
             duration-strings ;; vconcat converts lists to vectors
             total-duration)))))

(defun chronometrist-report-print-keybind (command &optional description firstonly)
  "Insert one or more keybindings for COMMAND into the current buffer.
DESCRIPTION is a description of the command.

If FIRSTONLY is non-nil, insert only the first keybinding found."
  (insert "\n    "
          (chronometrist-format-keybinds command firstonly)
          " - "
          (if description description "")))

(defun chronometrist-report-print-non-tabular ()
  "Print the non-tabular part of the buffer in `chronometrist-report'."
  (let ((inhibit-read-only t)
        (w                 "\n    ")
        (total-time-daily  (->> chronometrist-report--ui-week-dates
                                (mapcar #'chronometrist-date)
                                (mapcar #'chronometrist-active-time-one-day))))
    (goto-char (point-min))
    (insert "                         ")
    (insert (mapconcat (lambda (ts)
                         (ts-format "%F" ts))
                       (chronometrist-report-date-to-week-dates)
                       " "))
    (insert "\n")
    (goto-char (point-max))
    (insert w (format "%- 21s" "Total"))
    (->> total-time-daily
         (mapcar #'chronometrist-format-time)
         (--map (format "% 9s  " it))
         (apply #'insert))
    (->> total-time-daily
         (-reduce #'+)
         (chronometrist-format-time)
         (format "% 13s")
         (insert))
    (insert "\n" w)
    (insert-text-button "<<" 'action #'chronometrist-report-previous-week 'follow-link t)
    (insert (format "% 4s" " "))
    (insert-text-button ">>" 'action #'chronometrist-report-next-week 'follow-link t)
    (insert "\n")
    (chronometrist-report-print-keybind 'chronometrist-report-previous-week)
    (insert-text-button "previous week" 'action #'chronometrist-report-previous-week 'follow-link t)
    (chronometrist-report-print-keybind 'chronometrist-report-next-week)
    (insert-text-button "next week" 'action #'chronometrist-report-next-week 'follow-link t)
    (chronometrist-report-print-keybind 'chronometrist-open-log)
    (insert-text-button "open log file" 'action #'chronometrist-open-log 'follow-link t)))

(defun chronometrist-report-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the `chronometrist-report' buffer, without re-reading `chronometrist-file'."
  (let* ((w (get-buffer-window chronometrist-report-buffer-name t))
         (p (point)))
    (with-current-buffer chronometrist-report-buffer-name
      (tabulated-list-print t nil)
      (chronometrist-report-print-non-tabular)
      (chronometrist-maybe-start-timer)
      (set-window-point w p))))

(defun chronometrist-report-refresh-file (_fs-event)
  "Re-read `chronometrist-file' and refresh the `chronometrist-report' buffer.
Argument _FS-EVENT is ignored."
  (chronometrist-events-populate)
  (chronometrist-report-refresh))

(defvar chronometrist-report-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'chronometrist-open-log)
    (define-key map (kbd "b") #'chronometrist-report-previous-week)
    (define-key map (kbd "f") #'chronometrist-report-next-week)
    ;; Works when number of tasks < screen length; after that, you
    ;; probably expect mousewheel to scroll up/down, and
    ;; alt-mousewheel or something for next/previous week. For now,
    ;; I'm assuming most people won't have all that many tasks - I've
    ;; been using it for ~2 months and have 18 tasks, which are
    ;; still just half the screen on my 15" laptop. Let's see what
    ;; people say.
    (define-key map [mouse-4] #'chronometrist-report-next-week)
    (define-key map [mouse-5] #'chronometrist-report-previous-week)
    map)
  "Keymap used by `chronometrist-report-mode'.")

(define-derived-mode chronometrist-report-mode tabulated-list-mode "Chronometrist-Report"
  "Major mode for `chronometrist-report'."
  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format [("Task"   25 t)
                               ("Sunday"    10 t)
                               ("Monday"    10 t)
                               ("Tuesday"   10 t)
                               ("Wednesday" 10 t)
                               ("Thursday"  10 t)
                               ("Friday"    10 t)
                               ("Saturday"  10 t :pad-right 5)
                               ("Total"     12 t)])
  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'chronometrist-report-entries)
  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Task" . nil))
  (tabulated-list-init-header)
  (chronometrist-maybe-start-timer)
  (add-hook 'chronometrist-timer-hook
            (lambda ()
              (when (get-buffer-window chronometrist-report-buffer-name)
                (chronometrist-report-refresh))))
  (setq revert-buffer-function #'chronometrist-report-refresh)
  (unless chronometrist--fs-watch
    (setq chronometrist--fs-watch
          (file-notify-add-watch chronometrist-file
                                 '(change)
                                 #'chronometrist-refresh-file))))

;;;###autoload
(defun chronometrist-report (&optional keep-date)
  "Display a weekly report of the data in `chronometrist-file'.

 This is the 'listing command' for ‘chronometrist-report-mode’.

If a buffer called `chronometrist-report-buffer-name' already
exists and is visible, kill the buffer.

If KEEP-DATE is nil (the default when not supplied), set
`chronometrist-report--ui-date' to nil and display data from the
current week. Otherwise, display data from the week specified by
`chronometrist-report--ui-date'."
  (interactive)
  (chronometrist-migrate-check)
  (let ((buffer (get-buffer-create chronometrist-report-buffer-name)))
    (with-current-buffer buffer
      (cond ((and (get-buffer-window chronometrist-report-buffer-name)
                  (not keep-date))
             (setq chronometrist-report--point (point))
             (kill-buffer buffer))
            (t (unless keep-date
                 (setq chronometrist-report--ui-date nil))
               (chronometrist-common-create-file)
               (chronometrist-report-mode)
               (switch-to-buffer buffer)
               (chronometrist-report-refresh-file nil)
               (goto-char (or chronometrist-report--point 1)))))))

(defun chronometrist-report-previous-week (arg)
  "View the previous week's report.
With prefix argument ARG, move back ARG weeks."
  (interactive "P")
  (let ((arg (if (and arg (numberp arg))
                 (abs arg)
               1)))
    (setq chronometrist-report--ui-date
          (ts-adjust 'day (- (* arg 7))
                     (if chronometrist-report--ui-date
                         chronometrist-report--ui-date
                       (ts-now)))))
  (setq chronometrist-report--point (point))
  (kill-buffer)
  (chronometrist-report t))

(defun chronometrist-report-next-week (arg)
  "View the next week's report.
With prefix argument ARG, move forward ARG weeks."
  (interactive "P")
  (let ((arg (if (and arg (numberp arg))
                 (abs arg)
               1)))
    (setq chronometrist-report--ui-date
          (ts-adjust 'day (* arg 7)
                     (if chronometrist-report--ui-date
                         chronometrist-report--ui-date
                       (ts-now))))
    (setq chronometrist-report--point (point))
    (kill-buffer)
    (chronometrist-report t)))

(defgroup chronometrist-statistics nil
  "Statistics buffer for the `chronometrist' time tracker."
  :group 'chronometrist)

(defcustom chronometrist-statistics-buffer-name "*Chronometrist-Statistics*"
  "The name of the buffer created by `chronometrist-statistics'."
  :type 'string)

(defvar chronometrist-statistics--ui-state nil
  "Stores the display state for `chronometrist-statistics'.

This must be a plist in the form (:MODE :START :END).

:MODE is either 'week, 'month, 'year, 'full, or 'custom.

'week, 'month, and 'year mean display statistics
weekly/monthly/yearly respectively.

'full means display statistics from the beginning to the end of
the `chronometrist-file'.

'custom means display statistics from an arbitrary date range.

:START and :END are the start and end of the date range to be
displayed. They must be ts structs (see `ts.el').")

(defvar chronometrist-statistics--point nil)

(defvar chronometrist-statistics-mode-map)

(cl-defun chronometrist-statistics-count-average-time-spent (task &optional (table chronometrist-events))
  "Return the average time the user has spent on TASK from TABLE.
TABLE should be a hash table - if not supplied,
`chronometrist-events' is used."
  (cl-loop with days = 0
    for date being the hash-keys of table collect
    (let ((events-in-day (chronometrist-task-events-in-day task (chronometrist-iso-date-to-ts date))))
      (when events-in-day
        (cl-incf days)
        (-reduce #'+ (chronometrist-events-to-durations events-in-day))))
    into per-day-time-list
    finally return
    (if per-day-time-list
        (/ (-reduce #'+ per-day-time-list) days)
      0)))

(defun chronometrist-statistics-entries-internal (table)
  "Helper function for `chronometrist-statistics-entries'.

It simply operates on the entire hash table TABLE (see
`chronometrist-events' for table format), so ensure that TABLE is
reduced to the desired range using
`chronometrist-events-subset'."
  (mapcar (lambda (task)
            (let* ((active-days    (chronometrist-statistics-count-active-days task table))
                   (active-percent (cl-case (plist-get chronometrist-statistics--ui-state :mode)
                                     ('week (* 100 (/ active-days 7.0)))))
                   (active-percent (if (zerop active-days)
                                       (format "    % 6s" "-")
                                     (format "    %05.2f%%" active-percent)))
                   (active-days    (format "% 5s"
                                           (if (zerop active-days)
                                               "-"
                                             active-days)))
                   (average-time   (->> (chronometrist-statistics-count-average-time-spent task table)
                                        (chronometrist-format-time)
                                        (format "% 5s")))
                   (content        (vector task
                                           active-days
                                           active-percent
                                           average-time)))
              (list task content)))
          chronometrist-task-list))

(defun chronometrist-statistics-entries ()
  "Create entries to be displayed in the buffer created by `chronometrist-statistics'."
  ;; We assume that all fields in `chronometrist-statistics--ui-state' are set, so they must
  ;; be changed by the view-changing functions.
  (cl-case (plist-get chronometrist-statistics--ui-state :mode)
    ('week
     (let* ((start (plist-get chronometrist-statistics--ui-state :start))
            (end   (plist-get chronometrist-statistics--ui-state :end))
            (ht    (chronometrist-events-subset start end)))
       (chronometrist-statistics-entries-internal ht)))
    (t ;; `chronometrist-statistics--ui-state' is nil, show current week's data
     (let* ((start (chronometrist-previous-week-start (chronometrist-date)))
            (end   (ts-adjust 'day 7 start))
            (ht    (chronometrist-events-subset start end)))
       (setq chronometrist-statistics--ui-state `(:mode week :start ,start :end ,end))
       (chronometrist-statistics-entries-internal ht)))))

(defun chronometrist-statistics-print-keybind (command &optional description firstonly)
  "Insert the keybindings for COMMAND.
If DESCRIPTION is non-nil, insert that too.
If FIRSTONLY is non-nil, return only the first keybinding found."
  (insert "\n    "
          (chronometrist-format-keybinds command
                             chronometrist-statistics-mode-map
                             firstonly)
          " - "
          (if description description "")))

(defun chronometrist-statistics-print-non-tabular ()
  "Print the non-tabular part of the buffer in `chronometrist-statistics'."
  (let ((w "\n    ")
        (inhibit-read-only t))
    (goto-char (point-max))
    (insert w)
    (insert-text-button (cl-case (plist-get chronometrist-statistics--ui-state :mode)
                          ('week "Weekly view"))
                        ;; 'action #'chronometrist-report-previous-week ;; TODO - make interactive function to accept new mode from user
                        'follow-link t)
    (insert ", from")
    (insert
     (format " %s to %s\n"
             (ts-format "%F" (plist-get chronometrist-statistics--ui-state :start))
             (ts-format "%F" (plist-get chronometrist-statistics--ui-state :end))))))

(defun chronometrist-statistics-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the `chronometrist-statistics' buffer.
This does not re-read `chronometrist-file'.

The optional arguments _IGNORE-AUTO and _NOCONFIRM are ignored,
and are present solely for the sake of using this function as a
value of `revert-buffer-function'."
  (let* ((w (get-buffer-window chronometrist-statistics-buffer-name t))
         (p (point)))
    (with-current-buffer chronometrist-statistics-buffer-name
      (tabulated-list-print t nil)
      (chronometrist-statistics-print-non-tabular)
      (chronometrist-maybe-start-timer)
      (set-window-point w p))))

(defvar chronometrist-statistics-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'chronometrist-open-log)
    (define-key map (kbd "b") #'chronometrist-statistics-previous-range)
    (define-key map (kbd "f") #'chronometrist-statistics-next-range)
    map)
  "Keymap used by `chronometrist-statistics-mode'.")

(define-derived-mode chronometrist-statistics-mode tabulated-list-mode "Chronometrist-Statistics"
  "Major mode for `chronometrist-statistics'."
  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format
        [("Task"              25 t)
         ("Active days"       12 t)
         ("%% of days active" 17 t)
         ("Average time"      12 t)
         ;; ("Current streak"    10 t)
         ;; ("Last streak"       10 t)
         ;; ("Longest streak"    10 t)
         ])
  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'chronometrist-statistics-entries)
  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Task" . nil))
  (tabulated-list-init-header)
  ;; (chronometrist-maybe-start-timer)
  (add-hook 'chronometrist-timer-hook
            (lambda ()
              (when (get-buffer-window chronometrist-statistics-buffer-name)
                (chronometrist-statistics-refresh))))
  (setq revert-buffer-function #'chronometrist-statistics-refresh)
  (unless chronometrist--fs-watch
    (setq chronometrist--fs-watch
          (file-notify-add-watch chronometrist-file
                                 '(change)
                                 #'chronometrist-refresh-file))))

;;;###autoload
(defun chronometrist-statistics (&optional preserve-state)
  "Display statistics for data in `chronometrist-file'.
This is the 'listing command' for `chronometrist-statistics-mode'.

If a buffer called `chronometrist-statistics-buffer-name' already
exists and is visible, kill the buffer.

If PRESERVE-STATE is nil (the default when not supplied), display
data from the current week. Otherwise, display data from the week
specified by `chronometrist-statistics--ui-state'."
  (interactive)
  (chronometrist-migrate-check)
  (let* ((buffer     (get-buffer-create chronometrist-statistics-buffer-name))
         (today      (chronometrist-date))
         (week-start (chronometrist-previous-week-start today))
         (week-end   (ts-adjust 'day 6 week-start)))
    (with-current-buffer buffer
      (cond ((get-buffer-window chronometrist-statistics-buffer-name)
             (kill-buffer buffer))
            (t ;; (delete-other-windows)
             (unless preserve-state
               (setq chronometrist-statistics--ui-state `(:mode week
                                                   :start ,week-start
                                                   :end   ,week-end)))
             (chronometrist-common-create-file)
             (chronometrist-statistics-mode)
             (switch-to-buffer buffer)
             (chronometrist-statistics-refresh))))))

(defun chronometrist-statistics-previous-range (arg)
  "View the statistics in the previous time range.
If ARG is a numeric argument, go back that many times."
  (interactive "P")
  (let* ((arg   (if (and arg (numberp arg))
                    (abs arg)
                  1))
         (start (plist-get chronometrist-statistics--ui-state :start)))
    (cl-case (plist-get chronometrist-statistics--ui-state :mode)
      ('week
       (let* ((new-start (ts-adjust 'day (- (* arg 7)) start))
              (new-end   (ts-adjust 'day +6 new-start)))
         (plist-put chronometrist-statistics--ui-state :start new-start)
         (plist-put chronometrist-statistics--ui-state :end   new-end))))
    (setq chronometrist-statistics--point (point))
    (kill-buffer)
    (chronometrist-statistics t)))

(defun chronometrist-statistics-next-range (arg)
  "View the statistics in the next time range.
If ARG is a numeric argument, go forward that many times."
  (interactive "P")
  (let* ((arg   (if (and arg (numberp arg))
                    (abs arg)
                  1))
         (start (plist-get chronometrist-statistics--ui-state :start)))
    (cl-case (plist-get chronometrist-statistics--ui-state :mode)
      ('week
       (let* ((new-start (ts-adjust 'day (* arg 7) start))
              (new-end   (ts-adjust 'day 6 new-start)))
         (plist-put chronometrist-statistics--ui-state :start new-start)
         (plist-put chronometrist-statistics--ui-state :end   new-end))))
    (setq chronometrist-statistics--point (point))
    (kill-buffer)
    (chronometrist-statistics t)))

(provide 'chronometrist)

;;; chronometrist.el ends here


