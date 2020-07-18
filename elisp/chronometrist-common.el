;;; chronometrist-common.el --- Common definitions for Chronometrist -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>

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

(require 'dash)
(require 'cl-lib)
(require 'ts)

(require 'chronometrist-custom)
(require 'chronometrist-report-custom)
(require 'chronometrist-time)
(require 'chronometrist-sexp)

;; ## VARIABLES ##
;;; Code:

(defvar chronometrist-empty-time-string "-")

(defvar chronometrist-date-re "[0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\}")

(defvar chronometrist-time-re-ui
  (rx-to-string
   `(or
     (and (repeat 0 2
                  (optional (repeat 1 2 digit) ":"))
          (repeat 1 2 digit))
     ,chronometrist-empty-time-string))
  "Regular expression to represent a timestamp in `chronometrist'.
This is distinct from `chronometrist-time-re-file' (which see) -
`chronometrist-time-re-ui' is meant for the user interface, and
must correspond to the output from `chronometrist-format-time'.")

(defvar chronometrist-task-list nil
  "List of tasks in `chronometrist-file', as returned by `chronometrist-tasks-from-table'.")

(defun chronometrist-task-list-add (task)
  "Add TASK to `chronometrist-task-list', if it is not already present."
  (unless (member task chronometrist-task-list)
    (--> (list task)
         (append chronometrist-task-list it)
         (sort it #'string-lessp)
         (setq chronometrist-task-list it))))

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
      (let ((h (if (zerop h)
                   blank
                 (format "%2d:" h)))
            (m (cond ((and (zerop h)
                           (zerop m))
                      blank)
                     ((zerop h)
                      (format "%2d:" m))
                     (t
                      (format "%02d:" m))))
            (s (if (and (zerop h)
                        (zerop m))
                   (format "%2d" s)
                 (format "%02d" s))))
        (concat h m s)))))

(defun chronometrist-common-file-empty-p (file)
  "Return t if FILE is empty."
  (let ((size (elt (file-attributes file) 7)))
    (if (zerop size) t nil)))

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

(defun chronometrist-events->ts-pairs (events)
  "Convert EVENTS to a list of ts struct pairs (see `ts.el').

EVENTS must be a list of valid Chronometrist property lists (see
`chronometrist-file')."
  (cl-loop for plist in events collect
           (let* ((start (chronometrist-iso-timestamp->ts
                          (plist-get plist :start)))
                  (stop (plist-get plist :stop))
                  (stop (if stop
                            (chronometrist-iso-timestamp->ts stop)
                          (ts-now))))
             (cons start stop))))

(defun chronometrist-ts-pairs->durations (ts-pairs)
  "Return the durations represented by TS-PAIRS.
TS-PAIRS is a list of pairs, where each element is a ts struct (see `ts.el').

Return seconds as an integer, or 0 if TS-PAIRS is nil."
  (if ts-pairs
      (cl-loop for pair in ts-pairs collect
               (ts-diff (cdr pair) (car pair)))
    0))

(defun chronometrist-previous-week-start (ts)
  "Find the previous `chronometrist-report-week-start-day' from TS.

Return a ts struct for said day's beginning.

If the day of TS is the same as the
`chronometrist-report-week-start-day', return TS.

TS must be a ts struct (see `ts.el')."
  (cl-loop until (equal chronometrist-report-week-start-day
                        (ts-day-name ts))
           do (ts-decf (ts-day ts))
           finally return ts))

(provide 'chronometrist-common)

;;; chronometrist-common.el ends here
