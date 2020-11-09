;;; chronometrist-migrate.el --- Commands to aid in migrating between event formats -*- lexical-binding: t; -*-

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

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'seq)
(require 'chronometrist-common)
(require 'chronometrist-time)
(require 'chronometrist-plist-pp)
(require 'emacsql-sqlite3)

(defvar chronometrist-file)
(defvar chronometrist-migrate-table (make-hash-table :test #'equal))

;; TODO - support other timeclock codes (currently only "i" and "o"
;; are supported.)
(defun chronometrist-migrate-populate-timelog (in-file)
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

(defun chronometrist-migrate-timelog-file->sexp-file (&optional in-file out-file)
  "Migrate your existing `timeclock-file' to the Chronometrist file format.
IN-FILE and OUT-FILE, if provided, are used as input and output
file names respectively."
  (interactive `(,(if (featurep 'timeclock)
                      (read-file-name (format "timeclock file (default: %s): "
                                              timeclock-file)
                                      user-emacs-directory
                                      timeclock-file t)
                    (read-file-name "timeclock file: " user-emacs-directory nil t))
                 ,(read-file-name (format "Output file (default: %s): "
                                          (locate-user-emacs-file "chronometrist.sexp"))
                                  user-emacs-directory
                                  (locate-user-emacs-file "chronometrist.sexp"))))
  (when (if (file-exists-p out-file)
            (yes-or-no-p (format "Output file %s already exists - overwrite? " out-file))
          t)
    (let ((output (find-file-noselect out-file)))
      (with-current-buffer output
        (chronometrist-common-clear-buffer output)
        (chronometrist-migrate-populate-timelog in-file)
        (maphash (lambda (_key value)
                   (chronometrist-plist-pp value output)
                   (insert "\n\n"))
                 chronometrist-migrate-table)
        (save-buffer)))))

(defvar chronometrist-migrate-db (emacsql-sqlite3 (locate-user-emacs-file "chronometrist.sqlite")))

(defun chronometrist-migrate-populate-sexp (in-file)
  "Read data from IN-FILE to `chronometrist-migrate-table'.

IN-FILE should be a file in the Chronometrist s-expression format.
See `chronometrist-file' for a description."
  (clrhash chronometrist-migrate-table)
  (with-current-buffer (find-file-noselect in-file)
    (save-excursion
      (goto-char (point-min))
      (let ((index 0)
            expr)
        (while (setq expr (ignore-errors (read (current-buffer))))
          (let* ((new-date       (s-left 10 (plist-get expr :start)))
                 (existing-value (gethash new-date chronometrist-migrate-table)))
            (cl-incf index)
            (puthash new-date
                     (if existing-value
                         (append existing-value
                                 (list expr))
                       (list expr))
                     chronometrist-migrate-table)))
        (unless (zerop index) index)))))

(defun chronometrist-migrate-to-sqlite3 ()
  (cl-loop with count = 0
    for events being the hash-values of chronometrist-events do
    (cl-loop for event in events do
      (let* ((keywords (seq-filter #'keywordp event))
             (values   (seq-remove #'keywordp event))
             (columns  (mapcar (lambda (keyword)
                                 (--> (symbol-name keyword)
                                      (s-chop-prefix ":" it)
                                      ;; emacsql seems to automatically
                                      ;; convert dashes in column names
                                      ;; to underscores, so we do the
                                      ;; same, lest we get a "column
                                      ;; already exists" error
                                      (replace-regexp-in-string "-" "_" it)
                                      (intern it)))
                               keywords)))
        ;; ensure all keywords in this plist exist as SQL columns
        (cl-loop for column in columns do
          (let* ((pragma        (emacsql chronometrist-migrate-db [:pragma (funcall table_info events)]))
                 (column-exists (cl-loop for column-spec in pragma thereis
                                  (eq column (second column-spec)))))
            (unless column-exists
              (emacsql chronometrist-migrate-db [:alter-table events :add-column $i1] column))))
        (emacsql chronometrist-migrate-db [:insert-into events [$i1] :values $v2]
                 (vconcat columns) (vconcat values)))
      (incf count)
      (when (zerop (% count 5))
        (message "chronometrist-migrate-migrate - %s events converted" count)))
    finally do
    (message "chronometrist-migrate - finished converting %s events." count)))

(defun chronometrist-migrate (input-format output-format &optional input-file output-file)
  "Migrate a Chronometrist file from INPUT-FORMAT to OUTPUT-FORMAT.
Prompt for INPUT-FILE and OUTPUT-FILE if not provided.")

(defun chronometrist-migrate-check ()
  "Offer to import data from `timeclock-file' if `chronometrist-file' does not exist."
  (when (and (bound-and-true-p timeclock-file)
             (not (file-exists-p chronometrist-file)))
    (if (yes-or-no-p (format (concat "Chronometrist v0.3+ uses a new file format;"
                                     " import data from %s ? ")
                             timeclock-file))
        (chronometrist-migrate-timelog-file->sexp-file timeclock-file chronometrist-file)
      (message "You can migrate later using `chronometrist-migrate-timelog-file->sexp-file'."))))

(provide 'chronometrist-migrate)

;;; chronometrist-migrate.el ends here
