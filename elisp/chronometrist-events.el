;;; chronometrist-events.el --- Event management and querying code for Chronometrist -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; For more information, please refer to <https://unlicense.org>

;; (require 'chronometrist-plist-pp)
(require 'chronometrist-common)
(require 'chronometrist-sexp)
(require 'ts)

;; external -
;; chronometrist-day-start-time (-custom)
;; chronometrist-midnight-spanning-p (-time)
;; chronometrist-date-less-p (-time)

;;; Commentary:
;;

;;; Code:

(defvar chronometrist-events (make-hash-table :test #'equal)
  "Each key is a date in the form (YEAR MONTH DAY).

Values are lists containing events, where each event is a list in
the form (:name \"NAME\" :tags (TAGS) <key value pairs> ...
:start TIME :stop TIME).")

(defun chronometrist-day-start (timestamp)
  "Get start of day (according to `chronometrist-day-start-time') for TIMESTAMP.

TIMESTAMP must be a time string in the ISO-8601 format.

Return value is a time value (see `current-time')."
  (let ((timestamp-date-list (->> timestamp
                                  (parse-iso8601-time-string)
                                  (decode-time)
                                  (-drop 3)
                                  (-take 3))))
    (--> chronometrist-day-start-time
         (split-string it ":")
         (mapcar #'string-to-number it)
         (reverse it)
         (append it timestamp-date-list)
         (apply #'encode-time it))))

;; (defun chronometrist-file-clean ()
;;   "Clean `chronometrist-file' so that events can be processed accurately.
;; NOTE - currently unused.

;; This function splits midnight-spanning intervals into two. It
;; must be called before running `chronometrist-populate'.

;; It returns t if the table was modified, else nil."
;;   (let ((buffer (find-file-noselect chronometrist-file))
;;         modified
;;         expr)
;;     (with-current-buffer buffer
;;       (save-excursion
;;         (goto-char (point-min))
;;         (while (setq expr (ignore-errors (read (current-buffer))))
;;           (when (plist-get expr :stop)
;;             (let ((split-time (chronometrist-midnight-spanning-p (plist-get expr :start)
;;                                                      (plist-get expr :stop))))
;;               (when split-time
;;                 (let ((first-start  (plist-get (cl-first  split-time) :start))
;;                       (first-stop   (plist-get (cl-first  split-time) :stop))
;;                       (second-start (plist-get (cl-second split-time) :start))
;;                       (second-stop  (plist-get (cl-second split-time) :stop)))
;;                   (backward-list 1)
;;                   (chronometrist-sexp-delete-list)
;;                   (-> expr
;;                       (plist-put :start first-start)
;;                       (plist-put :stop  first-stop)
;;                       (chronometrist-plist-pp buffer))
;;                   (when (looking-at-p "\n\n")
;;                     (delete-char 2))
;;                   (-> expr
;;                       (plist-put :start second-start)
;;                       (plist-put :stop  second-stop)
;;                       (chronometrist-plist-pp buffer))
;;                   (setq modified t))))))
;;         (save-buffer)))
;;     modified))

(defun chronometrist-events-maybe-split (event)
  "Split EVENT if it spans midnight.

Return a list of two events if EVENT was split, else nil."
  (when (plist-get event :stop)
    (let ((split-time (chronometrist-midnight-spanning-p (plist-get event :start)
                                             (plist-get event :stop))))
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

;; TODO - Maybe strip dates from values, since they're part of the key
;; anyway. Consider using a state machine.

;; OPTIMIZE - It should not be necessary to call this unless the file
;; has changed. Any other refresh situations should not require this.
(defun chronometrist-events-populate ()
  "Clear hash table `chronometrist-events' (which see) and populate it.
The data is acquired from `chronometrist-file'.

Return final number of events read from file, or nil if there
were none."
  (clrhash chronometrist-events)
  (chronometrist-sexp-events-populate))

(defun chronometrist-tasks-from-table ()
  "Return a list of task names from `chronometrist-events'."
  (cl-loop for plists being the hash-values of chronometrist-events append
    (cl-loop for plist in plists collect (plist-get plist :name)) into list
    finally
    (cl-return
     (cl-remove-duplicates (sort list #'string-lessp) :test #'equal))))

(defun chronometrist-events-add (plist)
  "Add new PLIST at the end of `chronometrist-events'."
  (let* ((date-today   (format-time-string "%Y-%m-%d"))
         (events-today (gethash date-today chronometrist-events)))
    (--> (list plist)
         (append events-today it)
         (puthash date-today it chronometrist-events))))

(defun chronometrist-events-replace-last (plist)
  "Replace the last plist in `chronometrist-events' with PLIST."
  (let* ((date-today   (format-time-string "%Y-%m-%d"))
         (events-today (gethash date-today chronometrist-events)))
    (--> (reverse events-today)
         (cdr it)
         (append (list plist) it)
         (reverse it)
         (puthash date-today it chronometrist-events))))

;; to be replaced by plist-query
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
               (when (ts-in start end (chronometrist-iso-date->ts key))
                 (puthash key value subset)))
             chronometrist-events)
    subset))

(provide 'chronometrist-events)

;;; chronometrist-events.el ends here
