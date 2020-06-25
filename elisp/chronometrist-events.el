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
  (let (acc)
    (maphash (lambda (_key value)
               (mapc (lambda (event)
                       (setq acc (append acc `(,(plist-get event :name)))))
                     value))
             chronometrist-events)
    (cl-remove-duplicates (sort acc #'string-lessp)
                          :test #'equal)))

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

(defun chronometrist-events-query-spec-match-p (plist specifiers)
  "Return non-nil if SPECIFIERS match PLIST."
  (let* ((spec-only-keywords-p (when specifiers
                                 (seq-every-p #'keywordp specifiers)))
         (keyword-list         (unless spec-only-keywords-p
                                 (seq-filter #'keywordp specifiers))))
    ;; When all keys from SPECIFIERS are present...
    (cond (spec-only-keywords-p
           (->> (cl-loop for key in specifiers
                         collect (plist-member plist key))
                (seq-every-p #'identity)))
          ;; ...or SPECIFIERS has no keywords...
          ((not keyword-list) t)
          ;; ...or all key-values from SPECIFIERS match...
          (t (->> keyword-list
                  (mapcar (lambda (keyword)
                            (equal (plist-get plist keyword)
                                   (plist-get specifiers keyword))))
                  (-all-p #'identity))))))

;; examples -
;; (chronometrist-events-query chronometrist-events :get :name) - get all values for :name

;; TODO - values for SPECIFIERS and EXCEPT are AND'ed. No way to express an OR.
;; TODO - same goes for SPECIFIERS and EXCEPT themselves. No way to combine them.
;; TODO - an argument to SPECIFIERS and EXCEPT cannot be a combination of keyword-values and keywords (e.g. '(:a 1 :b) - where :a is 1 and :b is present)
;;   - Can we add support for arbitrary predicates as values? As a
;;     bonus, arguments like '(:a 1 :b) could be internally converted
;;     into '(:a 1 :b #'identity). Distinguishing between cases where
;;     :b actually has a value called #'identity would be the problem,
;;     but that seems highly unlikely in our use-case.
(cl-defun chronometrist-events-query (table &key get specifiers except)
  "Query the `chronometrist-events' hash table.

GET can be -
nil - return a list of plists
a keyword - return a list of values
a list of keywords - return a list of plists which contain only these keywords and their values

SPECIFIERS can be -
nil - to return any entry
a plist -  to return plists matching all given key-value pairs.
a list of keywords - to return plists which contain these keywords.

EXCEPT takes any values SPECIFIERS does. The plists matched by
EXCEPT will be excluded from the return value."
  (let* ((length-get (when (listp get) (length get)))
         return)
    (maphash (lambda (_key value-plists)
               (mapc (lambda (plist)
                       (when (and (chronometrist-events-query-spec-match-p plist specifiers)
                                  (if except
                                      (not (chronometrist-events-query-spec-match-p plist except))
                                    t))
                         ;; ...Store the values specified by GET.
                         (->> (cond ((keywordp get)
                                     `(,(plist-get plist get)))
                                    ;; (listp nil) => t, so we use consp
                                    ((consp get)
                                     (let ((count 0) acc)
                                       (mapc (lambda (get-key)
                                               (if (= count length-get)
                                                   acc
                                                 (->> `(,get-key ,(plist-get plist get-key))
                                                      (append acc)
                                                      (setq acc))
                                                 (cl-incf count)))
                                             get)
                                       (list acc)))
                                    (t (list plist)))
                              (append return)
                              (setq return))))
                     value-plists))
             table)
    (seq-remove #'null return)))

(provide 'chronometrist-events)

;; Local Variables:
;; nameless-current-name: "chronometrist-events"
;; End:

;;; chronometrist-events.el ends here
