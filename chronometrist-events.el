;;; chronometrist-events.el --- Event management and querying code for Chronometrist -*- lexical-binding: t; -*-

(require 'plist-pp)
(require 'subr-x)

;;; Commentary:
;;

;;; Code:

(defvar chronometrist-events (make-hash-table :test #'equal))

;; As we're no longer dealing with vectors, these are deprecated, and
;; will be removed once the rest of the codebase is migrated.
(defun chronometrist-vfirst (vector)
  "Return the first element of VECTOR."
  (elt vector 0))

(defun chronometrist-vlast (vector)
  "Return the last element of VECTOR."
  (elt vector (1- (length vector))))

(defun chronometrist-list-midnight-spanning-events ()
  "Test function to check for events which span midnights."
  (let ((dates))
    (maphash (lambda (key value)
               (when (-> value (chronometrist-vfirst) (chronometrist-vfirst) (equal "o"))
                 (->> key (list) (append dates) (setq dates))))
             chronometrist-events)
    dates))

(defun chronometrist-day-start (timestamp)
  "Get start of day (according to `chronometrist-day-start-time') for TIMESTAMP.

TIMESTAMP must be a time string in the ISO-8601 format.

Return value is a time value (see `current-time')."
  (let* ((timestamp-date (->> timestamp
                             (parse-iso8601-time-string)
                             (decode-time)
                             (-drop 3)
                             (-take 3))))
    (--> chronometrist-day-start-time
         (split-string it ":")
         (mapcar #'string-to-number it)
         (reverse it)
         (append it timestamp-date)
         (apply #'encode-time it))))

(defun chronometrist-file-clean ()
  "Clean `chronometrist-file' so that events can be processed accurately.

This function splits midnight-spanning intervals into two. It
must be called before running `chronometrist-populate'.

It returns t if the table was modified, else nil."
  (let ((buffer (find-file-noselect chronometrist-file))
        modified
        expr)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (setq expr (ignore-errors (read (current-buffer))))
          (when (plist-get expr :stop)
            (let ((split-time (chronometrist-events-midnight-spanning-p (plist-get expr :start)
                                                    (plist-get expr :stop))))
              (when split-time
                (let ((first-start  (plist-get (first  split-time) :start))
                      (first-stop   (plist-get (first  split-time) :stop))
                      (second-start (plist-get (second split-time) :start))
                      (second-stop  (plist-get (second split-time) :stop)))
                  (backward-list 1)
                  (chronometrist-delete-list)
                  (-> expr
                      (plist-put :start first-start)
                      (plist-put :stop  first-stop)
                      (plist-pp buffer))
                  (when (looking-at-p "\n\n")
                    (delete-char 2))
                  (-> expr
                      (plist-put :start second-start)
                      (plist-put :stop  second-stop)
                      (plist-pp buffer))
                  (setq modified t))))))
        (save-buffer)))
    modified))

(defun chronometrist-events-maybe-split (event)
  "Split EVENT if it spans midnight.

Return a list of two events if EVENT was split, else nil."
  (when (plist-get event :stop)
    (let ((split-time (chronometrist-events-midnight-spanning-p (plist-get event :start)
                                            (plist-get event :stop))))
      (when split-time
        (let ((first-start  (plist-get (first  split-time) :start))
              (first-stop   (plist-get (first  split-time) :stop))
              (second-start (plist-get (second split-time) :start))
              (second-stop  (plist-get (second split-time) :stop))
              ;; plist-put modifies lists in-place. The resulting bugs
              ;; left me puzzled for a while.
              (event-1      (copy-list event))
              (event-2      (copy-list event)))
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
  "Clear hash table `chronometrist-events' and populate it.

The data is acquired from `chronometrist-file'.

Each key is a date in the form (YEAR MONTH DAY).

Values are vectors containing events, where each event is a
vector in the form \[\"CODE\" YEAR MONTH DAY HOURS MINUTES
SECONDS \"PROJECT-NAME-OR-COMMENT\"\].

Return final number of events in the hash table, or nil if there
are none."
  (clrhash chronometrist-events)
  (with-current-buffer (find-file-noselect chronometrist-file)
    (save-excursion
      (goto-char (point-min))
      (let ((index 0) expr split-expr-leftover)
        (while (setq expr (ignore-errors (read (current-buffer))))
          ;; find and split midnight-spanning events during deserialization itself
          (let ((split-expr (chronometrist-events-maybe-split expr)))
            (incf index)
            (cond (split-expr-leftover
                   (puthash index split-expr-leftover chronometrist-events)
                   (setq split-expr-leftover nil))
                  (split-expr
                   (let* ((expr-1 (first  split-expr))
                          (expr-2 (second split-expr)))
                     (puthash index expr-1 chronometrist-events)
                     (setq split-expr-leftover expr-2)))
                  (t (puthash index expr chronometrist-events)))))
        (unless (zerop index) index)))))

(defun chronometrist-tasks-from-table ()
  "Return a list of task names from `chronometrist-events'."
  (let (acc)
    (maphash (lambda (key val)
               (setq acc (append acc `(,(plist-get val :name)))))
             chronometrist-events)
    (remove-duplicates (sort acc #'string-lessp)
                       :test #'equal)))

;; to be replaced by plist-query
(defun chronometrist-events-subset (start-date end-date)
  "Return a subset of `chronometrist-events'.

The subset will contain values between START-DATE and
END-DATE (both inclusive).

START-DATE and END-DATE must be dates in the form '(YEAR MONTH DAY)."
  (let ((subset (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (when (and (not (chronometrist-date-less-p key start-date))
                          (not (chronometrist-date-less-p end-date key)))
                 (puthash key value subset)))
             chronometrist-events)
    subset))

(provide 'chronometrist-events)

;; Local Variables:
;; nameless-current-name: "chronometrist-events"
;; End:

(provide 'chronometrist-events)

;;; chronometrist-events.el ends here
