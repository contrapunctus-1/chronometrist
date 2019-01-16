(defvar chronometrist-events (make-hash-table :test #'equal))

(defun chronometrist-vfirst (vector)
  "Return the first element of VECTOR."
  (elt vector 0))

(defun chronometrist-vlast (vector)
  "Return the last element of VECTOR."
  (elt vector (1- (length vector))))

;; test function
(defun chronometrist-list-midnight-spanning-events ()
  (let ((dates))
    (maphash (lambda (key value)
               (when (-> value (chronometrist-vfirst) (chronometrist-vfirst) (equal "o"))
                 (->> key (list) (append dates) (setq dates))))
             chronometrist-events)
    dates))

(defun chronometrist-events-clean ()
  "Clean `chronometrist-events' by splitting intervals which span
midnights into two. For event data to be processed accurately,
this must be called after `chronometrist-populate'. Returns t
if the table was modified, else nil."
  (let* ((latest-date (-> chronometrist-events
                          (hash-table-keys)
                          (car)))
         (latest-date-events (gethash latest-date chronometrist-events)))
    ;; If the most recent event isn't an "o" event, add one with the current time
    (when (equal "i"
                 (-> latest-date-events
                     (chronometrist-vlast)
                     (chronometrist-vfirst)))
      (cl-destructuring-bind (s m h day month year _ _ _)
          (decode-time)
        (let* ((new-date   `(,year ,month ,day))
               (temp-event `[["o" ,year ,month ,day ,h ,m ,s]])
               (new-date-events (gethash new-date chronometrist-events)))
          (if new-date-events
              (vconcat new-date-events temp-event)
            (puthash new-date temp-event chronometrist-events))))))
  ;; for each key-value, see if the first event has an "o" code
  (let (prev-date modified)
    (maphash (lambda (key value)
               (when (-> value (chronometrist-vfirst) (chronometrist-vfirst) (equal "o"))
                 ;; Add new "o" event on previous date with 24:00:00
                 ;; as end time, reusing the ending reason.
                 ;; Add new "i" event on current date with 00:00:00
                 ;; as start time, with the same project.
                 (let* ((reason  (->> value (chronometrist-vfirst) (chronometrist-vlast)))
                        (prev-events (gethash prev-date chronometrist-events))
                        (prev-event  (chronometrist-vlast prev-events))
                        (o-event     (vconcat ["o"] prev-date `[24 0 0 ,reason]))

                        (current-event (chronometrist-vfirst value))
                        (project       (chronometrist-vlast prev-event))
                        (i-event       (vconcat ["i"] key `[0 0 0 ,project])))
                   (--> prev-events
                        (vconcat it (vector o-event))
                        (puthash prev-date it chronometrist-events))
                   (--> (vconcat (vector i-event) value)
                        (puthash key it chronometrist-events))
                   (setq modified t)))
               (setq prev-date key))    ; this assumes that the first event of the first date doesn't
                                        ; have an "o" code (which a correct file shouldn't)
             chronometrist-events)
    modified))

;; TODO - Maybe strip dates from values, since they're part of the key
;; anyway. Consider using a state machine.
(defun chronometrist-events-populate ()
  "Clears hash table `chronometrist-events' and populates it
using data from `timeclock-file', with each key being a date in
the form (YEAR MONTH DAY). Values are vectors containing events,
where each event is a vector in the form \[CODE YEAR MONTH DAY
HOURS MINUTES SECONDS \"PROJECT-NAME-OR-COMMENT\"].

This function always returns nil."
  (clrhash chronometrist-events)
  (with-current-buffer (find-file-noselect timeclock-file)
    (save-excursion
      (goto-char (point-min))
      (let ((events))
        (while (not (= (point) (point-max)))
          (let* ((event-string       (buffer-substring-no-properties (point-at-bol)
                                                                     (point-at-eol)))
                 (info-re            (concat ". " chronometrist-date-re " " chronometrist-time-re-file))
                 (project-or-comment (->> event-string
                                          (replace-regexp-in-string (concat info-re " ?") "")
                                          (vector)))
                 (the-rest           (--> (concat "\\(" info-re "\\)" ".*")
                                          (replace-regexp-in-string it "\\1" event-string)
                                          (split-string it "[ /:]")
                                          (append (list (car it))
                                                  (mapcar #'string-to-number (-slice it 1 7)))))
                 (key                (-slice the-rest 1 4))
                 (old-value          (gethash key chronometrist-events))
                 (new-value          (vector (vconcat the-rest ;; vconcat converts lists to vectors
                                                      project-or-comment))))
            (if old-value
                (puthash key (vconcat old-value new-value) chronometrist-events)
              (puthash key new-value chronometrist-events)))
          (forward-line))
        nil))))

(defun chronometrist-events-subset (start-date end-date)
  "Return a subset of `chronometrist-events', containing values
between START-DATE and END-DATE (both inclusive). START-DATE and
END-DATE must be dates in the form '(YEAR MONTH DAY)."
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
