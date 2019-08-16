(chronometrist-query table)
(maphash (lambda (key value)
           (when (->> arg-list
                      (mapcar (lambda (arg)
                                (let ((value <get value from pair>))
                                  (alist-get arg key-predicate-alist)
                                  <see if value passes the predicate>)))
                      (-all-p #'identity))
             (cond return
                   ((symbolp return)
                    )
                   (())
                   (t value))))
         hash-table)

(defvar test-table (make-hash-table))

(defun chronometrist-events-populate-plists ()
  "Read data from `timeclock-file' to `test-table', storing events as plists."
  (clrhash test-table)
  (with-current-buffer (find-file-noselect timeclock-file)
    (save-excursion
      (goto-char (point-min))
      (let ((key-counter 0))
        (while (not (= (point) (point-max)))
          (let* ((event-string       (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                 (event-list         (split-string event-string "[ /:]"))
                 (code               (first event-list))
                 (date               (--> event-list
                                          (seq-drop it 1)
                                          (seq-take it 3)
                                          (mapcar #'string-to-number it)))
                 (time               (--> event-list
                                          (seq-drop it 4)
                                          (seq-take it 3)
                                          (mapcar #'string-to-number it)))
                 (project-or-comment (replace-regexp-in-string
                                      (rx (and (or "i" "o") " "
                                               (= 4 digit) "/" (= 2 digit) "/" (= 2 digit) " "
                                               (= 2 digit) ":" (= 2 digit) ":" (= 2 digit) " "))
                                      ""
                                      event-string)))
            (incf key-counter)
            (puthash key-counter
                     (append `(:code ,code :date ,date :time ,time)
                             (when (equal code "i")
                               `(:project ,project-or-comment))
                             (when (and (equal code "o")
                                        (stringp project-or-comment)
                                        (not
                                         (string= project-or-comment "")))
                               `(:comment ,project-or-comment)))
                     test-table))
          (forward-line)
          (goto-char (point-at-bol))))
      nil)))
