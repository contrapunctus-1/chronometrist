(defvar plt-key-comparisons-alist nil)

(defmacro plt-query (&rest body)
  "Query a hash table of plists using an SQL-like language.

GET <return value specifier> WHERE <plist specifier expression> ..."
  )

;; Leave defined comparison functions/pattern matching out for the
;; moment - just use `equal' and focus on the basics first.
(cl-defun plt-query-function (table &key get specifiers)
  (let ((keyword-list (seq-filter #'keywordp specifiers))
        (return))
    (maphash (lambda (key value)
               (when (->> keyword-list
                          (mapcar (lambda (keyword)
                                    ;; (alist-get arg key-predicate-alist)
                                    ;; <see if value passes the predicate>
                                    (equal (plist-get value keyword)
                                           (plist-get specifiers keyword))))
                          (-all-p #'identity))
                 (nconc return
                        (cond ((keywordp get)
                               (list
                                (plist-get value get)))
                              ;; (listp nil) => t, so we use consp
                              ((consp get)
                               (--map (plist-get value it)
                                      get))
                              (t value)))))
             table)
    (seq-remove #'null return)))

(defvar test-table (make-hash-table))

;; test function
(defun plt-populate-table ()
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
                                               (and (= 4 digit) "/" (= 2 digit) "/" (= 2 digit) " ")
                                               (and (= 2 digit) ":" (= 2 digit) ":" (= 2 digit))
                                               (opt " ")))
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
