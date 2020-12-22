;;; chronometrist-sqlite3.el --- sqlite3 backend for Chronometrist

;;; Commentary:
;;

(require 'anaphora)
(require 'emacsql-sqlite3)
(require 'chronometrist-backend)

(defclass chronometrist-sqlite3 (chronometrist-backend) nil)
(defvar chronometrist-sqlite3-backend (make-instance chronometrist-sqlite3 :name "sqlite3" :ext "sqlite3"))
(defvar chronometrist-sqlite3-db (chronometrist-backend-create-file chronometrist-sqlite3-backend))

;; # Migration #
(cl-defmethod chronometrist-backend-to-hash ((backend chronometrist-sqlite3) table))

(defun chronometrist-sqlite3-insert-plist (plist db)
  "Insert PLIST into DB."
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
      (let* ((pragma        (emacsql db [:pragma (funcall table_info events)]))
             (column-exists (cl-loop for column-spec in pragma thereis
                              (eq column (second column-spec)))))
        (unless column-exists
          (emacsql db [:alter-table events :add-column $i1] column))))
    (emacsql db [:insert-into events [$i1] :values $v2]
             (vconcat columns) (vconcat values))))

(cl-defmethod chronometrist-backend-from-hash ((backend chronometrist-sqlite3) file)
  (cl-loop with db = (emacsql-sqlite3
                      (concat file "." (oref chronometrist-sqlite3-backend :ext)))
    with count = 0
    for events being the hash-values of table do
    (cl-loop for event in events do
      (chronometrist-sqlite3-insert-plist event db)
      (incf count)
      (when (zerop (% count 5))
        (message "chronometrist-migrate-migrate - %s events converted" count)))
    finally return count do
    (message "chronometrist-migrate - finished converting %s events." count)))

;; # Queries #
(cl-defmethod chronometrist-backend-open-file ((backend chronometrist-sqlite3) file)
  (require 'sql)
  (switch-to-buffer
   (sql-comint-sqlite 'sqlite (list file))))

;; SELECT * FROM TABLE WHERE ID = (SELECT MAX(ID) FROM TABLE);
;; SELECT * FROM tablename ORDER BY column DESC LIMIT 1;
(cl-defmethod chronometrist-backend-latest-record ((backend chronometrist-sqlite3) db)
  (emacsql db [:select * :from events :order-by rowid :desc :limit 1]))

(cl-defmethod chronometrist-backend-current-task ((backend chronometrist-sqlite3)))

(cl-defmethod chronometrist-backend-intervals (task &optional (ts (ts-now)))
  "Return the time intervals for TASK on TS.")

(cl-defmethod chronometrist-backend-task-time (task &optional (ts (ts-now))))

(cl-defmethod chronometrist-backend-active-time (&optional ts))

(cl-defmethod chronometrist-backend-active-days (task))

;; # Modifications #
(cl-defmethod chronometrist-backend-create-file ((backend chronometrist-sqlite3))
  "Create file for BACKEND if it does not already exist.
Return the emacsql-sqlite3 connection object."
  (aprog1 (emacsql-sqlite3 (concat chronometrist-file "." (oref backend :ext)))
    (emacsql it [:create-table events ([name tags start stop])])))

(cl-defmethod chronometrist-backend-new-record ((backend chronometrist-sqlite3) plist)
  (chronometrist-sqlite3-insert-plist plist file))

(cl-defmethod chronometrist-backend-replace-last ((backend chronometrist-sqlite3) plist)
  (emacsql db [:delete-from events :where ]))

(provide 'chronometrist-sqlite3)

;;; chronometrist-sqlite3.el ends here
