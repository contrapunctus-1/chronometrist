;;; chronometrist-sqlite3.el --- sqlite3 backend for Chronometrist

;;; Commentary:
;;

(require 'emacsql-sqlite3)

(require 'chronometrist-backend)

(defclass chronometrist-sqlite3 (chronometrist-backend) nil)
(defvar chronometrist-sqlite3-backend (make-instance chronometrist-sqlite3 :name "sqlite3" :ext "sqlite3"))
(defvar chronometrist-sqlite3-db (emacsql-sqlite3
             (concat chronometrist-file "." (oref chronometrist-sqlite3-backend :ext))))

;; # Migration #
(cl-defmethod chronometrist-backend-to-hash ((backend chronometrist-sqlite3) table))

(defun chronometrist-sqlite3-insert-plist (plist db)
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

(cl-defmethod chronometrist-backend-from-hash ((backend chronometrist-sqlite3) table)
  (cl-loop with db = (emacsql-sqlite3
                      (concat chronometrist-file "migrate." (oref chronometrist-sqlite3-backend :ext)))
    with count = 0
    for events being the hash-values of table do
    (cl-loop for event in events do
      (chronometrist-sqlite3-insert-plist event db)
      (incf count)
      (when (zerop (% count 5))
        (message "chronometrist-migrate-migrate - %s events converted" count)))
    finally do
    (message "chronometrist-migrate - finished converting %s events." count)))

;; # Queries #
(cl-defmethod chronometrist-backend-open-file ((backend chronometrist-sqlite3))
  (require 'sql)
  (switch-to-buffer
   (sql-comint-sqlite 'sqlite `(,(chronometrist-file-path)))))

(cl-defmethod chronometrist-backend-latest-record ((backend chronometrist-sqlite3)))

(cl-defmethod chronometrist-backend-current-task ((backend chronometrist-sqlite3)))

;; # Modifications #
(cl-defmethod chronometrist-backend-create-file ((backend chronometrist-sqlite3)))

(cl-defmethod chronometrist-backend-new-record ((backend chronometrist-sqlite3) plist file)
  (chronometrist-sqlite3-insert-plist plist file))

(cl-defmethod chronometrist-backend-replace-last ((backend chronometrist-sqlite3) plist file))

(provide 'chronometrist-sqlite3)

;;; chronometrist-sqlite3.el ends here
