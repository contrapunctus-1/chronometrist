;;; chronometrist-history.el --- input history management for Chronometrist

;;; Commentary:
;;

;;; Code:

(defvar chronometrist-key-history (make-hash-table :test #'equal))

;; Currently, this doesn't use `chronometrist-events', because doing that would
;; count keys in split-at-midnight events twice.
(defun chronometrist-key-history-populate ()
  "Clear hash table `chronometrist-key-history' and populate it.

The data is acquired from `chronometrist-file'.

Each hash table key is the name of a task. Each hash table value
is a list in the form ((KEY-1 . N-1) (KEY-2 . N-2) ...), where N-1
is the number of times KEY-1 was used for the task."
  (clrhash chronometrist-key-history)
  (--map (puthash it nil chronometrist-key-history)
         ;; ;; Not necessary, if the only placed this is called is `chronometrist-refresh-file'
         ;; (setq chronometrist--task-list (chronometrist-tasks-from-table))
         chronometrist--task-list)
  (with-current-buffer (find-file-noselect chronometrist-file)
    (save-excursion
      (goto-char (point-min))
      (let (expr)
        (while (setq expr (ignore-errors (read (current-buffer))))
          (let* ((name          (plist-get expr :name))
                 (name-ht-value (gethash name chronometrist-key-history))
                 (keys          (->> (chronometrist-plist-remove expr :name :start :stop :tags)
                                     (seq-filter #'keywordp))))
            (mapc (lambda (key)
                    (let ((old-key (seq-find (lambda (pair)
                                               (equal (car pair) key))
                                             name-ht-value)))
                      (puthash name
                               (if old-key
                                   (append `(,(cons key (incf (cdr old-key))))
                                           (seq-remove (lambda (pair)
                                                         (equal (car pair) key))
                                                       name-ht-value))
                                 (append `(,(cons key 1)) name-ht-value))
                               chronometrist-key-history)))
                  keys)))))))

(defun chronometrist-key-history-for-task (task)
  (->> (gethash task chronometrist-key-history)
       (seq-sort (lambda (pair-1 pair-2)
                   (if (> (cdr pair-1) (cdr pair-2))
                       t nil)))
       (mapcar (lambda (pair)
                 (->> (car pair)
                      (symbol-name)
                      (s-chop-prefix ":"))))))

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

(provide 'chronometrist-history)

;;; chronometrist-history.el ends here
