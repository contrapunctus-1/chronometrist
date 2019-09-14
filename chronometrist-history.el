;;; chronometrist-history.el --- input history management for Chronometrist

;;; Commentary:
;;

;;; Code:

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
