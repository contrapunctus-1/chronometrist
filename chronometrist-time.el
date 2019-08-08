;;; chronometrist-time.el --- Time and date functions for Chronometrist

(require 'dash)

;;; Commentary:
;;

;;; Code:

(defun chronometrist-date (&optional date)
  "Return DATE or today in the form (YEAR MONTH DAY).

DATE can be
calendrical information returned by `decode-time'
\(YEAR MONTH DAY), in which case the same is returned, or
nil, in which case today's date is returned."
  (case (length date)
    (3 date)
    (t (cl-destructuring-bind (_ _ _ day month year _ _ _)
           (if date date (decode-time))
         (list year month day)))))

(defun chronometrist-time ())

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

(provide 'chronometrist-time)

;;; chronometrist-time.el ends here
