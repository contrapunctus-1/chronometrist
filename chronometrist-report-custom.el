;;; chronometrist-report-custom --- custom definitions for `chronometrist-report' -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>

;;; Commentary:
;;

;;; Code:

(defgroup chronometrist-report nil
  "Weekly report for the `chronometrist' time tracker.")

(defcustom chronometrist-report-buffer-name "*Chronometrist-Report*"
  "The name of the buffer created by `chronometrist-report'.")

(defcustom chronometrist-report-week-start-day "Sunday"
  "The day used for start of week by `chronometrist-report'.")

(defcustom chronometrist-report-weekday-number-alist
  '(("Sunday"    . 0)
    ("Monday"    . 1)
    ("Tuesday"   . 2)
    ("Wednesday" . 3)
    ("Thursday"  . 4)
    ("Friday"    . 5)
    ("Saturday"  . 6))
  "Alist in the form (\"NAME\" . NUMBER), where \"NAME\" is the name of a weekday and NUMBER its associated number.")

;; Local Variables:
;; nameless-current-name: "chronometrist-report"
;; End:

(provide 'chronometrist-report-custom)

;;; chronometrist-report-custom.el ends here
