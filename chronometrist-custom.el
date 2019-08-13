;;  -*- lexical-binding: t; -*-

(defgroup chronometrist nil
  "A time tracker with a nice UI.")

(defcustom chronometrist-buffer-name "*Chronometrist*"
  "The name of the buffer created by `chronometrist'.")

(defcustom chronometrist-hide-cursor nil
  "If non-nil, hide the cursor and only highlight the current line in the `chronometrist' buffer.")

(defcustom chronometrist-update-interval 5
  "How often the `chronometrist' buffer should be updated, in seconds.

This is not guaranteed to be accurate - see (info \"(elisp)Timers\").")

(provide 'chronometrist-custom)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:
