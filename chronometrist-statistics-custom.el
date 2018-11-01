(defgroup chronometrist-statistics nil
  "Statistics buffer for the `chronometrist' time tracker.")

(defcustom chronometrist-statistics-buffer-name "*Chronometrist-Statistics*"
  "The name of the buffer created by `chronometrist-statistics'.")

(defcustom chronometrist-statistics-update-interval 10
  "How often the `chronometrist-statistics' buffer should be updated, in seconds.

This is not guaranteed to be accurate - see (info \"(elisp)Timers\").")

(provide 'chronometrist-statistics-custom)

;; Local Variables:
;; nameless-current-name: "chronometrist-statistics"
;; End:
