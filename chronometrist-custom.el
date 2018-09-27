(defgroup chronometrist nil
  "A time tracker with a nice UI.")

(defcustom chronometrist-buffer-name "*Chronometrist*"
  "The name of the buffer created by `chronometrist'.")

(defcustom chronometrist-hide-cursor nil
  "If non-nil, hide the cursor and only highlight the current line in the `chronometrist' buffer.")

(defcustom chronometrist-update-interval 5
  "How often the `chronometrist' buffer should be updated, in seconds.

This is not guaranteed to be accurate - see (info \"(elisp)Timers\").")

(defvar chronometrist-project-start-hook nil
  "Hook run before a project is clocked in. Each function in this hook must accept a single argument, which is the project to be clocked-in.

The commands `chronometrist-toggle-project-button',
`chronometrist-add-new-project-button',
`chronometrist-toggle-project',
`chronometrist-add-new-project', and
`chronometrist-toggle-project-no-reason' will run this hook.")

(defvar chronometrist-project-end-hook nil
  "Hook run after a project is clocked out. Each function in this hook must accept a single argument, which is the clocked-out project.")

(provide 'chronometrist-custom)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:
