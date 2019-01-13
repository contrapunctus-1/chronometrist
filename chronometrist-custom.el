(defgroup chronometrist nil
  "A time tracker with a nice UI.")

(defcustom chronometrist-buffer-name "*Chronometrist*"
  "The name of the buffer created by `chronometrist'.")

(defcustom chronometrist-hide-cursor nil
  "If non-nil, hide the cursor and only highlight the current line in the `chronometrist' buffer.")

(defcustom chronometrist-update-interval 5
  "How often the `chronometrist' buffer should be updated, in seconds.

This is not guaranteed to be accurate - see (info \"(elisp)Timers\").")

(defcustom chronometrist-assist nil
  "Whether Chronometrist should assist you in tracking time. Possible values -
nil - disable assistance
suggest - offer to clock in/out based on your definitions in `chronometrist-projects-list'
auto - clock in/out automatically based on your definitions in `chronometrist-projects-list'")

(defcustom chronometrist-projects-list nil
  "List for specifying Chronometrist projects and their properties.

Each element must be in the form (\"PROJECT\" :PATH ... :MODE ... :REASON ...)

\"PROJECT\" is the project's name. Keyword arguments are optional.

:PATH and :MODE can be either a regular expression or a list of
regular expressions. Values for :PATH are matched against a file
path - working on files whose paths match against an expression
will be considered working on \"PROJECT\". Similarly, values
for :MODE are matched against buffer major modes.

:REASON can have the following values -
t - (default) always ask for a reason,
\"reason\" - use as initial contents when asking,
nil - never ask, always leave blank.")

(provide 'chronometrist-custom)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:
