(defun chronometrist-assist-atom->list (arg)
  (when arg
    (if (consp arg)
        arg
      (list arg))))

(defun chronometrist-assist-match-project ()
  "Check if the current buffer matches a project defined in
`chronometrist-project-list'. Return the project name, or nil if
there was no match."
  (catch 'got-project
    (mapcar (lambda (entry)
              (let* ((project (car entry))
                     (plist (cdr entry))
                     (modes (chronometrist-assist-atom->list (plist-get plist :mode)))
                     (paths (chronometrist-assist-atom->list (plist-get plist :path))))
                (when modes
                  (mapcar (lambda (elt)
                            (when (string-match-p elt (symbol-name major-mode))
                              (throw 'got-project project)))
                          modes))
                (when paths
                  (mapcar (lambda (elt)
                            (when (string-match-p elt (buffer-file-name))
                              (throw 'got-project project)))
                          paths))))
            chronometrist-project-list)
    nil))

(defun chronometrist-assist (start end)
  "Assist the user in time tracking, by either clocking in
automatically or suggesting doing so (see Custom variable
`chronometrist-assist').

This function is added to `before-change-functions'."
  (if chronometrist-project-list
      (let ((project (chronometrist-assist-match-project)))
        (when project
          (case chronometrist-assist
            ('auto
             (timeclock-in nil project nil))
            ('suggest
             (when (yes-or-no-p (concat "Clock into \"" project "\"?"))
               (timeclock-in nil project nil))))))
    (message "To use chronometrist-assist, please define some projects in `chronometrist-project-list'.")))

;; Local Variables:
;; nameless-current-name: "chronometrist-assist"
;; End:
