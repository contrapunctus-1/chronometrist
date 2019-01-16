(require 'seq)

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
                     (paths (chronometrist-assist-atom->list (plist-get plist :path)))
                     (mode-name (symbol-name major-mode))
                     (mode-matches (when modes
                                     (->> modes
                                          (--map (string-match-p it mode-name))
                                          (seq-some #'identity)
                                          (chronometrist-assist-atom->list))))
                     (path-matches (when (and (buffer-file-name) paths)
                                     (->> paths
                                          (--map (string-match-p it (buffer-file-name)))
                                          (seq-some #'identity)
                                          (chronometrist-assist-atom->list)))))
                (when (seq-every-p #'identity
                                   (append mode-matches
                                           path-matches))
                  ;; (message "Matched rule %s%s%s" entry
                  ;;          " for buffer " (buffer-name)
                  ;;          " with mode " mode-name)
                  (throw 'got-project project))))
            chronometrist-project-list)
    nil))

(defun chronometrist-assist ()
  "Assist the user in time tracking, by either clocking in
automatically or suggesting doing so (see Custom variable
`chronometrist-assist').

This function is added to `first-change-hook'."
  (if chronometrist-project-list
      (let ((project (chronometrist-assist-match-project)))
        (when (and project
                   (not (chronometrist-current-project)))
          (case chronometrist-assist
            ('auto
             (timeclock-in nil project nil))
            ('suggest
             (when (yes-or-no-p (concat "Clock into \"" project "\"?"
                                        " (" (buffer-name) ")"))
               (timeclock-in nil project nil))))))
    (message "To use chronometrist-assist, please define some projects in `chronometrist-project-list'.")))

(provide 'chronometrist-assist)

;; Local Variables:
;; nameless-current-name: "chronometrist-assist"
;; End:
