(defun chronometrist-assist-match-project ()
  "Check if the current buffer matches a project defined in
`chronometrist-project-list'. Returns the project name, or nil if
there was no match."
  (catch 'got-project
    (mapcar (lambda (entry)
              (let* ((project (car entry))
                     (plist (cdr entry))
                     (mode (plist-get plist :mode))
                     (path (plist-get plist :path)))
                (when mode
                  (mapcar (lambda (elt)
                            (when (string-match-p elt (symbol-name major-mode))
                              (throw 'got-project project)))
                          (if (consp mode) mode (list mode))))
                (when path
                  (mapcar (lambda (elt)
                            (when (string-match-p elt (buffer-file-name))
                              (throw 'got-project project)))
                          (if (consp path) path (list path))))))
            chronometrist-project-list)
    nil))

;; Local Variables:
;; nameless-current-name: "chronometrist-assist"
;; End:
