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

;; Local Variables:
;; nameless-current-name: "chronometrist-assist"
;; End:
