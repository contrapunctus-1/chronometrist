;;; chronometrist-sexp.el --- S-expression backend for Chronometrist

(require 'chronometrist-migrate)

;;; Commentary:
;;

(require 'dash)
(require 'seq)

;;; Code:

(defvar chronometrist-file "~/.emacs.d/chronometrist.sexp"
  "Default path and name of chronometrist database.")

(defun chronometrist-plist-remove (plist &rest keys)
  "Return PLIST with KEYS and their associated values removed."
  (let ((keys (--filter (plist-member plist it) keys)))
    (mapc (lambda (key)
            (let ((pos (seq-position plist key)))
              (setq plist (append (seq-take plist pos)
                                  (seq-drop plist (+ 2 pos))))))
          keys)
    plist))

(defun chronometrist-delete-list (&optional arg)
  "Delete ARG lists after point."
  (let ((point-1 (point)))
    (forward-sexp (or arg 1))
    (delete-region point-1 (point))))

(defun chronometrist-maybe-string-to-symbol (list)
  "For each string in LIST, if it has no spaces, convert it to a symbol."
  (--map (unless (string-match-p "[[:space:]]" it)
           (make-symbol it))
         list))

(defun chronometrist-in (task &optional tags plist)
  (interactive `(,(completing-read "Task name: "
                                   (chronometrist-tasks-from-table)
                                   nil 'confirm nil
                                   ;; TODO - implement history
                                   nil)
                 ,(completing-read-multiple "Tags (optional): "
                                            ;; FIXME - use tags, not tasks
                                            (chronometrist-tasks-from-table)
                                            nil 'confirm nil 'history)))
  "Add new time interval as an s-expression to `chronometrist-file'.

TASK is the name of the task, a string.

PLIST is a property list containing any other information about
this time interval that should be recorded."
  (let ((buffer (find-file-noselect chronometrist-file)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (when (not (bobp)) (insert "\n"))
      (when (not (bolp)) (insert "\n"))
      (plist-pp (append `(:name ,task)
                        (when tags
                          `(:tags ,(chronometrist-maybe-string-to-symbol tags)))
                        (chronometrist-plist-remove plist :tags)
                        `(:start ,(format-time-string "%FT%T%z")))
                buffer)
      (save-buffer))))

;; TODO - implement PLIST arg
(defun chronometrist-out (&optional tags plist)
  "Record current moment as stop time to last s-exp in `chronometrist-file'.

PLIST is a property list containing any other information about
this time interval that should be recorded."
  (interactive `(,(completing-read-multiple "Tags (optional): "
                                            ;; FIXME - use tags, not tasks
                                            (chronometrist-tasks-from-table)
                                            nil 'confirm nil 'history)))
  (let ((buffer (find-file-noselect chronometrist-file)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (unless (bobp) (insert "\n"))
      (backward-list 1)
      (--> (read buffer)
           (plist-put it :stop (chronometrist-format-time-iso8601))
           (if tags
               (append (-take 2 it)
                       `(:tags ,(chronometrist-maybe-string-to-symbol tags))
                       (-drop 2 it))
             it)
           (progn
             (backward-list 1)
             (chronometrist-delete-list)
             (plist-pp it buffer)))
      (save-buffer))))

(defun chronometrist-reindent-buffer ()
  (interactive)
  (let (expr)
    (goto-char (point-min))
    (while (setq expr (ignore-errors (read (current-buffer))))
      (backward-list)
      (chronometrist-delete-list)
      (when (looking-at "\n*")
        (delete-region (match-beginning 0)
                       (match-end 0)))
      (plist-pp expr (current-buffer))
      (insert "\n")
      (unless (eobp)
        (insert "\n")))))

(define-derived-mode chronometrist-read-key-values-mode emacs-lisp-mode "Key-Values"
  "Mode used by `chronometrist' to read key values from the user."
  (add-hook 'after-load-functions #'elisp--font-lock-flush-elisp-buffers)
  (insert ";; Use C-c C-c to accept, or C-c C-k to cancel\n")
  ;; FIXME - font lock doesn't work in these buffers...
  ;; (font-lock-fontify-buffer)
  )

;; TODO - C-c C-c/C-c C-k bindings
(defun chronometrist-read-key-values ()
  (let ((buffer (get-buffer-create "*Chronometrist-Key-Values*")))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (chronometrist-common-clear-buffer buffer)
      (emacs-lisp-mode)
      ;; (chronometrist-read-key-values-mode)
      (unwind-protect
          (progn
            (insert "(")
            (while t
              (let (key value)
                ;; can't query these within the let definitions,
                ;; because that way KEY won't be inserted into the
                ;; buffer until you enter VALUE
                (setq key (completing-read "Key (C-g to quit): " nil))
                (when key   (insert ":" key))
                (setq value (completing-read "Value (C-g to quit): " nil))
                (when value (insert " " value "\n")))))
        (when (bolp)
          (backward-char 1))
        (insert ")")
        (chronometrist-reindent-buffer)))))

(provide 'chronometrist-sexp)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

;;; chronometrist-sexp.el ends here
