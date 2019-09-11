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

(defun chronometrist-in (task &optional tags)
  "Clock in to TASK; record current time in `chronometrist-file'.

TASK is the name of the task, a string."
  (interactive `(,(completing-read "Task name: "
                                   (chronometrist-tasks-from-table)
                                   nil 'confirm nil
                                   ;; TODO - implement history
                                   nil)
                 ,(completing-read-multiple "Tags (optional): "
                                            ;; FIXME - use tags, not tasks
                                            (chronometrist-tasks-from-table)
                                            nil 'confirm nil 'history)))
  (let ((buffer (find-file-noselect chronometrist-file)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (when (not (bobp)) (insert "\n"))
      (when (not (bolp)) (insert "\n"))
      (plist-pp (append `(:name ,task)
                        (when tags
                          `(:tags ,(chronometrist-maybe-string-to-symbol tags)))
                        ;; May cause problems if PLIST has any keys in
                        ;; common with Chronometrist's...
                        `(:start ,(format-time-string "%FT%T%z")))
                buffer)
      (save-buffer))))

(defun chronometrist-out (&optional tags)
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

(defvar chronometrist-kv-buffer-name "*Chronometrist-Key-Values*")

(defvar chronometrist--kv nil)

(defun chronometrist-kv-accept ()
  "Accept the property list in `chronometrist-kv-buffer-name', adding it to `chronometrist-file'."
  (interactive)
  (let ((backend-buffer (find-file-noselect chronometrist-file))
        user-kv-expr
        last-expr)
    (with-current-buffer (get-buffer-create chronometrist-kv-buffer-name)
      (goto-char (point-min))
      (setq user-kv-expr (ignore-errors (read (current-buffer))))
      (kill-buffer chronometrist-kv-buffer-name))
    (with-current-buffer backend-buffer
      (goto-char (point-max))
      (backward-list)
      (setq last-expr (ignore-errors (read backend-buffer)))
      (backward-list)
      (chronometrist-delete-list)
      ;; REVIEW - as a side-effect, this removes anything that isn't
      ;; one of :name, :tags, :start, and :stop...just for the sake of
      ;; keeping the keys in a specific order.
      (let ((name  (plist-get last-expr :name))
            (tags  (plist-get last-expr :tags))
            (start (plist-get last-expr :start))
            (stop  (plist-get last-expr :stop)))
        (plist-pp (append (when name  `(:name  ,name))
                          (when tags  `(:tags  ,tags))
                          user-kv-expr
                          (when start `(:start ,start))
                          (when stop  `(:stop  ,stop)))
                  backend-buffer))
      (save-buffer))))

(defun chronometrist-kv-reject ()
  (interactive)
  (kill-buffer chronometrist-kv-buffer-name))

(defvar chronometrist-kv-read-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'chronometrist-kv-accept)
    (define-key map (kbd "C-c C-k") #'chronometrist-kv-reject)
    map)
  "Keymap used by `chronometrist-mode'.")

(define-derived-mode chronometrist-kv-read-mode emacs-lisp-mode "Key-Values"
  "Mode used by `chronometrist' to read key values from the user."
  ;; TODO - check keybindings at run-time instead of hard-coding them
  (insert ";; Use C-c C-c to accept, or C-c C-k to cancel\n"))

(defun chronometrist-last-sexp ()
  "Return last s-expression from `chronometrist-file'.

Point is left after the last expression."
  (let ((buffer (find-file-noselect chronometrist-file)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (backward-list)
      (ignore-errors
        (read buffer)))))

(defun chronometrist-kv-read (&rest args)
  "Read key-values from user.

ARGS are ignored."
  (let ((buffer (get-buffer-create chronometrist-kv-buffer-name)))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (chronometrist-common-clear-buffer buffer)
      (chronometrist-kv-read-mode)
      (if (chronometrist-current-task)
          (progn
            (-> (chronometrist-last-sexp)
                (chronometrist-plist-remove :name :tags :start :stop)
                (plist-pp buffer))
            (down-list -1)
            (insert "\n "))
        (insert "("))
      (catch 'empty-input
        (let (input key value)
          (while t
            ;; TODO - implement history/suggestions

            ;; can't query these within the `let' definitions,
            ;; because that way KEY won't be inserted into the
            ;; buffer until you enter VALUE
            (setq key   (completing-read "Key (leave blank to quit): " nil)
                  input key)
            (if (string-empty-p input)
                (throw 'empty-input nil)
              (insert " :" key))

            ;; TODO - insert as string if it contains spaces and isn't a list
            (setq value (read-from-minibuffer "Value (leave blank to quit): ")
                  input value)
            (if (string-empty-p input)
                (throw 'empty-input nil)
              (insert " " value "\n")))))
      (when (bolp)
        (backward-char 1))
      (unless (chronometrist-current-task)
        (insert ")"))
      (chronometrist-reindent-buffer)))
  t)

(provide 'chronometrist-sexp)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

;;; chronometrist-sexp.el ends here
