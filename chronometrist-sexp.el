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

(defun chronometrist-last-sexp ()
  "Return last s-expression from `chronometrist-file'.

Point is left after the last expression."
  (let ((buffer (find-file-noselect chronometrist-file)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (backward-list)
      (ignore-errors
        (read buffer)))))

;;;; KEY-VALUES ;;;;
(defvar chronometrist-kv-buffer-name "*Chronometrist-Key-Values*")

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
  (->> ";; Use \\[chronometrist-kv-accept] to accept, or \\[chronometrist-kv-reject] to cancel\n"
       (substitute-command-keys)
       (insert)))

(defun chronometrist-kv-completion-quit-key ()
  "Return appropriate keybinding (as a string) to quit from `completing-read'.

It currently supports ido, ido-ubiquitous, ivy, and helm."
  (substitute-command-keys
   (cond ((or (bound-and-true-p ido-mode)
              (bound-and-true-p ido-ubiquitous-mode))
          "\\<ido-completion-map>\\[ido-select-text]")
         ((bound-and-true-p ivy-mode)
          "\\<ivy-minibuffer-map>\\[ivy-immediate-done]")
         ((bound-and-true-p helm-mode)
          "\\<helm-comp-read-map>\\[helm-cr-empty-string]")
         (t "leave blank"))))

(defun chronometrist-kv-read (&rest args)
  "Read key-values from user.

ARGS are ignored. This function always returns t."
  (let ((buffer (get-buffer-create chronometrist-kv-buffer-name))
        last-sexp)
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (chronometrist-common-clear-buffer buffer)
      (chronometrist-kv-read-mode)
      (if (and
           (chronometrist-current-task)
           (setq last-sexp (chronometrist-plist-remove (chronometrist-last-sexp)
                                          :name :tags :start :stop)))
          (progn
            (plist-pp last-sexp buffer)
            (down-list -1)
            (insert "\n "))
        (insert "()")
        (down-list -1))
      (catch 'empty-input
        (let (input key value)
          (while t
            (setq key (completing-read (concat "Key ("
                                               (chronometrist-kv-completion-quit-key)
                                               " to quit): ")
                                       (-> (chronometrist-last-sexp)
                                           (plist-get :name)
                                           (chronometrist-key-history-for-task)))
                  input key)
            (if (string-empty-p input)
                (throw 'empty-input nil)
              (insert " :" key))
            (setq value (read-from-minibuffer "Value (RET to quit): ")
                  input value)
            (if (string-empty-p input)
                (throw 'empty-input nil)
              (insert " " value "\n")))))
      (chronometrist-reindent-buffer)))
  t)


;;;; COMMANDS ;;;;
(defun chronometrist-in (task &optional tags)
  "Clock in to TASK; record current time in `chronometrist-file'.

TASK is the name of the task, a string."
  (interactive `(,(completing-read "Task name: "
                                   (chronometrist-tasks-from-table)
                                   nil 'confirm nil
                                   ;; TODO - implement history
                                   nil)))
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

(provide 'chronometrist-sexp)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

;;; chronometrist-sexp.el ends here
