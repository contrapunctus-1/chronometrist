;;; chronometrist-sexp.el --- S-expression backend for Chronometrist

(require 'chronometrist-migrate)
(require 'chronometrist-events)
(require 'dash)
(require 'seq)

;;; Commentary:
;;

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
  (--map (unless (chronometrist-string-has-whitespace-p it)
           (make-symbol it))
         list))

(defun chronometrist-maybe-symbol-to-string (list)
  "Convert each symbol in LIST to a string."
  (--map (unless (stringp it)
           (symbol-name it))
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

(defun chronometrist-last-expr ()
  "Return last s-expression from `chronometrist-file'.

Point is left after the last expression."
  (let ((buffer (find-file-noselect chronometrist-file)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (backward-list)
      (ignore-errors
        (read buffer)))))

(defun chronometrist-append-to-last-expr (tags plist)
  "Add TAGS and PLIST to last s-expression in `chronometrist-file'.

TAGS should be a list of symbols and/or strings.

PLIST should be a property list. Properties reserved by
Chronometrist - currently :name, :tags, :start, and :stop - will
be removed."
  (let* ((old-expr  (chronometrist-last-expr))
         (old-name  (plist-get old-expr :name))
         (old-start (plist-get old-expr :start))
         (old-stop  (plist-get old-expr :stop))
         (old-tags  (plist-get old-expr :tags))
         (old-kvs   (chronometrist-plist-remove old-expr
                                   :name :tags :start :stop))
         (plist     (chronometrist-plist-remove plist
                                   :name :tags :start :stop))
         (new-tags  (if old-tags
                        (-> (append old-tags tags)
                            ;; BUG - why isn't this removing duplicates as expected?
                            (remove-duplicates :test #'equal))
                      tags))
         (new-kvs   (copy-list old-expr))
         (new-kvs   (-> (loop for (key val) on plist by #'cddr
                              do (plist-put new-kvs key val)
                              return new-kvs)
                        (chronometrist-plist-remove :name :tags :start :stop)))
         (buffer     (find-file-noselect chronometrist-file)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (backward-list)
      (chronometrist-delete-list)
      (-> (append `(:name ,old-name)
                  `(:tags ,new-tags)
                  new-kvs
                  `(:start ,old-start)
                  (when old-stop `(:stop  ,old-stop)))
          (plist-pp buffer))
      (save-buffer))))

;;;; TAGS ;;;;
(defvar chronometrist-tags-history (make-hash-table :test #'equal)
  "List of past tag combinations.

Each combination is a list containing tags as symbol and/or strings.")

(defun chronometrist-tags-history-combination-strings (task)
  "Return list of past tag combinations for TASK.

Each combination is a string, with tags separated by commas."
  (->> (gethash task chronometrist-tags-history)
       (mapcar (lambda (list)
                 (->> list
                      (mapcar (lambda (elt)
                                (unless (stringp elt)
                                  (symbol-name elt))))
                      (-interpose ",")
                      (apply #'concat))))))

(defun chronometrist-tags-history-individual-strings (task)
  "Return list of tags for TASK as individual strings."
  (--> (gethash task chronometrist-tags-history)
       (-flatten it)
       (remove-duplicates it :test #'equal)
       (loop for elt in it
             collect (if (stringp elt)
                         elt
                       (symbol-name elt)))))

(defun chronometrist-tags-history-populate ()
  "Add keys and values to `chronometrist-tags-history' by querying `chronometrist-events'."
  (let ((table chronometrist-tags-history))
    (clrhash table)
    (loop for plist in (chronometrist-events-query chronometrist-events :get '(:name :tags))
          do (let* ((name          (plist-get plist :name))
                    (tags          (plist-get plist :tags))
                    (existing-tags (gethash name table)))
               (when tags
                 (puthash name
                          (if existing-tags
                              (append existing-tags `(,tags))
                            `(,tags))
                          table))))
    (loop for task being the hash-keys of table
          using (hash-values list)
          do (puthash task
                      (remove-duplicates list :test #'equal)
                      table))))

(defun chronometrist-tags-prompt (&optional task initial-input)
  "Read one or more tags from the user and return them as a list of strings.

INITIAL-INPUT is as used in `completing-read'."
  (let ((history (chronometrist-tags-history-combination-strings task)))
    (completing-read-multiple "Tags (optional): "
                              (chronometrist-tags-history-individual-strings task)
                              nil
                              'confirm
                              initial-input
                              'history)))

(defun chronometrist-tags-add (&rest args)
  "Read tags from the user and add them to the last s-expr in `chronometrist-file'.

ARGS are ignored. This function always returns t."
  (let* ((last-expr (chronometrist-last-expr))
         (last-name (plist-get last-expr :name))
         (last-tags (plist-get last-expr :tags)))
    (chronometrist-append-to-last-expr (->> last-tags
                               (chronometrist-maybe-symbol-to-string)
                               (-interpose ",")
                               (apply #'concat)
                               (chronometrist-tags-prompt last-name)
                               (chronometrist-maybe-string-to-symbol))
                          nil))
  t)

;;;; KEY-VALUES ;;;;
(defvar chronometrist-key-history   (make-hash-table :test #'equal))
(defvar chronometrist-value-history (make-hash-table :test #'equal))
(defvar chronometrist-kv-buffer-name "*Chronometrist-Key-Values*")

(defun chronometrist-ht-history-prep (table)
  "Prepare history hash tables for use in prompts.

Each value in hash table TABLE must be a list. Each value will be
reversed and will have duplicate elements removed."
  (maphash (lambda (key value)
             (puthash key
                      (-> (-flatten value)
                          (reverse)
                          (remove-duplicates :test #'equal))
                      table))
           table))

;; Since we have discarded sorting-by-frequency, we can now consider
;; implementing this by querying `chronometrist-events' instead of reading the file
;;
;; Buggy - getting >1 entries for the same
(defun chronometrist-key-history-populate ()
  "Clear hash table `chronometrist-key-history' and populate it with user-added keys.

The data is acquired from `chronometrist-file'.

Each hash table key is the name of a task. Each hash table value
is a list containing keywords used with that task, in reverse
chronological order. The keywords are stored as strings and their
leading \":\" is removed."
  (clrhash chronometrist-key-history)
  (--map (puthash it nil chronometrist-key-history)
         ;; ;; Not necessary, if the only placed this is called is `chronometrist-refresh-file'
         ;; (setq chronometrist--task-list (chronometrist-tasks-from-table))
         chronometrist-task-list)
  (with-current-buffer (find-file-noselect chronometrist-file)
    (save-excursion
      (goto-char (point-min))
      (let (expr)
        (while (setq expr (ignore-errors (read (current-buffer))))
          (let* ((name          (plist-get expr :name))
                 (name-ht-value (gethash name chronometrist-key-history))
                 (keys          (->> (chronometrist-plist-remove expr :name :start :stop :tags)
                                     (seq-filter #'keywordp))))
            (loop for key in keys
                  do (when key
                       (let ((key-string (->> (symbol-name key)
                                              (s-chop-prefix ":")
                                              (list))))
                         (puthash name
                                  (if name-ht-value
                                      (append name-ht-value key-string)
                                    key-string)
                                  chronometrist-key-history))))))
        (chronometrist-ht-history-prep chronometrist-key-history)))))

;; FIXME - seems to be a little buggy. The latest value for e.g. :song
;; is different from the one that ends up as the last in
;; `chronometrist-value-history' (before being reversed by `chronometrist-ht-history-prep')
(defun chronometrist-value-history-populate ()
  ;; Note - while keys are Lisp keywords, values may be any Lisp
  ;; object, including lists
  (clrhash chronometrist-value-history)
  (let (user-kvs)
    (maphash (lambda (date plist-list)
               (loop for plist in plist-list
                     do (setq user-kvs (chronometrist-plist-remove plist :name :tags :start :stop))
                     for (key1 val1) on user-kvs by #'cddr
                     do (let ((key1-string (->> (symbol-name key1)
                                                (s-chop-prefix ":")))
                              (key1-ht     (gethash key1 chronometrist-value-history))
                              (val1        (if (not (stringp val1))
                                               (list
                                                (format "%s" val1))
                                             (list val1))))
                          (puthash key1-string
                                   (if key1-ht
                                       (append key1-ht val1)
                                     val1)
                                   chronometrist-value-history))))
             chronometrist-events))
  (chronometrist-ht-history-prep chronometrist-value-history))

;; TODO - refactor this to use `chronometrist-append-to-last-expr'
(defun chronometrist-kv-accept ()
  "Accept the property list in `chronometrist-kv-buffer-name', adding it to `chronometrist-file'."
  (interactive)
  (let ((backend-buffer (find-file-noselect chronometrist-file))
        user-kv-expr
        last-expr)
    (with-current-buffer (get-buffer chronometrist-kv-buffer-name)
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
  (kill-buffer chronometrist-kv-buffer-name)
  (chronometrist-refresh))

(defvar chronometrist-kv-read-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'chronometrist-kv-accept)
    (define-key map (kbd "C-c C-k") #'chronometrist-kv-reject)
    map)
  "Keymap used by `chronometrist-mode'.")

(define-derived-mode chronometrist-kv-read-mode emacs-lisp-mode "Key-Values"
  "Mode used by `chronometrist' to read key values from the user."
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

(defun chronometrist-string-has-whitespace-p (string)
  (string-match-p "[[:space:]]" string))

(defun chronometrist-kv-add (&rest args)
  "Read key-values from user, adding them to a temporary buffer for review.

In the resulting buffer, users can run `chronometrist-kv-accept'
to add them to the last s-expression in `chronometrist-file', or
`chronometrist-kv-reject' to cancel.

ARGS are ignored. This function always returns t."
  (let ((buffer (get-buffer-create chronometrist-kv-buffer-name))
        (first-key-p t)
        last-sexp)
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (chronometrist-common-clear-buffer buffer)
      (chronometrist-kv-read-mode)
      (if (and
           (chronometrist-current-task)
           (setq last-sexp (chronometrist-plist-remove (chronometrist-last-expr)
                                          :name :tags :start :stop)))
          (progn
            (plist-pp last-sexp buffer)
            (down-list -1)
            (insert "\n "))
        (insert "()")
        (down-list -1))
      (catch 'empty-input
        (let (input key value value-history)
          (while t
            (setq key (completing-read (concat "Key ("
                                               (chronometrist-kv-completion-quit-key)
                                               " to quit): ")
                                       (-> (chronometrist-last-expr)
                                           (plist-get :name)
                                           (gethash chronometrist-key-history)))
                  input key)
            (if (string-empty-p input)
                (throw 'empty-input nil)
              (unless first-key-p
                (insert " ")
                (setq first-key-p nil))
              (insert ":" key))
            (setq value-history (gethash key chronometrist-value-history)
                  value (read-from-minibuffer "Value (RET to quit): "
                                              nil nil nil
                                              'value-history)
                  input value)
            (if (string-empty-p input)
                (throw 'empty-input nil)
              (if (chronometrist-string-has-whitespace-p value)
                  (insert " \"" value "\"")
                (insert " " value "\n"))))))
      (chronometrist-reindent-buffer)))
  t)


;;;; COMMANDS ;;;;
(defun chronometrist-in (task &optional prefix)
  "Clock in to TASK; record current time in `chronometrist-file'.

TASK is the name of the task, a string."
  (interactive "P")
  (let ((buffer (find-file-noselect chronometrist-file)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (when (not (bobp)) (insert "\n"))
      (when (not (bolp)) (insert "\n"))
      (plist-pp `(:name  ,task
                  :start ,(format-time-string "%FT%T%z"))
                buffer)
      (save-buffer))))

(defun chronometrist-out (&optional prefix)
  "Record current moment as stop time to last s-exp in `chronometrist-file'.

PLIST is a property list containing any other information about
this time interval that should be recorded."
  (interactive "P")
  (let ((buffer (find-file-noselect chronometrist-file)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (unless (bobp) (insert "\n"))
      (backward-list 1)
      (--> (read buffer)
           (plist-put it :stop (chronometrist-format-time-iso8601))
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
