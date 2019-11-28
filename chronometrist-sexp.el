;;; chronometrist-sexp.el --- S-expression backend for Chronometrist -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'dash)
(require 'seq)

(require 'chronometrist-migrate)
(require 'chronometrist-events)
(require 'chronometrist-plist-pp)
(require 'chronometrist-common)

(declare-function chronometrist-refresh "chronometrist.el")

;;; Commentary:
;;

;;; Code:

(defvar chronometrist--tag-suggestions nil
  "Suggestions for tags.
Used as history by `chronometrist-tags-prompt'.")

(defvar chronometrist--value-suggestions nil
  "Suggestions for values.
Used as history by `chronometrist--value-suggestions'.")

(defun chronometrist-plist-remove (plist &rest keys)
  "Return PLIST with KEYS and their associated values removed."
  (let ((keys (--filter (plist-member plist it) keys)))
    (mapc (lambda (key)
            (let ((pos (seq-position plist key)))
              (setq plist (append (seq-take plist pos)
                                  (seq-drop plist (+ 2 pos))))))
          keys)
    plist))

(defun chronometrist-maybe-string-to-symbol (list)
  "For each string in LIST, if it has no spaces, convert it to a symbol."
  (--map (if (chronometrist-string-has-whitespace-p it)
             it
           (intern it))
         list))

(defun chronometrist-maybe-symbol-to-string (list)
  "Convert each symbol in LIST to a string."
  (--map (if (symbolp it)
             (symbol-name it)
           it)
         list))

(defun chronometrist-reindent-buffer ()
  "Reindent the current buffer.
This is meant to be run in `chronometrist-file'."
  (interactive)
  (let (expr)
    (goto-char (point-min))
    (while (setq expr (ignore-errors (read (current-buffer))))
      (backward-list)
      (chronometrist-delete-list)
      (when (looking-at "\n*")
        (delete-region (match-beginning 0)
                       (match-end 0)))
      (chronometrist-plist-pp expr (current-buffer))
      (insert "\n")
      (unless (eobp)
        (insert "\n")))))

(defun chronometrist-last-expr ()
  "Return last s-expression from `chronometrist-file'."
  (let ((buffer (find-file-noselect chronometrist-file)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (backward-list)
        (ignore-errors
          (read buffer))))))

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
         (old-kvs   (chronometrist-plist-remove old-expr :name :tags :start :stop))
         (plist     (chronometrist-plist-remove plist    :name :tags :start :stop))
         (new-tags  (if old-tags
                        (-> (append old-tags tags)
                            (cl-remove-duplicates :test #'equal))
                      tags))
         (new-kvs   (cl-copy-list old-expr))
         (new-kvs   (if plist
                        (-> (cl-loop for (key val) on plist by #'cddr
                                     do (plist-put new-kvs key val)
                                     finally return new-kvs)
                            (chronometrist-plist-remove :name :tags :start :stop))
                      old-kvs))
         (buffer     (find-file-noselect chronometrist-file)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (backward-list)
      (chronometrist-delete-list)
      (-> (append `(:name ,old-name)
                  (when new-tags `(:tags ,new-tags))
                  new-kvs
                  `(:start ,old-start)
                  (when old-stop `(:stop  ,old-stop)))
          (chronometrist-plist-pp buffer))
      (save-buffer))))

;;;; TAGS ;;;;
(defvar chronometrist-tags-history (make-hash-table :test #'equal)
  "Hash table of tasks and past tag combinations.
Each value is a list of tag combinations, in reverse
chronological order. Each combination is a list containing tags
as symbol and/or strings.")

(defun chronometrist-tags-history-populate ()
  "Add keys and values to `chronometrist-tags-history' by querying `chronometrist-events'."
  (let ((table chronometrist-tags-history))
    (clrhash table)
    (cl-loop for plist in (chronometrist-events-query chronometrist-events :get '(:name :tags))
             do (let* ((name          (plist-get plist :name))
                       (tags          (plist-get plist :tags))
                       (existing-tags (gethash name table)))
                  (when tags
                    (puthash name
                             (if existing-tags
                                 (append existing-tags `(,tags))
                               `(,tags))
                             table))))
    ;; We can't use `chronometrist-ht-history-prep' here, because it uses
    ;; `-flatten'; `chronometrist-tags-history' holds tag combinations (as lists),
    ;; not individual tags.
    (cl-loop for task being the hash-keys of table
             using (hash-values list)
             do (puthash task
                         ;; Because remove-duplicates keeps the _last_
                         ;; occurrence, trying to avoid this `reverse' by
                         ;; switching the args in the call to `append'
                         ;; above will not get you the correct behavior!
                         (-> (cl-remove-duplicates list :test #'equal)
                             (reverse))
                         table))))

(defun chronometrist-tags-history-combination-strings (task)
  "Return list of past tag combinations for TASK.
Each combination is a string, with tags separated by commas.

This is used to provide history for `completing-read-multiple' in
`chronometrist-tags-prompt'."
  (->> (gethash task chronometrist-tags-history)
       (mapcar (lambda (list)
                 (->> list
                      (mapcar (lambda (elt)
                                (if (stringp elt)
                                    elt
                                  (symbol-name elt))))
                      (-interpose ",")
                      (apply #'concat))))))

(defun chronometrist-tags-history-individual-strings (task)
  "Return list of tags for TASK, with each tag being a single string.
This is used to provide completion for individual tags, in
`completing-read-multiple' in `chronometrist-tags-prompt'."
  (--> (gethash task chronometrist-tags-history)
       (-flatten it)
       (cl-remove-duplicates it :test #'equal)
       (cl-loop for elt in it
                collect (if (stringp elt)
                            elt
                          (symbol-name elt)))))

(defun chronometrist-tags-prompt (task &optional initial-input)
  "Read one or more tags from the user and return them as a list of strings.
TASK should be a string.
INITIAL-INPUT is as used in `completing-read'."
  (setq chronometrist--tag-suggestions (chronometrist-tags-history-combination-strings task))
  (completing-read-multiple (concat "Tags for " task " (optional): ")
                            (chronometrist-tags-history-individual-strings task)
                            nil
                            'confirm
                            initial-input
                            'chronometrist--tag-suggestions))

(defun chronometrist-tags-add (&rest _args)
  "Read tags from the user, add them to the last s-expr in `chronometrist-file'.
_ARGS are ignored. This function always returns t."
  (let* ((last-expr (chronometrist-last-expr))
         (last-name (plist-get last-expr :name))
         (last-tags (plist-get last-expr :tags))
         (input     (->> last-tags
                         (chronometrist-maybe-symbol-to-string)
                         (-interpose ",")
                         (apply #'concat)
                         (chronometrist-tags-prompt last-name)
                         (chronometrist-maybe-string-to-symbol))))
    (when input
      (-> (append last-tags input)
          (reverse)
          (cl-remove-duplicates :test #'equal)
          (reverse)
          (chronometrist-append-to-last-expr nil)))
    t))

;;;; KEY-VALUES ;;;;
(defgroup chronometrist-key-values nil
  "Add key-values to Chronometrist time intervals."
  :group 'chronometrist)

(defcustom chronometrist-kv-buffer-name "*Chronometrist-Key-Values*"
  "Buffer name to read key-values from."
  :group 'chronometrist-key-values
  :type 'string)

(defvar chronometrist-key-history
  (make-hash-table :test #'equal)
  "Hash table to store previously-used user-keys.
The hash table keys are task names (as strings), and the values
are lists containing user-key names (as strings).")

(defvar chronometrist-value-history
  (make-hash-table :test #'equal)
  "Hash table to store previously-used values for user-keys.
The hash table keys are user-key names (as strings), and the
values are lists containing values (as strings).")

(defun chronometrist-ht-history-prep (table)
  "Prepare history hash tables for use in prompts.
Each value in hash table TABLE must be a list. Each value will be
reversed and will have duplicate elements removed."
  (maphash (lambda (key value)
             (puthash key
                      ;; placing `reverse' after `remove-duplicates'
                      ;; to get a list in reverse chronological order
                      (-> (-flatten value)
                          (cl-remove-duplicates :test #'equal)
                          (reverse))
                      table))
           table))

;; Since we have discarded sorting-by-frequency, we can now consider
;; implementing this by querying `chronometrist-events' instead of reading the file
(defun chronometrist-key-history-populate ()
  "Populate `chronometrist-key-history' from from `chronometrist-file'.
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
            (cl-loop for key in keys
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
  "Read values for user-keys from `chronometrist-events'.
The values are stored in `chronometrist-value-history'."
  ;; Note - while keys are Lisp keywords, values may be any Lisp
  ;; object, including lists
  (let ((table chronometrist-value-history)
        user-kvs)
    (clrhash table)
    (maphash (lambda (_date plist-list)
               (cl-loop for plist in plist-list
                        do (setq user-kvs (chronometrist-plist-remove plist
                                                         :name :tags
                                                         :start :stop))
                        (cl-loop for (key1 val1) on user-kvs by #'cddr
                                 do (let* ((key1-string (->> (symbol-name key1)
                                                             (s-chop-prefix ":")))
                                           (key1-ht     (gethash key1-string table))
                                           (val1        (if (not (stringp val1))
                                                            (list
                                                             (format "%s" val1))
                                                          (list val1))))
                                      (puthash key1-string
                                               (if key1-ht
                                                   (append key1-ht val1)
                                                 val1)
                                               table)))))
             chronometrist-events)
    (chronometrist-ht-history-prep table)))

;; TODO - refactor this to use `chronometrist-append-to-last-expr'
(defun chronometrist-kv-accept ()
  "Accept the plist in `chronometrist-kv-buffer-name' and add it to `chronometrist-file'."
  (interactive)
  (let ((backend-buffer (find-file-noselect chronometrist-file))
        user-kv-expr
        last-expr)
    (with-current-buffer (get-buffer chronometrist-kv-buffer-name)
      (goto-char (point-min))
      (setq user-kv-expr (ignore-errors (read (current-buffer))))
      (kill-buffer chronometrist-kv-buffer-name))
    (when user-kv-expr
      (with-current-buffer backend-buffer
        (goto-char (point-max))
        (backward-list)
        (setq last-expr (ignore-errors (read backend-buffer)))
        (backward-list)
        (chronometrist-delete-list)
        (let ((name    (plist-get last-expr :name))
              (tags    (plist-get last-expr :tags))
              (start   (plist-get last-expr :start))
              (stop    (plist-get last-expr :stop))
              (old-kvs (chronometrist-plist-remove last-expr :name :tags :start :stop)))
          (chronometrist-plist-pp (append (when name  `(:name  ,name))
                             (when tags  `(:tags  ,tags))
                             old-kvs
                             user-kv-expr
                             (when start `(:start ,start))
                             (when stop  `(:stop  ,stop)))
                     backend-buffer))
        (save-buffer)))))

(defun chronometrist-kv-reject ()
  "Reject the property list in `chronometrist-kv-buffer-name'."
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
  "Return non-nil if STRING contains whitespace."
  (string-match-p "[[:space:]]" string))

(defun chronometrist-key-prompt (used-keys)
  "Prompt the user to enter keys.
USED-KEYS are keys they have already used in this session."
  (let ((key-suggestions (--> (chronometrist-last-expr)
                              (plist-get it :name)
                              (gethash it chronometrist-key-history))))
    (completing-read (concat "Key ("
                             (chronometrist-kv-completion-quit-key)
                             " to quit): ")
                     ;; don't suggest keys which have already been used
                     (cl-loop for used-key in used-keys
                              do (->> key-suggestions
                                      (seq-remove (lambda (key)
                                                    (equal key used-key)))
                                      (setq key-suggestions))
                              finally return key-suggestions))))

(defun chronometrist-value-prompt (key)
  "Prompt the user to enter values.
KEY should be a string for the just-entered key."
  (setq chronometrist--value-suggestions
        (gethash key chronometrist-value-history))
  (read-from-minibuffer
   "Value (RET to quit): "
   ;; (2019-09-20T11:54:51+0530) this is more troublesome than helpful...
   ;; (car (gethash key chronometrist-value-history))
   nil nil nil
   'chronometrist--value-suggestions))

(defun chronometrist-kv-add (&rest _args)
  "Read key-values from user, adding them to a temporary buffer for review.

In the resulting buffer, users can run `chronometrist-kv-accept'
to add them to the last s-expression in `chronometrist-file', or
`chronometrist-kv-reject' to cancel.

_ARGS are ignored. This function always returns t."
  (let* ((buffer      (get-buffer-create chronometrist-kv-buffer-name))
         (first-key-p t)
         (last-kvs    (chronometrist-plist-remove (chronometrist-last-expr)
                                     :name :tags :start :stop))
         (used-keys   (->> (seq-filter #'keywordp last-kvs)
                           (mapcar #'symbol-name)
                           (--map (s-chop-prefix ":" it)))))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (chronometrist-common-clear-buffer buffer)
      (chronometrist-kv-read-mode)
      (if (and (chronometrist-current-task) last-kvs)
          (progn
            (chronometrist-plist-pp last-kvs buffer)
            (down-list -1)
            (insert "\n "))
        (insert "()")
        (down-list -1))
      (catch 'empty-input
        (let (input key value)
          (while t
            (setq key (chronometrist-key-prompt used-keys)
                  input key
                  used-keys (append used-keys
                                    (list key)))
            (if (string-empty-p input)
                (throw 'empty-input nil)
              (unless first-key-p
                (insert " "))
              (insert ":" key)
              (setq first-key-p nil))
            (setq value (chronometrist-value-prompt key)
                  input value)
            (if (string-empty-p input)
                (throw 'empty-input nil)
              (if (chronometrist-string-has-whitespace-p value)
                  (insert " \"" value "\"\n")
                (insert " " value "\n"))))))
      (chronometrist-reindent-buffer)))
  t)


;;;; COMMANDS ;;;;
(defun chronometrist-in (task &optional _prefix)
  "Clock in to TASK; record current time in `chronometrist-file'.
TASK is the name of the task, a string.

PREFIX is ignored."
  (interactive "P")
  (let ((buffer (find-file-noselect chronometrist-file)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (when (not (bobp)) (insert "\n"))
      (when (not (bolp)) (insert "\n"))
      (chronometrist-plist-pp `(:name  ,task
                  :start ,(format-time-string "%FT%T%z"))
                buffer)
      (save-buffer))))

(defun chronometrist-out (&optional _prefix)
  "Record current moment as stop time to last s-exp in `chronometrist-file'.
PLIST is a property list containing any other information about
this time interval that should be recorded.

PREFIX is ignored."
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
             (chronometrist-plist-pp it buffer)))
      (save-buffer))))

(provide 'chronometrist-sexp)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

;;; chronometrist-sexp.el ends here
