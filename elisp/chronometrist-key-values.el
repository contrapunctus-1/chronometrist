;;; chronometrist-key-values.el --- add key-values to Chronometrist data -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'dash)
(require 'seq)
(require 'anaphora)

(require 'chronometrist-migrate)
(require 'chronometrist-events)
(require 'chronometrist-plist-pp)
(require 'chronometrist-common)

(declare-function chronometrist-refresh "chronometrist.el")
(declare-function chronometrist-last "chronometrist-queries.el")

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; For more information, please refer to <https://unlicense.org>

;;; Commentary:
;;

;;; Code:

(require 'chronometrist-sexp)

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

(defun chronometrist-append-to-last (tags plist)
  "Add TAGS and PLIST to the last entry in `chronometrist-file'.
TAGS should be a list of symbols and/or strings.

PLIST should be a property list. Properties reserved by
Chronometrist - currently :name, :tags, :start, and :stop - will
be removed."
  (let* ((old-expr    (chronometrist-last))
         (old-name    (plist-get old-expr :name))
         (old-start   (plist-get old-expr :start))
         (old-stop    (plist-get old-expr :stop))
         (old-tags    (plist-get old-expr :tags))
         ;; Anything that's left will be the user's key-values.
         (old-kvs     (chronometrist-plist-remove old-expr :name :tags :start :stop))
         ;; Prevent the user from adding reserved key-values.
         (plist       (chronometrist-plist-remove plist    :name :tags :start :stop))
         (new-tags    (if old-tags
                          (-> (append old-tags tags)
                              (cl-remove-duplicates :test #'equal))
                        tags))
         ;; In case there is an overlap in key-values, we use
         ;; plist-put to replace old ones with new ones.
         (new-kvs     (cl-copy-list old-expr))
         (new-kvs     (if plist
                          (-> (cl-loop for (key val) on plist by #'cddr
                                       do (plist-put new-kvs key val)
                                       finally return new-kvs)
                              (chronometrist-plist-remove :name :tags :start :stop))
                        old-kvs))
         (plist     (append `(:name ,old-name)
                            (when new-tags `(:tags ,new-tags))
                            new-kvs
                            `(:start ,old-start)
                            (when old-stop `(:stop  ,old-stop)))))
    (chronometrist-sexp-replace-last plist)))

;;;; TAGS ;;;;
(defvar chronometrist-tags-history (make-hash-table :test #'equal)
  "Hash table of tasks and past tag combinations.
Each value is a list of tag combinations, in reverse
chronological order. Each combination is a list containing tags
as symbol and/or strings.")

(defun chronometrist-tags-history-populate (events-table history-table)
  "Clear HISTORY-TABLE and fill it with tag combination data from EVENTS-TABLE.
Return the new value of HISTORY-TABLE.

HISTORY-TABLE and EVENTS-TABLE must be hash tables. (see
`chronometrist-tags-history' and `chronometrist-events')"
  (clrhash history-table)
  (cl-loop for events being the hash-values of events-table do
    (cl-loop for event in events do
      (let* ((name          (plist-get event :name))
             (tags          (plist-get event :tags))
             (existing-tags (gethash name history-table)))
        (when tags
          (puthash name
                   (if existing-tags
                       (append existing-tags `(,tags))
                     `(,tags))
                   history-table)))))
  ;; We can't use `chronometrist-ht-history-prep' to do this, because it uses
  ;; `-flatten'; the values of `chronometrist-tags-history' hold tag combinations
  ;; (as lists), not individual tags.
  (cl-loop for task being the hash-keys of history-table
    using (hash-values tag-lists) do
    (puthash task
             ;; Because remove-duplicates keeps the _last_
             ;; occurrence, trying to avoid this `reverse' by
             ;; switching the args in the call to `append'
             ;; above will not get you the correct behavior!
             (-> (cl-remove-duplicates tag-lists :test #'equal)
                 (reverse))
             history-table)
    finally return history-table))

(defun chronometrist-tags-history-add (plist)
  "Add tags from PLIST to `chronometrist-tags-history'."
  (let* ((table    chronometrist-tags-history)
         (name     (plist-get plist :name))
         (tags     (awhen (plist-get plist :tags) (list it)))
         (old-tags (gethash name table)))
    (when tags
      (--> (append tags old-tags)
           (puthash name it table)))))

(defun chronometrist-tags-history-replace-last (plist)
  "Replace the latest tag combination for PLIST's task with tags from PLIST."
  (let* ((table    chronometrist-tags-history)
         (name     (plist-get plist :name))
         (tags     (awhen (plist-get plist :tags) (list it)))
         (old-tags (gethash name table)))
    (when tags
      (if old-tags
          (--> (cdr old-tags)
               (append tags it)
               (puthash name it table))
        (puthash name tags table)))))

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
  "Read tags from the user and add them to the last entry in `chronometrist-file'.
_ARGS are ignored. This function always returns t, so it can be
used in `chronometrist-before-out-functions'."
  (let* ((last-expr (chronometrist-last))
         (last-name (plist-get last-expr :name))
         (last-tags (plist-get last-expr :tags))
         (input     (->> last-tags
                         (chronometrist-maybe-symbol-to-string)
                         (-interpose ",")
                         (apply #'concat)
                         (chronometrist-tags-prompt last-name)
                         (chronometrist-maybe-string-to-symbol))))
    (when input
      (--> (append last-tags input)
           (reverse it)
           (cl-remove-duplicates it :test #'equal)
           (reverse it)
           (chronometrist-append-to-last it nil)))
    t))

;;;; KEY-VALUES ;;;;
(defgroup chronometrist-key-values nil
  "Add key-values to Chronometrist time intervals."
  :group 'chronometrist)

(defcustom chronometrist-kv-buffer-name "*Chronometrist-Key-Values*"
  "Name of buffer in which key-values are entered."
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
  (cl-loop for list being the hash-values of table
    using (hash-keys key) do
    (puthash key
             ;; placing `reverse' after `remove-duplicates'
             ;; to get a list in reverse chronological order
             (-> (-flatten value)
                 (cl-remove-duplicates :test #'equal)
                 (reverse))
             table)
    finally returning table))

(defun chronometrist-key-history-populate ()
  "Populate `chronometrist-key-history' from `chronometrist-file'.
Each hash table key is the name of a task. Each hash table value
is a list containing keywords used with that task, in reverse
chronological order. The keywords are stored as strings and their
leading \":\" is removed."
  (clrhash chronometrist-key-history)
  ;; add each task as a key
  (mapc (lambda (task)
          (puthash task nil chronometrist-key-history))
        ;; ;; Not necessary, if the only place this is called is `chronometrist-refresh-file'
        ;; (setq chronometrist--task-list (chronometrist-tasks-from-table))
        chronometrist-task-list)
  (cl-loop for events being the hash-values of chronometrist-events do
    (cl-loop for plist in events do
      (let ((name (plist-get plist :name))
            (keys (->> (chronometrist-plist-remove plist :name :start :stop :tags)
                       (seq-filter #'keywordp))))
        (cl-loop for key in keys do
          (when key
            (let ((old-keys (gethash name chronometrist-key-history))
                  (new-key  (->> (symbol-name key)
                                 (s-chop-prefix ":")
                                 (list))))
              (--> (if old-keys
                       (append old-keys new-key)
                     new-key)
                   (puthash name it chronometrist-key-history))))))))
  (chronometrist-ht-history-prep chronometrist-key-history))

(defun chronometrist-value-history-populate (events-table history-table)
  "Clear HISTORY-TABLE and fill it with values for user-keys from EVENTS-TABLE.
Return the new value of HISTORY-TABLE.

EVENTS-TABLE and HISTORY-TABLE must be hash tables. (see
`chronometrist-events' and `chronometrist-value-history')"
  ;; Note - while keys are Lisp keywords, values may be any Lisp
  ;; object, including lists
  (clrhash history-table)
  (cl-loop with user-key-values
    for events-list being the hash-values of events-table do
    (cl-loop for event in events-list do
      ;; We call them user-key-values because we filter out Chronometrist's
      ;; reserved key-values
      (setq user-key-values (chronometrist-plist-remove event :name :tags :start :stop))
      (cl-loop for (key value) on user-key-values by #'cddr do
        (let* ((key-string (->> (symbol-name key) (s-chop-prefix ":")))
               (old-values (gethash key-string history-table))
               (value      (if (not (stringp value))
                               (list (format "%s" value))
                             (list value))))
          (puthash key-string
                   (if old-values
                       (append old-values value)
                     value)
                   history-table)))))
  (chronometrist-ht-history-prep history-table))

(defvar chronometrist-kv-read-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'chronometrist-kv-accept)
    (define-key map (kbd "C-c C-k") #'chronometrist-kv-reject)
    map)
  "Keymap used by `chronometrist-kv-read-mode'.")

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
USED-KEYS are keys they have already added since the invocation
of `chronometrist-kv-add'."
  (let ((key-suggestions (--> (chronometrist-last)
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

(defun chronometrist-value-insert (value)
  "Insert VALUE into the key-value entry buffer."
  (insert " ")
  (cond ((or
          ;; list or vector
          (and (string-match-p (rx (and bos (or "(" "\"" "["))) value)
               (string-match-p (rx (and (or ")" "\"" "]") eos)) value))
          ;; int or float
          (string-match-p "^[0-9]*\\.?[0-9]*$" value))
         (insert value))
        (t
         (insert "\"" value "\"")))
  (insert "\n"))

(defun chronometrist-kv-add (&rest _args)
  "Read key-values from user, adding them to a temporary buffer for review.
In the resulting buffer, users can run `chronometrist-kv-accept'
to add them to the last s-expression in `chronometrist-file', or
`chronometrist-kv-reject' to cancel.

_ARGS are ignored. This function always returns t, so it can be
used in `chronometrist-before-out-functions'."
  (let* ((buffer      (get-buffer-create chronometrist-kv-buffer-name))
         (first-key-p t)
         (last-kvs    (chronometrist-plist-remove (chronometrist-last)
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
              (chronometrist-value-insert value)))))
      (chronometrist-sexp-reindent-buffer)))
  t)

;;;; COMMANDS ;;;;
(defun chronometrist-kv-accept ()
  "Accept the plist in `chronometrist-kv-buffer-name' and add it to `chronometrist-file'."
  (interactive)
  (let (user-kv-expr)
    (with-current-buffer (get-buffer chronometrist-kv-buffer-name)
      (goto-char (point-min))
      (setq user-kv-expr (ignore-errors (read (current-buffer))))
      (kill-buffer chronometrist-kv-buffer-name))
    (if user-kv-expr
        (chronometrist-append-to-last nil user-kv-expr)
      (chronometrist-refresh))))

(defun chronometrist-kv-reject ()
  "Reject the property list in `chronometrist-kv-buffer-name'."
  (interactive)
  (kill-buffer chronometrist-kv-buffer-name)
  (chronometrist-refresh))

(provide 'chronometrist-key-values)

;;; chronometrist-key-values.el ends here
