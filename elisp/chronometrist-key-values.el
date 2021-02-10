;;; chronometrist-key-values.el --- add key-values to Chronometrist data -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'dash)
(require 'seq)
(require 'anaphora)
(require 'choice)

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

(defun chronometrist-plist-update (old-plist new-plist)
  "Add tags and keyword-values from NEW-PLIST to OLD-PLIST.
OLD-PLIST and NEW-PLIST should be a property lists.

Keywords reserved by Chronometrist - :name, :start, and :stop -
will not be updated. Keywords in OLD-PLIST with new values in
NEW-PLIST will be updated. Tags in OLD-PLIST will be preserved
alongside new tags from NEW-PLIST."
  (-let* (((&plist :name  old-name  :tags old-tags
                   :start old-start :stop old-stop) old-plist)
          ;; Anything that's left will be the user's key-values.
          (old-kvs   (chronometrist-plist-remove old-plist :name :tags :start :stop))
          ;; Prevent the user from adding reserved key-values.
          (plist     (chronometrist-plist-remove new-plist :name :tags :start :stop))
          (new-tags  (-> (append old-tags (plist-get new-plist :tags))
                         (cl-remove-duplicates :test #'equal)))
          ;; In case there is an overlap in key-values, we use
          ;; plist-put to replace old ones with new ones.
          (new-kvs   (cl-copy-list old-plist))
          (new-kvs   (if plist
                         (-> (cl-loop for (key val) on plist by #'cddr
                               do (plist-put new-kvs key val)
                               finally return new-kvs)
                             (chronometrist-plist-remove :name :tags :start :stop))
                       old-kvs)))
    (append `(:name ,old-name)
            (when new-tags `(:tags ,new-tags))
            new-kvs
            `(:start ,old-start)
            (when old-stop `(:stop  ,old-stop)))))

;;;; TAGS ;;;;
(defcustom chronometrist-tag-history-style :combinations
  "How previously-used tags are suggested.
Valid values are :combinations and :individual."
  :group 'chronometrist-key-values
  :type '(choice (const :combinations)
                 (const :individual)))

(defcustom chronometrist-key-history-style :individual
  "How previously-used tags are suggested.
Valid values are :combinations and :individual."
  :group 'chronometrist-key-values
  :type '(choice (const :combinations)
                 (const :individual)))

(defvar chronometrist-tags-history (make-hash-table :test #'equal)
  "Hash table of tasks and past tag combinations.
Each value is a list of tag combinations, in reverse
chronological order. Each combination is a list containing tags
as symbol and/or strings.")

(defun chronometrist-history-prep (key history-table)
  "Prepare history hash tables for use in prompts.
Each value in hash table TABLE must be a list. Each value will be
reversed and will have duplicate elements removed."
  (--> (gethash key history-table)
       (cl-remove-duplicates it :test #'equal :from-end t)
       (puthash key it history-table)))

(defun chronometrist-tags-history-populate (task history-table file)
  "Store tag history for TASK in HISTORY-TABLE from FILE.
Return the new value inserted into HISTORY-TABLE.

HISTORY-TABLE must be a hash table. (see `chronometrist-tags-history')"
  (puthash task nil history-table)
  (chronometrist-loop-file for plist in file do
    (let ((new-tag-list  (plist-get plist :tags))
          (old-tag-lists (gethash task history-table)))
      (and (equal task (plist-get plist :name))
           new-tag-list
           (puthash task
                    (if old-tag-lists
                        (append old-tag-lists (list new-tag-list))
                      (list new-tag-list))
                    history-table))))
  (chronometrist-history-prep task history-table))

(defun chronometrist-key-history-populate (task history-table file)
  "Store key history for TASK in HISTORY-TABLE from FILE.
Return the new value inserted into HISTORY-TABLE.

HISTORY-TABLE must be a hash table (see `chronometrist-key-history')."
  (puthash task nil history-table)
  (chronometrist-loop-file for plist in file do
    (catch 'quit
      (let* ((name     (plist-get plist :name))
             (check    (unless (equal name task) (throw 'quit nil)))
             (new-keys (--> (chronometrist-plist-remove plist :name :start :stop :tags)
                            (seq-filter #'keywordp it)
                            (cl-loop for key in it collect
                              (s-chop-prefix ":" (symbol-name key)))))
             (check    (unless new-keys (throw 'quit nil)))
             (new-keys (case chronometrist-key-history-style
                         (:combinations (list new-keys))
                         (:individual   new-keys)))
             (old-keys (gethash name history-table)))
        (puthash name
                 (if old-keys (append old-keys new-keys) new-keys)
                 history-table))))
  (chronometrist-history-prep task history-table))

;; We don't want values to be task-sensitive, so this does not have a
;; KEY parameter similar to TASK for `chronometrist-tags-history-populate' or
;; `chronometrist-key-history-populate'
(defun chronometrist-value-history-populate (history-table file)
  "Store value history in HISTORY-TABLE from FILE.
HISTORY-TABLE must be a hash table. (see `chronometrist-value-history')"
  (clrhash history-table)
  ;; Note - while keys are Lisp keywords, values may be any Lisp
  ;; object, including lists
  (chronometrist-loop-file for plist in file do
    ;; We call them user-key-values because we filter out Chronometrist's
    ;; reserved key-values
    (let ((user-key-values (chronometrist-plist-remove plist :name :tags :start :stop)))
      (cl-loop for (key value) on user-key-values by #'cddr do
        (let* ((key-string (s-chop-prefix ":" (symbol-name key)))
               (old-values (gethash key-string history-table))
               (value      (if (not (stringp value)) ;; why?
                               (list (format "%S" value))
                             (list value))))
          (puthash key-string
                   (if old-values (append old-values value) value)
                   history-table)))))
  (maphash (lambda (key values)
             (chronometrist-history-prep key history-table))
           history-table))

(defun chronometrist-tags-history-add (plist)
  "Add tags from PLIST to `chronometrist-tags-history'."
  (let* ((table    chronometrist-tags-history)
         (name     (plist-get plist :name))
         (tags     (awhen (plist-get plist :tags) (list it)))
         (old-tags (gethash name table)))
    (when tags
      (--> (append tags old-tags)
           (puthash name it table)))))

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
  "Read tags from the user; add them to the last entry in `chronometrist-file'.
_ARGS are ignored. This function always returns t, so it can be
used in `chronometrist-before-out-functions'."
  (interactive)
  (unless chronometrist--skip-detail-prompts
    (let* ((last-expr (chronometrist-last))
           (last-name (plist-get last-expr :name))
           (_history  (chronometrist-tags-history-populate last-name chronometrist-tags-history chronometrist-file))
           (last-tags (plist-get last-expr :tags))
           (input     (->> (chronometrist-maybe-symbol-to-string last-tags)
                           (-interpose ",")
                           (apply #'concat)
                           (chronometrist-tags-prompt last-name)
                           (chronometrist-maybe-string-to-symbol))))
      (when input
        (--> (chronometrist-plist-update (chronometrist-sexp-last) (list :tags input))
             (chronometrist-sexp-replace-last it)))))
  t)

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
Each hash key is the name of a task. Each hash value is a list
containing keywords used with that task, in reverse chronological
order. The keywords are stored as strings and their leading \":\"
is removed.")

(defvar chronometrist-value-history
  (make-hash-table :test #'equal)
  "Hash table to store previously-used values for user-keys.
The hash table keys are user-key names (as strings), and the
values are lists containing values (as strings).")

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
    (completing-read (format "Key (%s to quit): " (chronometrist-kv-completion-quit-key))
                     ;; don't suggest keys which have already been used
                     (cl-loop for used-key in used-keys do
                       (->> key-suggestions
                            (seq-remove (lambda (key)
                                          (equal key used-key)))
                            (setq key-suggestions))
                       finally return key-suggestions)
                     nil nil nil 'key-suggestions)))

(defun chronometrist-value-prompt (key)
  "Prompt the user to enter values.
KEY should be a string for the just-entered key."
  (setq chronometrist--value-suggestions (gethash key chronometrist-value-history))
  (completing-read (format "Value (%s to quit): " (chronometrist-kv-completion-quit-key))
                   chronometrist--value-suggestions nil nil nil 'chronometrist--value-suggestions))

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
        (t (insert "\"" value "\"")))
  (insert "\n"))

(defun chronometrist-kv-add (&rest _args)
  "Read key-values from user, adding them to a temporary buffer for review.
In the resulting buffer, users can run `chronometrist-kv-accept'
to add them to the last s-expression in `chronometrist-file', or
`chronometrist-kv-reject' to cancel.

_ARGS are ignored. This function always returns t, so it can be
used in `chronometrist-before-out-functions'."
  (unless chronometrist--skip-detail-prompts
    (let* ((buffer      (get-buffer-create chronometrist-kv-buffer-name))
           (first-key-p t)
           (last-sexp   (chronometrist-last))
           (last-name   (plist-get last-sexp :name))
           (last-kvs    (chronometrist-plist-remove last-sexp :name :tags :start :stop))
           (used-keys   (->> (seq-filter #'keywordp last-kvs)
                             (mapcar #'symbol-name)
                             (--map (s-chop-prefix ":" it)))))
      (chronometrist-key-history-populate last-name chronometrist-key-history chronometrist-file)
      (chronometrist-value-history-populate chronometrist-value-history chronometrist-file)
      (switch-to-buffer buffer)
      (with-current-buffer buffer
        (chronometrist-common-clear-buffer buffer)
        (chronometrist-kv-read-mode)
        (if (and (chronometrist-current-task) last-kvs)
            (progn
              (funcall chronometrist-sexp-pretty-print-function last-kvs buffer)
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
        (chronometrist-sexp-reindent-buffer))))
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
    (aif user-kv-expr
        (chronometrist-sexp-replace-last
         (chronometrist-plist-update (chronometrist-sexp-last) it))
      (chronometrist-refresh))))

(defun chronometrist-kv-reject ()
  "Reject the property list in `chronometrist-kv-buffer-name'."
  (interactive)
  (kill-buffer chronometrist-kv-buffer-name)
  (chronometrist-refresh))

;;;; SKIPPING QUERIES ;;;;
(defvar chronometrist--skip-detail-prompts nil)

(defun chronometrist-skip-query-prompt (task)
  "Offer to skip tag/key-value prompts and reuse last-used details.
This function always returns t, so it can be used in `chronometrist-before-out-functions'."
  ;; find latest interval for TASK; if it has tags or key-values, prompt
  (let (plist)
    ;; iterate over events in reverse
    (cl-loop for key in (reverse (hash-table-keys chronometrist-events)) do
      (cl-loop for event in (reverse (gethash key chronometrist-events))
        when (and (equal task (plist-get event :name))
                  (setq plist (chronometrist-plist-remove event :name :start :stop)))
        return nil)
      when plist return nil)
    (and plist
         (yes-or-no-p
          (format "Skip prompt and use last-used tags/key-values? %S " plist))
         (setq chronometrist--skip-detail-prompts t)
         (chronometrist-sexp-replace-last
          (chronometrist-plist-update (chronometrist-sexp-last) plist)))
    t))

(defun chronometrist-skip-query-reset (_task)
  "Enable prompting for tags and key-values.
This function always returns t, so it can be used in `chronometrist-before-out-functions'."
  (setq chronometrist--skip-detail-prompts nil) t)

;; TODO
;; 1. rename `chronometrist-tags-history' to `chronometrist-tag-history' for consistency
;; 2. suggest key combinations for task, instead of individual keys
;;    * values for each of the selected keys can be queried one by one
;;      after that
;;    * make it possible to select new keys after initial
;;      key-combination selection - perhaps at a confirmation step
;;      after the values are selected?
;; 3. select a combination and edit it
;;    * use universal argument?
;; 4. Multiple values for a key

;; #### POSSIBLE INTERFACES ####
;;
;; (#1 and #2 are meant to be mixed and matched.)
;;
;; 1. (tag|key|value) combinations -> ...
;;      0-9     - use combination (and exit)
;;      C-u 0-9 - edit combination (then exit)
;;      s       - skip (exit)
;;      (b      - back [to previous prompt])

;; 2. select individual (tags|keys|values) -> ...
;;      0-9 - select keys (toggles; save in var; doesn't exit)
;;      u   - use selection (and exit)
;;      e   - edit selection (then exit)
;;      s   - skip (exit)
;;      (b  - back [to previous prompt])
;;    Great for values; makes it easy to add multiple values, too,
;;    especially for users who don't know Lisp.

;; 3. tag-key-value combinations (everything in one prompt)
;;      0-9     - use combination (and exit)
;;      C-u 0-9 - edit combination (then exit)
;;      s       - skip (exit)

;; [x] we want C-g to quit, and universal arg to work...

;; FIXME - incorrect tags added to file

(defun chronometrist-defchoice (mode key table)
  "MODE ::= :tag
          | :key
          | :value

KEY ::= \"task\" (if MODE is :tags or :keys)
      | \"key\" (if MODE is :values)"
  (cl-loop with num = 0
    for comb in (-take 10 (gethash key table))
    do (incf num)
    if (= num 10) do (setq num 0)
    collect
    (list (format "%s" num)
          `(chronometrist-sexp-replace-last
            (chronometrist-plist-update (chronometrist-sexp-last) ',(list :tags comb)))
          (format "%s" comb))
    into numeric-commands
    finally do
    (eval `(defchoice ,(intern
                        (format
                         "chronometrist-%s" (s-chop-prefix ":" (symbol-name mode))))
             ,@numeric-commands
             ("s" nil "skip")))))

(defun chronometrist-tag-choice (task)
  "Query user for tags to be added to TASK.
Return t, to permit use in `chronometrist-before-out-functions'."
  (let ((table chronometrist-tags-history))
    (chronometrist-tags-history-populate task table chronometrist-file)
    (if (hash-table-empty-p table)
        (chronometrist-tags-add)
      (chronometrist-defchoice :tag task table)
      (chronometrist-tag-choice-prompt "Which tags?"))
    t))

(provide 'chronometrist-key-values)

;;; chronometrist-key-values.el ends here
