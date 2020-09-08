;;; chronometrist-plist-pp.el --- Functions to pretty print property lists -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; For more information, please refer to <https://unlicense.org>

;;; Commentary:

;;; potential improvements -
;;; * probably more robust code

;;; Code:

(require 'dash)

(defvar chronometrist-plist-pp-keyword-re ":[a-zA-Z0-9\-\?\+]+")

(defvar chronometrist-plist-pp-whitespace-re "[\t\s]+?")

(defun chronometrist-plist-pp-longest-keyword-length ()
  "Find the length of the longest keyword.

This assumes there is a single plist in the current buffer."
  (save-excursion
    (let ((keyword-lengths-list)
          (sexp))
      (goto-char (point-min))
      (ignore-errors (down-list 1))
      (while (setq sexp (ignore-errors
                          (read (current-buffer))))
        (when (symbolp sexp)
          (setq keyword-lengths-list (append keyword-lengths-list
                                             `(,(length (symbol-name sexp)))))))
      (-> keyword-lengths-list
          (sort #'>)
          (car)))))

(defun chronometrist-plist-pp-buffer-keyword-helper (indent)
  "Indent the keyword after point by INDENT spaces."
  (looking-at chronometrist-plist-pp-keyword-re)
  (let ((keyword (buffer-substring-no-properties (match-beginning 0)
                                                 (match-end 0))))
    (delete-region (match-beginning 0)
                   (match-end 0))
    (format (concat "% -" (number-to-string indent) "s")
            keyword)))

(defun chronometrist-plist-pp-buffer ()
  "Naive pretty-printer for plists."
  (let ((indent (chronometrist-plist-pp-longest-keyword-length)))
    (goto-char (point-min))
    (while (not (eobp))
      (cond
       ;; opening paren + first keyword
       ((looking-at-p (concat "(" chronometrist-plist-pp-keyword-re chronometrist-plist-pp-whitespace-re))
        (ignore-errors (down-list 1))
        (insert (chronometrist-plist-pp-buffer-keyword-helper indent))
        (forward-sexp 1)
        (insert "\n"))
       ((looking-at chronometrist-plist-pp-whitespace-re)
        (delete-region (match-beginning 0)
                       (match-end 0)))
       ;; any other keyword
       ((looking-at chronometrist-plist-pp-keyword-re)
        (insert " " (chronometrist-plist-pp-buffer-keyword-helper indent))
        (forward-sexp)
        (backward-sexp)
        ;; an alist as a value
        (if (looking-at "((")
            (progn
              (ignore-errors (down-list 1))
              (let (expr)
                (while (setq expr (ignore-errors (read (current-buffer))))
                  ;; end of alist
                  (unless (looking-at-p ")")
                    (insert "\n " (make-string (1+ indent) ?\s))))))
          (forward-sexp 1))
        (unless (looking-at-p ")")
          (insert "\n")))
       ((and (looking-at ")") (bolp))
        (delete-char -1)
        (forward-char 1))
       (t (forward-char 1))))))

(defun chronometrist-plist-pp-to-string (object)
  "Convert OBJECT to a pretty-printed string."
  (with-temp-buffer
    (lisp-mode-variables nil)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (let ((print-quoted t))
      (prin1 object (current-buffer)))
    (when (> (length object) 2)
      (chronometrist-plist-pp-buffer))
    (buffer-string)))

(defun chronometrist-plist-pp (object &optional stream)
  "Pretty-print OBJECT and output to STREAM (see `princ')."
  (princ (chronometrist-plist-pp-to-string object)
         (or stream standard-output)))

(provide 'chronometrist-plist-pp)

;;; chronometrist-plist-pp.el ends here
