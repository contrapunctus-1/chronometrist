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

(defvar chronometrist-plist-pp-whitespace-re "[\t\s]+?")

(defun chronometrist-plist-pp-longest-keyword-length ()
  "Find the length of the longest keyword.
This assumes there is a single plist in the current buffer."
  (save-excursion
    (let (keyword-lengths-list sexp)
      (goto-char (point-min))
      (ignore-errors (down-list 1))
      (while (setq sexp (ignore-errors (read (current-buffer))))
        (when (symbolp sexp)
          (setq keyword-lengths-list (append keyword-lengths-list
                                             `(,(length (symbol-name sexp)))))))
      (-> keyword-lengths-list (sort #'>) (car)))))

(defun chronometrist-plist-pp-buffer-keyword-helper (sexp indent)
  "Return a string indenting SEXP by INDENT spaces."
  (chronometrist-sexp-delete-list)
  (format (concat "% -" (number-to-string indent) "s") sexp))

(defun chronometrist-plist-pp-buffer ()
  "Naive pretty-printer for plists."
  (let ((indent (chronometrist-plist-pp-longest-keyword-length)) sexp)
    (goto-char (point-min))
    ;; opening paren + first keyword
    (ignore-errors (down-list 1))
    (setq sexp (save-excursion (read (current-buffer))))
    (insert (chronometrist-plist-pp-buffer-keyword-helper sexp indent))
    (forward-sexp 1)
    (insert "\n")
    (while (not (eobp))
      (unless (looking-at-p ")")
        (setq sexp (save-excursion (read (current-buffer)))))
      (cond
       ((and (looking-at ")") (bolp))
        (progn (delete-char -1) (forward-char 1)))
       ((looking-at chronometrist-plist-pp-whitespace-re)
        (delete-region (match-beginning 0) (match-end 0)))
       ;; any other keyword
       ((keywordp sexp)
        (insert " " (chronometrist-plist-pp-buffer-keyword-helper sexp indent))
        (if (not (looking-at "("))
            (forward-sexp 1)
          ;; we are before a list as a value
          (let ((sexp (read (current-buffer))))
            (cond ((json-plist-p sexp) )
                  ((json-alist-p sexp) )))
          ;; TODO - determine if it is a plain list, a plist/alist,
          ;; or a list of plists
          (backward-sexp)
          (chronometrist-sexp-delete-list)
          (insert (chronometrist-plist-pp-to-string sexp)))
        (insert "\n"))
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
