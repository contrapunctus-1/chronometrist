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

(defvar chronometrist-plist-pp-whitespace-re "[\t\s]+?")

(defun chronometrist-plist-pp-normalize-whitespace ()
  (when (looking-at chronometrist-plist-pp-whitespace-re)
    (delete-region (match-beginning 0) (match-end 0))
    (insert " ")))

(defun chronometrist-plist-pp-column ()
  "Return column point is on, as an integer.
0 means point is at the beginning of the line."
  (- (point) (point-at-bol)))

(defun chronometrist-plist-pp-pair-p (cons)
  (and (listp cons) (not (listp (cdr cons)))))

(defun chronometrist-plist-pp-alist-p (list)
  (when (listp list)
    (cl-loop for elt in list always (chronometrist-plist-pp-pair-p elt))))

(defun chronometrist-plist-pp-longest-keyword-length ()
  "Find the length of the longest keyword in a plist.
This assumes there is a single plist in the current buffer, and
that point is after the first opening parenthesis."
  (save-excursion
    (cl-loop with sexp
      while (setq sexp (ignore-errors (read (current-buffer))))
      when (keywordp sexp)
      maximize (length (symbol-name sexp)))))

(cl-defun chronometrist-plist-pp-indent-sexp (sexp &key (left-indent 0) (right-indent 0))
  "Return a string indenting SEXP by LEFT-INDENT and RIGHT-INDENT spaces."
  (format (concat (make-string left-indent ? )
                  "% -" (number-to-string right-indent) "s")
          sexp))

;; not a list? forward-sexp
;; if list ->
;;   plist -> plist indent;
;;   alist -> alist indent;
;;   else descend and call self for each element, then leave

;; FIXME - we're losing the value of inside-sublist-p somewhere

(cl-defun chronometrist-plist-pp-buffer (&optional inside-sublist-p)
  "Naive pretty-printer for plists."
  (if (not (looking-at-p ")"))
      (progn
        (setq sexp (save-excursion (read (current-buffer))))
        (cond
         ((json-plist-p sexp)
          (chronometrist-plist-pp-buffer-plist inside-sublist-p) (chronometrist-plist-pp-buffer))
         ((chronometrist-plist-pp-alist-p sexp)
          (chronometrist-plist-pp-buffer-alist)
          (unless inside-sublist-p (chronometrist-plist-pp-buffer)))
         ((listp sexp)
          (down-list)
          (chronometrist-plist-pp-buffer t))
         (t (forward-sexp)
            (chronometrist-plist-pp-buffer))))
    (when (bolp) (delete-char -1))
    (forward-char)))

(defun chronometrist-plist-pp-buffer-plist (&optional inside-sublist-p)
  "Indent a single plist after point."
  (down-list)
  (let ((left-indent  (1- (chronometrist-plist-pp-column)))
        (right-indent (chronometrist-plist-pp-longest-keyword-length))
        (first-p t) sexp)
    (while (not (looking-at-p ")"))
      (chronometrist-plist-pp-normalize-whitespace)
      (setq sexp (save-excursion (read (current-buffer))))
      (cond ((keywordp sexp)
             (chronometrist-sexp-delete-list)
             (insert (if first-p
                         (progn (setq first-p nil) "")
                       (make-string left-indent ?\ ))
                     (chronometrist-plist-pp-indent-sexp sexp :right-indent right-indent)))
            ;; not a keyword = a value
            ((listp sexp)
             (chronometrist-plist-pp-buffer t)
             (insert "\n"))
            (t (forward-sexp)
               (insert "\n"))))
    (when (bolp) (delete-char -1))
    (up-list)
    (unless (eobp) (insert "\n"))
    (when inside-sublist-p
      (insert (make-string (1- left-indent) ?\ )))))

(defun chronometrist-plist-pp-buffer-alist ()
  "Indent a single alist after point."
  (down-list)
  (let ((indent (chronometrist-plist-pp-column)) (first-p t) sexp)
    (while (not (looking-at-p ")"))
      (setq sexp (save-excursion (read (current-buffer))))
      (chronometrist-sexp-delete-list)
      (insert (if first-p
                  (progn (setq first-p nil) "")
                (make-string indent ?\ ))
              (format "%S\n" sexp)))
    (when (bolp) (delete-char -1))
    (up-list)))

(defun chronometrist-plist-pp-to-string (object &optional left-indent)
  "Convert OBJECT to a pretty-printed string."
  (with-temp-buffer
    (lisp-mode-variables nil)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (let ((print-quoted t))
      (prin1 object (current-buffer)))
    (when (> (length object) 2)
      (chronometrist-plist-pp-buffer left-indent))
    (buffer-string)))

(defun chronometrist-plist-pp (object &optional stream)
  "Pretty-print OBJECT and output to STREAM (see `princ')."
  (princ (chronometrist-plist-pp-to-string object)
         (or stream standard-output)))

(provide 'chronometrist-plist-pp)

;;; chronometrist-plist-pp.el ends here
