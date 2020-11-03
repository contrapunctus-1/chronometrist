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

(defun chronometrist-plist-pp-pair-p (cons)
  (and (listp cons) (not (listp (cdr cons)))))

(defun chronometrist-plist-pp-alist-p (list)
  (cl-loop for elt in list always (chronometrist-plist-pp-pair-p elt)))

(defun chronometrist-plist-pp-longest-keyword-length ()
  "Find the length of the longest keyword.
This assumes there is a single plist in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (down-list 1))
    (cl-loop with sexp
      while (setq sexp (ignore-errors (read (current-buffer))))
      when (keywordp sexp)
      maximize (length (symbol-name sexp)))))

(cl-defun chronometrist-plist-pp-indent-sexp (sexp &key (left-indent 0) (right-indent 0))
  "Return a string indenting SEXP by LEFT-INDENT and RIGHT-INDENT spaces."
  (chronometrist-sexp-delete-list)
  (format (concat (make-string left-indent ? )
                  "% -" (number-to-string right-indent) "s")
          sexp))

(cl-defun chronometrist-plist-pp-buffer (&optional (left-indent 0))
  "Naive pretty-printer for plists."
  (let (right-indent sexp)
    (goto-char (point-min))
    (while (not (eobp))
      (unless (looking-at-p ")")
        (setq sexp (save-excursion (read (current-buffer)))))
      (cond
       ((and (bobp) (json-plist-p sexp))
        ;; first keyword-value pair
        (setq right-indent (chronometrist-plist-pp-longest-keyword-length))
        (ignore-errors (down-list 1))
        (setq sexp (save-excursion (read (current-buffer))))
        ;; the first keyword does not need to be left-indented
        (insert (chronometrist-plist-pp-indent-sexp sexp :right-indent right-indent))
        (forward-sexp 1))
       ((looking-at-p ")")
        (if (bolp)
            (progn (delete-char -1) (forward-char 1))
          (forward-char)))
       ;; ((looking-at chronometrist-plist-pp-whitespace-re)
       ;;  (delete-region (match-beginning 0) (match-end 0)))
       ;; any other keyword
       ((keywordp sexp)
        (insert "\n " (chronometrist-plist-pp-indent-sexp sexp :left-indent left-indent
                                    :right-indent right-indent)))
       ;; we are before a list as a value
       ;; FIXME - not using (looking-at-p "(") results in extra
       ;; iterations; but that needs forward-char rather than
       ;; forward-sexp as the fallback
       ((listp sexp)
        ;; TODO - fix indentation
        (let ((sublist-indent (- (point) (point-at-bol))))
          (cond ((json-plist-p sexp)
                 (chronometrist-sexp-delete-list)
                 (insert (chronometrist-plist-pp-to-string sexp sublist-indent))
                 (insert "\n"))
                ((chronometrist-plist-pp-alist-p sexp)
                 (down-list)
                 (while (not (looking-at-p ")"))
                   (forward-sexp)
                   (insert "\n")))
                (t (forward-char)))))
       (t (forward-sexp))))))

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
