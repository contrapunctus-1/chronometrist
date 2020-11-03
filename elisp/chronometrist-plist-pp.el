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
  (format (concat (make-string left-indent ? )
                  "% -" (number-to-string right-indent) "s")
          sexp))

;; not a list? forward-sexp
;; if list ->
;;   plist -> plist indent;
;;   alist -> alist indent;
;;   else descend and call self for each element, then leave

(cl-defun chronometrist-plist-pp-buffer ()
  "Naive pretty-printer for plists."
  (while (not (looking-at-p ")"))
    (setq sexp (save-excursion (read (current-buffer))))
    (cond
     ((json-plist-p sexp) (chronometrist-plist-pp-plist-buffer) (forward-sexp))
     ((chronometrist-plist-pp-alist-p sexp) (chronometrist-plist-pp-alist-buffer) (forward-sexp))
     ((listp sexp) (down-list) (chronometrist-plist-pp-buffer))
     (t (forward-sexp))))
  (if (bolp) (progn (delete-char -1) (forward-char 1))
    (forward-char)))

(defun chronometrist-plist-pp-buffer-plist ()
  (down-list)
  (let ((right-indent (chronometrist-plist-pp-longest-keyword-length)) (first-p t) sexp)
    (while (not (looking-at-p ")"))
      (chronometrist-plist-pp-normalize-whitespace)
      (setq sexp (save-excursion (read (current-buffer))))
      (cond ((keywordp sexp)
             (chronometrist-sexp-delete-list)
             (insert (if first-p "" " ")
                     (chronometrist-plist-pp-indent-sexp sexp :right-indent right-indent))
             (setq first-p nil))
            ;; value
            ((listp sexp) (chronometrist-plist-pp-buffer))
            (t (forward-sexp)
               (insert "\n"))))
    (when (bolp) (delete-char -1))
    (up-list)))

(defun chronometrist-plist-pp-buffer-alist ()
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

(cl-defun chronometrist-plist-pp-buffer (&optional (left-indent 0) in-sublist-p)
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
          (forward-char))
        (setq in-sublist-p nil))
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
        (let ((sublist-indent (if in-sublist-p left-indent (chronometrist-plist-pp-column))))
          (cond ((json-plist-p sexp)
                 (chronometrist-sexp-delete-list)
                 (insert (chronometrist-plist-pp-to-string sexp sublist-indent))
                 (insert "\n"))
                ((chronometrist-plist-pp-alist-p sexp)
                 (down-list)
                 (while (not (looking-at-p ")"))
                   (forward-sexp)
                   (insert "\n")))
                (t (down-list)
                   (setq in-sublist-p t
                         left-indent (chronometrist-plist-pp-column))))))
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
