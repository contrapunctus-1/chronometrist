;;; chronometrist-statistics-custom.el --- Custom definitions for chronometrist-statistics -*- lexical-binding: t; -*-

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
;;

;;; Code:

(defgroup chronometrist-statistics nil
  "Statistics buffer for the `chronometrist' time tracker."
  :group 'chronometrist)

(defcustom chronometrist-statistics-buffer-name "*Chronometrist-Statistics*"
  "The name of the buffer created by `chronometrist-statistics'."
  :type 'string)

(provide 'chronometrist-statistics-custom)

;;; chronometrist-statistics-custom.el ends here
