;;; chronometrist-key-values.el --- add key-values to Chronometrist data -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Keywords: calendar
;; Homepage: gemini://tilde.team/~contrapunctus/software.gmi
;; Package-Requires: ((chronometrist "0.5.0") (choice "0.0.1"))
;; Version: 0.1.0

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
;; This package lets users attach tags and key-values to their tracked time, similar to tags and properties in Org mode.
;;
;; To use, add one or more of these functions to any chronometrist hook except `chronometrist-before-in-functions`.
;; * `completing-read'-based - `chronometrist-tags-add` and/or `chronometrist-kv-add'
;; * `choice'-based (Hydra-like) - `chronometrist-unified-choice'

;;; Code:
(require 'literate-elisp)

(literate-elisp-load
 (format "%schronometrist-key-values.org" (file-name-directory load-file-name)))

(provide 'chronometrist-key-values)
;;; chronometrist-key-values.el ends here
