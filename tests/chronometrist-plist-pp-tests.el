;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'chronometrist-plist-pp)

(describe "chronometrist-plist-pp-buffer"
  :var ((buffer (find-file-noselect "tests/plist-pp-test.sexp")))
  (it "indents plists nicely"
    (expect
     (with-current-buffer buffer
       (chronometrist-plist-pp-buffer)
       (buffer-substring (point-min) (point-max)))
     :to-equal
     (concat
      "(:name    \"Task\"\n"
      " :tags    (foo bar)\n"
      " :comment ((70 . \"baz\")\n"
      "           \"zot\"\n"
      "           (16 . \"frob\")\n"
      "           (20 20 \"quux\"))\n"
      " :start   \"2020-06-25T19:27:57+0530\"\n"
      " :stop    \"2020-06-25T19:43:30+0530\")\n"))))

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:
