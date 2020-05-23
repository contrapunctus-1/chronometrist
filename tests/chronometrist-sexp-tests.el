;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'chronometrist-sexp)

(describe "chronometrist-sexp-last"
  :var ((chronometrist-file "tests/test.sexp"))
  (it "should return a plist"
      (expect (consp (chronometrist-sexp-last))
              :to-be t)))

(describe "chronometrist-sexp-between"
  :var ((ts (chronometrist-iso-date->ts "2020-05-10")))
  (before-all
   (setq chronometrist-file-old chronometrist-file
         chronometrist-file     "tests/test.sexp"))
  (after-all
   (setq chronometrist-file chronometrist-file-old))
  (it "returns events between a certain time"
      (expect (length
               (chronometrist-sexp-between (chronometrist-iso-date->ts "2020-05-10")
                              (chronometrist-iso-date->ts "2020-05-11")))
              :to-equal 3)))

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:
