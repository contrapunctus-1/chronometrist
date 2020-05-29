;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'chronometrist-sexp)

(describe "chronometrist-sexp-last"
  :var ((chronometrist-file "tests/test.sexp"))
  (it "should return a plist"
      (expect (consp (chronometrist-sexp-last))
              :to-be t)))

(describe "chronometrist-sexp-read"
  :var ((ts (chronometrist-iso-date->ts "2020-05-10")))
  (before-all (setq chronometrist-file-old chronometrist-file
                    chronometrist-file     "tests/test.sexp"))
  (after-all  (setq chronometrist-file chronometrist-file-old))
  (it "returns all events if no arguments are given"
    (expect (length
             (chronometrist-sexp-read))
            :to-equal 11))
  (it "returns events between a certain time"
    (expect (length
             (chronometrist-sexp-read (chronometrist-iso-date->ts "2020-05-10")
                         (chronometrist-iso-date->ts "2020-05-11")))
            :to-equal 3))
  (it "splits events if they cross the given times"
    (expect (chronometrist-sexp-read (chronometrist-iso-date->ts "2018-01-03")
                        (chronometrist-iso-date->ts "2018-01-04"))
            :to-equal
            '((:name "Cooking"
                     :start "2018-01-03T23:00:00+0530"
                     :stop  "2018-01-04T00:00:00+0530")
              (:name "Programming"
                     :start "2018-01-03T00:00:00+0530"
                     :stop  "2018-01-03T01:00:00+0530")))))

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:
