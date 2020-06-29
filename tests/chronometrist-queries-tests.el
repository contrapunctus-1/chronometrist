;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'ts)
(require 'chronometrist-sexp)
(require 'chronometrist-events)
(require 'chronometrist-queries)
(require 'chronometrist-time)


(describe "chronometrist-task-time-one-day"
  :var ((ts-1 (chronometrist-iso-date->ts "2018-01-01"))
        (ts-2 (chronometrist-iso-date->ts "2018-01-02"))
        (ts-3 (chronometrist-iso-date->ts "2018-01-03"))
        (1-hour 3600))
  (before-all
    (setq chronometrist-file-old chronometrist-file
          chronometrist-file     "tests/test.sexp")
    (chronometrist-events-populate))
  (after-all
    (setq chronometrist-file chronometrist-file-old))
  (it "returns the time spent in one day, in seconds"
    (expect (chronometrist-task-time-one-day "Programming" ts-1)
            :to-equal 1-hour)
    (expect (chronometrist-task-time-one-day "Swimming"    ts-1)
            :to-equal 1-hour)
    (expect (chronometrist-task-time-one-day "Cooking"     ts-1)
            :to-equal 1-hour)
    (expect (chronometrist-task-time-one-day "Guitar"      ts-1)
            :to-equal 1-hour)
    (expect (chronometrist-task-time-one-day "Cycling"     ts-1)
            :to-equal 1-hour))

  (it "works with midnight-crossing events"
    (expect (chronometrist-task-time-one-day "Programming" ts-2)
            :to-equal 1-hour)
    (expect (chronometrist-task-time-one-day "Programming" ts-3)
            :to-equal 1-hour)))

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:
