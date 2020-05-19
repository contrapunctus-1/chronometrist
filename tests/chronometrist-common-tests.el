;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'chronometrist-common)

(describe
 "chronometrist-format-time"
 (it "works with lists"
     (expect (chronometrist-format-time '( 0  0  0))
             :to-be "       -")
     (expect (chronometrist-format-time '( 0  0  1))
             :to-be "       1")
     (expect (chronometrist-format-time '( 0  0 10))
             :to-be "      10")
     (expect (chronometrist-format-time '( 0  1 10))
             :to-be "    1:10")
     (expect (chronometrist-format-time '( 0 10 10))
             :to-be "   10:10")
     (expect (chronometrist-format-time '( 1 10 10))
             :to-be " 1:10:10")
     (expect (chronometrist-format-time '(10 10 10))
             :to-be "10:10:10"))
 (it "also works with vectors"
     (expect (chronometrist-format-time '[ 0  0  0])
             :to-be "       -")
     (expect (chronometrist-format-time '[ 0  0  1])
             :to-be "       1")
     (expect (chronometrist-format-time '[ 0  0 10])
             :to-be "      10")
     (expect (chronometrist-format-time '[ 0  1 10])
             :to-be "    1:10")
     (expect (chronometrist-format-time '[ 0 10 10])
             :to-be "   10:10")
     (expect (chronometrist-format-time '[ 1 10 10])
             :to-be " 1:10:10")
     (expect (chronometrist-format-time '[10 10 10])
             :to-be "10:10:10")))

(describe
 "chronometrist-previous-week-start"
 :var ((chronometrist-report-week-start-day "Sunday")
       (ts (chronometrist-iso-date->ts "2018-09-02")))
 (it "should work with Sundays"
     (should (ts= (chronometrist-previous-week-start
                   (chronometrist-iso-date->ts "2018-09-01"))
                  (chronometrist-iso-date->ts "2018-08-26")))
     (should (ts= ts (chronometrist-previous-week-start (chronometrist-iso-date->ts "2018-09-02"))))
     (should (ts= ts (chronometrist-previous-week-start (chronometrist-iso-date->ts "2018-09-03"))))
     (should (ts= ts (chronometrist-previous-week-start (chronometrist-iso-date->ts "2018-09-04"))))
     (should (ts= ts (chronometrist-previous-week-start (chronometrist-iso-date->ts "2018-09-05"))))
     (should (ts= ts (chronometrist-previous-week-start (chronometrist-iso-date->ts "2018-09-06"))))
     (should (ts= ts (chronometrist-previous-week-start (chronometrist-iso-date->ts "2018-09-07"))))
     (should (ts= ts (chronometrist-previous-week-start (chronometrist-iso-date->ts "2018-09-08"))))))
