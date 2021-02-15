;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'chronometrist)

(describe
 "chronometrist-format-time"
 (it "works with lists"
     (expect (chronometrist-format-time 0)
             :to-equal "       -")
     (expect (chronometrist-format-time 1)
             :to-equal "       1")
     (expect (chronometrist-format-time 10)
             :to-equal "      10")
     (expect (chronometrist-format-time 70)
             :to-equal "    1:10")
     (expect (chronometrist-format-time (+ (* 10 60) ;; 10 minutes
                               10))      ;; 10 seconds
             :to-equal "   10:10")
     (expect (chronometrist-format-time (+ (* 1 60 60) ;; 1 hour
                               (* 10 60)   ;; 10 minutes
                               10))        ;; 10 seconds
             :to-equal " 1:10:10")
     (expect (chronometrist-format-time (+ (* 10 60 60) ;; 10 hours
                               (* 10 60)    ;; 10 minutes
                               10))         ;; 10 seconds
             :to-equal "10:10:10")))

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
