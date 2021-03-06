;;; chronometrist-tests.el --- Tests for Chronometrist -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; For more information, please refer to <https://unlicense.org>

;;; Commentary:
;; These should be replaced by buttercup tests, which have a nicer
;; syntax.

(require 'ert)
(require 'chronometrist)

;; TODO - add tests for chronometrist-task-time-one-day with custom day start
;; times.

;; #### CHRONOMETRIST-REPORT ####

;;; Code:

(defun interval-test (start target)
  "Basic logic used to derive 'gap' in `chronometrist-previous-week-start'."
  (cond ((= start target) 7)
        ((> start target) (- start target))
        ((< start target) (+ start (- 7 target)))))

(ert-deftest chronometrist-interval-test-0 ()
  (should (= (interval-test 0 0) 7))
  (should (= (interval-test 0 1) 6))
  (should (= (interval-test 0 2) 5))
  (should (= (interval-test 0 3) 4))
  (should (= (interval-test 0 4) 3))
  (should (= (interval-test 0 5) 2))
  (should (= (interval-test 0 6) 1)))

(ert-deftest chronometrist-interval-test-1 ()
  (should (= (interval-test 1 0) 1))
  (should (= (interval-test 1 1) 7))
  (should (= (interval-test 1 2) 6))
  (should (= (interval-test 1 3) 5))
  (should (= (interval-test 1 4) 4))
  (should (= (interval-test 1 5) 3))
  (should (= (interval-test 1 6) 2)))

(ert-deftest chronometrist-interval-test-2 ()
  (should (= (interval-test 2 0) 2))
  (should (= (interval-test 2 1) 1))
  (should (= (interval-test 2 2) 7))
  (should (= (interval-test 2 3) 6))
  (should (= (interval-test 2 4) 5))
  (should (= (interval-test 2 5) 4))
  (should (= (interval-test 2 6) 3)))

(ert-deftest chronometrist-interval-test-3 ()
  (should (= (interval-test 3 0) 3))
  (should (= (interval-test 3 1) 2))
  (should (= (interval-test 3 2) 1))
  (should (= (interval-test 3 3) 7))
  (should (= (interval-test 3 4) 6))
  (should (= (interval-test 3 5) 5))
  (should (= (interval-test 3 6) 4)))

(ert-deftest chronometrist-interval-test-4 ()
  (should (= (interval-test 4 0) 4))
  (should (= (interval-test 4 1) 3))
  (should (= (interval-test 4 2) 2))
  (should (= (interval-test 4 3) 1))
  (should (= (interval-test 4 4) 7))
  (should (= (interval-test 4 5) 6))
  (should (= (interval-test 4 6) 5)))

(ert-deftest chronometrist-interval-test-5 ()
  (should (= (interval-test 5 0) 5))
  (should (= (interval-test 5 1) 4))
  (should (= (interval-test 5 2) 3))
  (should (= (interval-test 5 3) 2))
  (should (= (interval-test 5 4) 1))
  (should (= (interval-test 5 5) 7))
  (should (= (interval-test 5 6) 6)))

(ert-deftest chronometrist-interval-test-6 ()
  (should (= (interval-test 6 0) 6))
  (should (= (interval-test 6 1) 5))
  (should (= (interval-test 6 2) 4))
  (should (= (interval-test 6 3) 3))
  (should (= (interval-test 6 4) 2))
  (should (= (interval-test 6 5) 1))
  (should (= (interval-test 6 6) 7)))

(ert-deftest chronometrist-previous-week-start-sunday ()
  "Tests for `chronometrist-previous-week-start'."
  (let ((chronometrist-report-week-start-day "Sunday"))
    (should (equal (chronometrist-previous-week-start '(0 0 0 1 9 2018 6 nil 19800))
                   '(0 0 0 26 8 2018 0 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 2 9 2018 0 nil 19800))
                   '(0 0 0 2  9 2018 0 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 3 9 2018 1 nil 19800))
                   '(0 0 0 2  9 2018 0 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 4 9 2018 2 nil 19800))
                   '(0 0 0 2  9 2018 0 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 5 9 2018 3 nil 19800))
                   '(0 0 0 2  9 2018 0 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 6 9 2018 4 nil 19800))
                   '(0 0 0 2  9 2018 0 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 7 9 2018 5 nil 19800))
                   '(0 0 0 2  9 2018 0 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 8 9 2018 6 nil 19800))
                   '(0 0 0 2  9 2018 0 nil 19800)))))

(ert-deftest chronometrist-previous-week-start-monday ()
  "Tests for `chronometrist-previous-week-start'."
  (let ((chronometrist-report-week-start-day "Monday"))
    (should (equal (chronometrist-previous-week-start '(0 0 0 1 9 2018 6 nil 19800))
                   '(0 0 0 27 8 2018 1 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 2 9 2018 0 nil 19800))
                   '(0 0 0 27 8 2018 1 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 3 9 2018 1 nil 19800))
                   '(0 0 0 3  9 2018 1 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 4 9 2018 2 nil 19800))
                   '(0 0 0 3  9 2018 1 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 5 9 2018 3 nil 19800))
                   '(0 0 0 3  9 2018 1 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 6 9 2018 4 nil 19800))
                   '(0 0 0 3  9 2018 1 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 7 9 2018 5 nil 19800))
                   '(0 0 0 3  9 2018 1 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 8 9 2018 6 nil 19800))
                   '(0 0 0 3  9 2018 1 nil 19800)))))

(ert-deftest chronometrist-previous-week-start-tuesday ()
  "Tests for `chronometrist-previous-week-start'."
  (let ((chronometrist-report-week-start-day "Tuesday"))
    (should (equal (chronometrist-previous-week-start '(0 0 0 1 9 2018 6 nil 19800))
                   '(0 0 0 28 8 2018 2 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 2 9 2018 0 nil 19800))
                   '(0 0 0 28 8 2018 2 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 3 9 2018 1 nil 19800))
                   '(0 0 0 28 8 2018 2 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 4 9 2018 2 nil 19800))
                   '(0 0 0 4  9 2018 2 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 5 9 2018 3 nil 19800))
                   '(0 0 0 4  9 2018 2 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 6 9 2018 4 nil 19800))
                   '(0 0 0 4  9 2018 2 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 7 9 2018 5 nil 19800))
                   '(0 0 0 4  9 2018 2 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 8 9 2018 6 nil 19800))
                   '(0 0 0 4  9 2018 2 nil 19800)))))

(ert-deftest chronometrist-previous-week-start-wednesday ()
  "Tests for `chronometrist-previous-week-start'."
  (let ((chronometrist-report-week-start-day "Wednesday"))
    (should (equal (chronometrist-previous-week-start '(0 0 0 1 9 2018 6 nil 19800))
                   '(0 0 0 29 8 2018 3 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 2 9 2018 0 nil 19800))
                   '(0 0 0 29 8 2018 3 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 3 9 2018 1 nil 19800))
                   '(0 0 0 29 8 2018 3 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 4 9 2018 2 nil 19800))
                   '(0 0 0 29 8 2018 3 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 5 9 2018 3 nil 19800))
                   '(0 0 0 5  9 2018 3 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 6 9 2018 4 nil 19800))
                   '(0 0 0 5  9 2018 3 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 7 9 2018 5 nil 19800))
                   '(0 0 0 5  9 2018 3 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 8 9 2018 6 nil 19800))
                   '(0 0 0 5  9 2018 3 nil 19800)))))

(ert-deftest chronometrist-previous-week-start-thursday ()
  "Tests for `chronometrist-previous-week-start'."
  (let ((chronometrist-report-week-start-day "Thursday"))
    (should (equal (chronometrist-previous-week-start '(0 0 0 1 9 2018 6 nil 19800))
                   '(0 0 0 30 8 2018 4 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 2 9 2018 0 nil 19800))
                   '(0 0 0 30 8 2018 4 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 3 9 2018 1 nil 19800))
                   '(0 0 0 30 8 2018 4 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 4 9 2018 2 nil 19800))
                   '(0 0 0 30 8 2018 4 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 5 9 2018 3 nil 19800))
                   '(0 0 0 30 8 2018 4 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 6 9 2018 4 nil 19800))
                   '(0 0 0 6  9 2018 4 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 7 9 2018 5 nil 19800))
                   '(0 0 0 6  9 2018 4 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 8 9 2018 6 nil 19800))
                   '(0 0 0 6  9 2018 4 nil 19800)))))

(ert-deftest chronometrist-previous-week-start-friday ()
  "Tests for `chronometrist-previous-week-start'."
  (let ((chronometrist-report-week-start-day "Friday"))
    (should (equal (chronometrist-previous-week-start '(0 0 0 1 9 2018 6 nil 19800))
                   '(0 0 0 31 8 2018 5 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 2 9 2018 0 nil 19800))
                   '(0 0 0 31 8 2018 5 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 3 9 2018 1 nil 19800))
                   '(0 0 0 31 8 2018 5 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 4 9 2018 2 nil 19800))
                   '(0 0 0 31 8 2018 5 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 5 9 2018 3 nil 19800))
                   '(0 0 0 31 8 2018 5 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 6 9 2018 4 nil 19800))
                   '(0 0 0 31 8 2018 5 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 7 9 2018 5 nil 19800))
                   '(0 0 0 7  9 2018 5 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 8 9 2018 6 nil 19800))
                   '(0 0 0 7  9 2018 5 nil 19800)))))

(ert-deftest chronometrist-previous-week-start-saturday ()
  "Tests for `chronometrist-previous-week-start'."
  (let ((chronometrist-report-week-start-day "Saturday"))
    (should (equal (chronometrist-previous-week-start '(0 0 0 30 8 2018 4 nil 19800))
                   '(0 0 0 25 8 2018 6 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 31 8 2018 5 nil 19800))
                   '(0 0 0 25 8 2018 6 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 2 9 2018 0 nil 19800))
                   '(0 0 0 1  9 2018 6 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 3 9 2018 1 nil 19800))
                   '(0 0 0 1  9 2018 6 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 4 9 2018 2 nil 19800))
                   '(0 0 0 1  9 2018 6 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 5 9 2018 3 nil 19800))
                   '(0 0 0 1  9 2018 6 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 6 9 2018 4 nil 19800))
                   '(0 0 0 1  9 2018 6 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 7 9 2018 5 nil 19800))
                   '(0 0 0 1  9 2018 6 nil 19800)))
    (should (equal (chronometrist-previous-week-start '(0 0 0 8 9 2018 6 nil 19800))
                   '(0 0 0 8  9 2018 6 nil 19800)))))

;; #### CHRONOMETRIST-COMMON ####

(ert-deftest chronometrist-ptod-tests ()
  "Tests for `chronometrist-task-time-one-day'."
  (let ((timeclock-file "test.timelog"))
    (timeclock-reread-log)
    ;; basic 1 hour test
    (should (equal (chronometrist-task-time-one-day "Programming" '(0 0 0 1 1 2018))
                   [1 0 0]))
    (should (equal (chronometrist-task-time-one-day "Swimming"    '(0 0 0 1 1 2018))
                   [1 0 0]))
    (should (equal (chronometrist-task-time-one-day "Cooking"     '(0 0 0 1 1 2018))
                   [1 0 0]))
    (should (equal (chronometrist-task-time-one-day "Guitar"      '(0 0 0 1 1 2018))
                   [1 0 0]))
    (should (equal (chronometrist-task-time-one-day "Cycling"     '(0 0 0 1 1 2018))
                   [1 0 0]))
    ;; across midnight
    (should (equal (chronometrist-task-time-one-day "Programming" '(0 0 0 2 1 2018))
                   [1 0 0]))
    (should (equal (chronometrist-task-time-one-day "Programming" '(0 0 0 3 1 2018))
                   [1 0 0]))))

(ert-deftest chronometrist-ptod-midnight-clocked-in ()
  "Tests for `chronometrist-task-time-one-day' behaviour
across midnight + when not clocked out."
  :expected-result :failed
  (let ((timeclock-file "test2.timelog"))
    (timeclock-reread-log)
    (should (equal (chronometrist-task-time-one-day "Test" '(0 0 0 1 1 2018))
                   [1 0 0]))
    (should (equal (chronometrist-task-time-one-day "Test" '(0 0 0 2 1 2018))
                   [24 0 0]))))

;; #### CHRONOMETRIST ####

(ert-deftest chronometrist-seconds-to-hms-tests ()
  "Tests for `chronometrist-seconds-to-hms'."
  (should (equal (chronometrist-seconds-to-hms 1)
                 [0 0 1]))
  (should (equal (chronometrist-seconds-to-hms 60)
                 [0 1 0]))
  (should (equal (chronometrist-seconds-to-hms 61)
                 [0 1 1]))
  (should (equal (chronometrist-seconds-to-hms 3600)
                 [1 0 0]))
  (should (equal (chronometrist-seconds-to-hms 3660)
                 [1 1 0]))
  (should (equal (chronometrist-seconds-to-hms 3661)
                 [1 1 1])))

(ert-deftest chronometrist-time-add-tests ()
  "Tests for `chronometrist-time-add'."
  (should (equal (chronometrist-time-add [0 0 0] [0 0 0])
                 [0 0 0]))
  (should (equal (chronometrist-time-add [0 0 1] [0 0 0])
                 [0 0 1]))
  (should (equal (chronometrist-time-add [0 0 1] [0 0 59])
                 [0 1 0]))
  (should (equal (chronometrist-time-add [0 1 0] [0 0 1])
                 [0 1 1]))
  (should (equal (chronometrist-time-add [0 1 1] [0 59 59])
                 [1 1 0])))

(ert-deftest chronometrist-ttod-tests ()
  "Tests for `chronometrist-active-time-one-day'."
  (let ((timeclock-file "test.timelog"))
    (timeclock-reread-log)
    ;; 1 hour per activity test
    (should (equal (chronometrist-active-time-one-day '(0 0 0 1 1 2018))
                   [5 0 0]))
    ;; pan-midnight tests
    (should (equal (chronometrist-active-time-one-day '(0 0 0 2 1 2018))
                   [1 0 0]))
    (should (equal (chronometrist-active-time-one-day '(0 0 0 3 1 2018))
                   [1 0 0]))
    ;; 1 second test
    (should (equal (chronometrist-active-time-one-day '(0 0 0 4 1 2018))
                   [0 0 1]))))

(ert-deftest chronometrist-format-time-tests ()
  "Tests for `chronometrist-format-time'."
  (should (equal (chronometrist-format-time '( 0  0  0))
                 "       -"))
  (should (equal (chronometrist-format-time '( 0  0  1))
                 "       1"))
  (should (equal (chronometrist-format-time '( 0  0 10))
                 "      10"))
  (should (equal (chronometrist-format-time '( 0  1 10))
                 "    1:10"))
  (should (equal (chronometrist-format-time '( 0 10 10))
                 "   10:10"))
  (should (equal (chronometrist-format-time '( 1 10 10))
                 " 1:10:10"))
  (should (equal (chronometrist-format-time '(10 10 10))
                 "10:10:10"))

  (should (equal (chronometrist-format-time '[ 0  0  0])
                 "       -"))
  (should (equal (chronometrist-format-time '[ 0  0  1])
                 "       1"))
  (should (equal (chronometrist-format-time '[ 0  0 10])
                 "      10"))
  (should (equal (chronometrist-format-time '[ 0  1 10])
                 "    1:10"))
  (should (equal (chronometrist-format-time '[ 0 10 10])
                 "   10:10"))
  (should (equal (chronometrist-format-time '[ 1 10 10])
                 " 1:10:10"))
  (should (equal (chronometrist-format-time '[10 10 10])
                 "10:10:10")))

(ert-deftest chronometrist-report-iodd-tests ()
  (should (equal (chronometrist-date-op '(2020 2 28) '+)
                 '(2020 2 29))))

(provide 'chronometrist-tests)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

;;; chronometrist-tests.el ends here
