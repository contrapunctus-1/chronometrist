(require 'chronometrist-key-values)

(ert-deftest chronometrist-plist-remove ()
  (should
   (equal (chronometrist-plist-remove '(:a 1 :b 2 :c 3 :d 4) :a)
          '(:b 2 :c 3 :d 4)))
  (should
   (equal (chronometrist-plist-remove '(:a 1 :b 2 :c 3 :d 4) :b)
          '(:a 1 :c 3 :d 4)))
  (should
   (equal (chronometrist-plist-remove '(:a 1 :b 2 :c 3 :d 4) :c)
          '(:a 1 :b 2 :d 4)))
  (should
   (equal (chronometrist-plist-remove '(:a 1 :b 2 :c 3 :d 4) :d)
          '(:a 1 :b 2 :c 3)))
  (should
   (equal (chronometrist-plist-remove '(:a 1 :b 2 :c 3 :d 4) :a :b)
          '(:c 3 :d 4)))
  (should
   (equal (chronometrist-plist-remove '(:a 1 :b 2 :c 3 :d 4) :a :d)
          '(:b 2 :c 3)))
  (should
   (equal (chronometrist-plist-remove '(:a 1 :b 2 :c 3 :d 4) :c :d)
          '(:a 1 :b 2)))
  (should (equal
           (chronometrist-plist-remove '(:a 1 :b 2 :c 3 :d 4) :a :b :c :d)
           nil))
  (should
   (equal (chronometrist-plist-remove '(:a 1 :b 2 :c 3 :d 4) :d :a)
          '(:b 2 :c 3))))

(ert-deftest chronometrist-tags-history ()
  (let ((_update (progn
                   (clrhash chronometrist-tags-history)
                   (cl-loop for task in '("Guitar" "Programming") do
                     (chronometrist-tags-history-populate task chronometrist-tags-history "test.sexp")))))
    (should
     (= (hash-table-count chronometrist-tags-history) 2))
    (should
     (cl-loop for task being the hash-keys of chronometrist-tags-history
       always (stringp task)))
    (should
     (equal (gethash "Guitar" chronometrist-tags-history)
            '((classical solo)
              (classical warm-up))))
    (should
     (equal (gethash "Programming" chronometrist-tags-history)
            '((reading) (bug-hunting))))))

(ert-deftest chronometrist-key-history ()
  (before-all
   (setq chronometrist-file "tests/test.sexp")
   (chronometrist-events-populate)
   (setq chronometrist-task-list (chronometrist-tasks-from-table))
   (chronometrist-key-history-populate chronometrist-events chronometrist-key-history))
  (it "should
have 6 hash keys"
      (should
       (hash-table-count chronometrist-key-history)
              :to-be 6))
  (it "should
store multiple values"
      (should
       (length (gethash "Programming" chronometrist-key-history))
              :to-be 3)
      (should
       (length (gethash "Arrangement/new edition" chronometrist-key-history))
              :to-be 2)))

(ert-deftest chronometrist-value-history ()
  (before-all
   (setq chronometrist-file "tests/test.sexp")
   (chronometrist-events-populate)
   (chronometrist-value-history-populate chronometrist-events chronometrist-value-history))
  (it "should
have 5 hash keys"
      (should
       (hash-table-count chronometrist-value-history)
              :to-be 5)
      (should
       (cl-loop for task being the hash-keys of chronometrist-value-history
                always (stringp task))
              :to-be t)))

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:
