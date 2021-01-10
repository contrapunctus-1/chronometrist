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

(ert-deftest chronometrist-plist-update ()
  (let ((test-plist-1 '(:name "Old name"
                              :tags (foo)
                              :key1 "val 1"
                              :start "2021-01-10T22:59:23+0530"
                              :stop "2021-01-10T22:59:27+0530"))
        (test-plist-2 '(:name "New name" :tags (bar)
                              :key1 "new val 1"
                              :key2 "val 2"
                              :start "2021-01-10T22:59:23+0530"
                              :stop "2021-01-10T22:59:27+0530")))
    ;; :name, :start, and :stop should not be updated
    ;; same keys should be updated
    ;; new keys should be added
    ;; old tags should be preserved
    ;; new tags should be added
    (should (equal (chronometrist-plist-update test-plist-1 test-plist-2)
                   '(:name "Old name"
                           :tags (foo bar)
                           :key1 "new val 1"
                           :key2 "val 2"
                           :start "2021-01-10T22:59:23+0530"
                           :stop "2021-01-10T22:59:27+0530")))))

(ert-deftest chronometrist-tags-history ()
  (progn
    (clrhash chronometrist-tags-history)
    (cl-loop for task in '("Guitar" "Programming") do
      (chronometrist-tags-history-populate task chronometrist-tags-history "test.sexp")))
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
          '((reading) (bug-hunting)))))

(ert-deftest chronometrist-key-history ()
  (progn
    (clrhash chronometrist-key-history)
    (cl-loop for task in '("Programming" "Arrangement/new edition") do
      (chronometrist-key-history-populate task chronometrist-key-history "test.sexp")))
  (should (= (hash-table-count chronometrist-key-history) 2))
  (should (= (length (gethash "Programming" chronometrist-key-history)) 3))
  (should (= (length (gethash "Arrangement/new edition" chronometrist-key-history)) 2)))

(ert-deftest chronometrist-value-history ()
  (progn
    (clrhash chronometrist-value-history)
    (chronometrist-value-history-populate chronometrist-value-history "test.sexp"))
  (should (= (hash-table-count chronometrist-value-history) 5))
  (should
   (cl-loop for task being the hash-keys of chronometrist-value-history
     always (stringp task))))

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:
