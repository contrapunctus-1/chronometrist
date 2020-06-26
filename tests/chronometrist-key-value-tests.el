(require 'buttercup)
(require 'chronometrist-key-values)

(describe
 "chronometrist-plist-remove"
 (it "works with one key"
     (expect (chronometrist-plist-remove '(:a 1 :b 2 :c 3 :d 4)
                            :a)
             :to-equal '(:b 2 :c 3 :d 4))
     (expect (chronometrist-plist-remove '(:a 1 :b 2 :c 3 :d 4)
                            :b)
             :to-equal '(:a 1 :c 3 :d 4))
     (expect (chronometrist-plist-remove '(:a 1 :b 2 :c 3 :d 4)
                            :c)
             :to-equal '(:a 1 :b 2 :d 4))
     (expect (chronometrist-plist-remove '(:a 1 :b 2 :c 3 :d 4)
                            :d)
             :to-equal '(:a 1 :b 2 :c 3)))
 (it "works with multiple keys"
     (expect (chronometrist-plist-remove '(:a 1 :b 2 :c 3 :d 4)
                            :a :b)
             :to-equal '(:c 3 :d 4))
     (expect (chronometrist-plist-remove '(:a 1 :b 2 :c 3 :d 4)
                            :a :d)
             :to-equal '(:b 2 :c 3))
     (expect (chronometrist-plist-remove '(:a 1 :b 2 :c 3 :d 4)
                            :c :d)
             :to-equal '(:a 1 :b 2))
     (expect (chronometrist-plist-remove '(:a 1 :b 2 :c 3 :d 4)
                            :a :b :c :d)
             :to-equal nil))
 (it "works with keys in any order"
     (expect (chronometrist-plist-remove '(:a 1 :b 2 :c 3 :d 4)
                            :d :a)
             :to-equal '(:b 2 :c 3))))

(describe
 "chronometrist-key-history"
 (before-all
  (setq chronometrist-file "tests/test.sexp")
  (chronometrist-events-populate)
  (setq chronometrist-task-list (chronometrist-tasks-from-table))
  (chronometrist-key-history-populate))
 (it "should have 6 keys"
     (expect (hash-table-count chronometrist-key-history)
             :to-be 6))
 (it "should store multiple values"
     (expect (length (gethash "Programming" chronometrist-key-history))
             :to-be 3)
     (expect (length (gethash "Arrangement/new edition" chronometrist-key-history))
             :to-be 2)))

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:
