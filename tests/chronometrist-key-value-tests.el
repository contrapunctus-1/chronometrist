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

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:
