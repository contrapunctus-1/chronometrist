;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'chronometrist)

(describe "chronometrist-plist-pp-buffer"
  :var ((buffer (find-file-noselect "tests/plist-pp-test.sexp")))
  (it "indents plists nicely"
    (expect
     (chronometrist-plist-pp-to-string
      '(:name "Task"
        :tags (foo bar)
        :comment ((70 . "baz")
                  "zot"
                  (16 . "frob")
                  (20 20 "quux"))
        :start "2020-06-25T19:27:57+0530"
        :stop "2020-06-25T19:43:30+0530"))
     :to-equal
     (concat
      "(:name    \"Task\"\n"
      " :tags    (foo bar)\n"
      " :comment ((70 . \"baz\")\n"
      "           \"zot\"\n"
      "           (16 . \"frob\")\n"
      "           (20 20 \"quux\"))\n"
      " :start   \"2020-06-25T19:27:57+0530\"\n"
      " :stop    \"2020-06-25T19:43:30+0530\")\n"))
    (expect
     (chronometrist-plist-pp-to-string
      '(:name  "Singing"
        :tags  (classical solo)
        :piece ((:composer "Gioachino Rossini"
                           :name     "Il barbiere di Siviglia"
                           :aria     ("All'idea di quel metallo" "Dunque io son"))
                (:composer "Ralph Vaughan Williams"
                           :name     "Songs of Travel"
                           :movement ((4 . "Youth and Love")
                                      (5 . "In Dreams")
                                      (7 . "Wither Must I Wander?")))
                (:composer "Ralph Vaughan Williams"
                           :name     "Merciless Beauty"
                           :movement 1)
                (:composer "Franz Schubert"
                           :name     "Winterreise"
                           :movement ((1 . "Gute Nacht")
                                      (2 . "Die Wetterfahne")
                                      (4 . "Erstarrung"))))
        :start "2020-11-01T12:01:20+0530"
        :stop  "2020-11-01T13:08:32+0530"))
     :to-equal
     (concat
      "(:name  \"Singing\"\n"
      " :tags  (classical solo)\n"
      " :piece ((:composer \"Gioachino Rossini\"\n"
      "          :name     \"Il barbiere di Siviglia\"\n"
      "          :aria     (\"All'idea di quel metallo\" \"Dunque io son\"))\n"
      "         (:composer \"Ralph Vaughan Williams\"\n"
      "          :name     \"Songs of Travel\"\n"
      "          :movement ((4 . \"Youth and Love\")\n"
      "                     (5 . \"In Dreams\")\n"
      "                     (7 . \"Wither Must I Wander?\")))\n"
      "         (:composer \"Ralph Vaughan Williams\"\n"
      "          :name     \"Merciless Beauty\"\n"
      "          :movement 1)\n"
      "         (:composer \"Franz Schubert\"\n"
      "          :name     \"Winterreise\"\n"
      "          :movement ((1 . \"Gute Nacht\")\n"
      "                     (2 . \"Die Wetterfahne\")\n"
      "                     (4 . \"Erstarrung\"))))\n"
      " :start \"2020-11-01T12:01:20+0530\"\n"
      " :stop  \"2020-11-01T13:08:32+0530\")"))
    (expect
     (chronometrist-plist-pp-to-string
      '(:name "Cooking"
        :tags (lunch)
        :recipe (:name "moong-masoor ki dal"
                       :url "https://www.mirchitales.com/moong-masoor-dal-red-and-yellow-lentil-curry/")
        :start "2020-09-23T15:22:39+0530"
        :stop "2020-09-23T16:29:49+0530"))
     :to-equal
     (concat
      "(:name   \"Cooking\""
      " :tags   (lunch)"
      " :recipe (:name \"moong-masoor ki dal\""
      "          :url  \"https://www.mirchitales.com/moong-masoor-dal-red-and-yellow-lentil-curry/\")"
      " :start  \"2020-09-23T15:22:39+0530\""
      " :stop   \"2020-09-23T16:29:49+0530\")"))
    (expect
     (chronometrist-plist-pp-to-string
      '(:name    "Exercise"
        :tags    (warm-up)
        :start   "2018-11-21T15:35:04+0530"
        :stop    "2018-11-21T15:38:41+0530"
        :comment ("stretching" (25 10 "push-ups"))))
     :to-equal
     (concat
      "(:name    \"Exercise\"\n"
      " :tags    (warm-up)\n"
      " :start   \"2018-11-21T15:35:04+0530\"\n"
      " :stop    \"2018-11-21T15:38:41+0530\"\n"
      " :comment (\"stretching\" (25 10 \"push-ups\")))"))))

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:
