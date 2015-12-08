(ns crossword.core-test
  (:require [clojure.test :refer :all]
            [crossword.core :refer :all]))

(def test-affected-pattern-grid-across ["_ _ # _"
                                        "_ _ _ _"])

(def test-affected-pattern-grid-down ["_ _ _ _"
                                      "_ _ _ _"
                                      "# # _ _"
                                      "_ _ _ _"])

(def test-grid-all-patterns ["_ _"
                             "_ _"])

(def test-grid-no-patterns ["# # # # #"
                            "# # # # #"])

(def test-grid-no-elements [])

(def test-grid-letters ["# a p _ "])

(deftest test-hash-wordlist
  (is (= (hash-wordlist '()) {}))
  (is (= (hash-wordlist '("a" "ab" "abc" "aa" "bb" "c")) {:1 '("a" "c"), :2 '("ab" "aa" "bb"), :3 '("abc")}))) ;; order will be respected

(deftest create-init-patterns
  (is (= (create-patterns (format-grid test-grid-simple)) (set (list (->Pattern 0 2 across 3 0 "[a-z][a-z][a-z]" "")
                                                                     (->Pattern 1 1 across 4 0 "[a-z][a-z][a-z][a-z]" "")
                                                                     (->Pattern 2 0 across 5 0 "[a-z][a-z][a-z][a-z][a-z]" "")
                                                                     (->Pattern 3 0 across 4 0 "[a-z][a-z][a-z][a-z]" "")
                                                                     (->Pattern 4 0 across 3 0 "[a-z][a-z][a-z]" "")
                                                                     (->Pattern 2 0 down 3 0 "[a-z][a-z][a-z]" "")
                                                                     (->Pattern 1 1 down 4 0 "[a-z][a-z][a-z][a-z]" "")
                                                                     (->Pattern 0 2 down 5 0 "[a-z][a-z][a-z][a-z][a-z]" "")
                                                                     (->Pattern 0 3 down 4 0 "[a-z][a-z][a-z][a-z]" "")
                                                                     (->Pattern 0 4 down 3 0 "[a-z][a-z][a-z]" "")))))
  (is (= (create-patterns (format-grid test-grid-no-patterns)) (set nil)))
  (is (= (create-patterns (format-grid test-grid-no-elements)) (set nil)))
  (is (= (create-patterns (format-grid test-grid-all-patterns)) (set (list (->Pattern 0 0 across 2 0 "[a-z][a-z]" "")
                                                                           (->Pattern 1 0 across 2 0 "[a-z][a-z]" "")
                                                                           (->Pattern 0 0 down 2 0 "[a-z][a-z]" "")
                                                                           (->Pattern 0 1 down 2 0 "[a-z][a-z]" "")))))
  (is (= (create-patterns ["#_#"
                           "__#"]) (set (list (->Pattern 1 0 across 2 0 "[a-z][a-z]" "")
                                              (->Pattern 0 1 down 2 0 "[a-z][a-z]" ""))))))

(deftest test-pattern->regex
  (is (= (str (pattern->regex (->Pattern 0 2 across 3 0 "" "") (format-grid test-grid-simple))) (str  #"[a-z][a-z][a-z]")))
  (is (= (str (pattern->regex (->Pattern 0 0 down 2 0 "" "") (format-grid test-grid-all-patterns))) (str #"[a-z][a-z]")))
  (is (= (str (pattern->regex (->Pattern 0 1 across 3 0 "" "") (format-grid test-grid-letters))) (str #"ap[a-z]"))))

(deftest test-most-constrained ;; {:2 '("at" "on") :3 ("the" "cat" "dog")}
  (is (= (fill-strategy {:fill-strategy :most-constrained :regex #"[a-z][a-z][a-z]" :words '("the" "cat" "dog")}) 3))
  (is (= (fill-strategy {:fill-strategy :most-constrained :regex #"[a-z]o[a-z]" :words '("the" "cat" "dog")}) 1))
  (is (= (fill-strategy {:fill-strategy :most-constrained :regex #"fo[a-z]" :words '("the" "cat" "dog")}) 0)))

(deftest test-ratio
  (is (= (fill-strategy {:fill-strategy :ratio :regex #"[a-z]a[a-z][a-z]"}) 3/4))
  (is (= (fill-strategy {:fill-strategy :ratio :regex #"[a-z][a-z]"}) 1))
  (is (= (fill-strategy {:fill-strategy :ratio :regex #"abc"}) 0)))

(deftest test-fill-pattern
  (is (= (fill-pattern "most-constrained" (list (->Pattern 0 3 across 3 0 "[a-z][a-z][a-z]" "")
                                                (->Pattern 0 2 across 3 0 "[a-z]o[a-z]" "")) {:3 '("cat" "dog" "cow")}) (->Pattern 0 2 across 3 2 "[a-z]o[a-z]" "")))
  (is (= (fill-pattern "most-constrained" (list (->Pattern 0 3 across 3 0 "[a-z][a-z][a-z]" "")
                                                (->Pattern 0 2 across 3 0 "[a-z]ot" "")) {:3 '("cat" "dog" "cow")}) (->Pattern 0 2 across 3 0 "[a-z]ot" "")))
  (is (= (fill-pattern "most-constrained" (list (->Pattern 0 3 across 3 0 "[a-z][a-z][a-z]" "")
                                                (->Pattern 0 2 across 3 0 "[a-z]o[a-z]" "")) {:3 '("cot" "dog" "cow")}) (->Pattern 0 2 across 3 3 "[a-z]o[a-z]" ""))))

(deftest test-pick-word
  (is (= (pick-words "first-n" (->Pattern 0 0 across 5 0 "[a-z][a-z]a[a-z]t" "") nil {:1 '("a" "b") :5 '("quart" "cluby" "testi" "start")})
         '("quart" "start")))
  (is (= (pick-words "first-n" (->Pattern 0 0 across 3 0 "[a-z]at" "") nil {:3 '("dog" "cow")})
         '()))
  (is (= (set(pick-words "random" (->Pattern 0 0 across 3 0 "[a-z]o[a-z]" "") nil {:3 '("dog" "ape" "cot")}))
         #{"cot" "dog"}))
  (is (= (set(pick-words "random" (->Pattern 0 0 across 3 0 "[a-z]at" "") nil {:3 '("dog" "ape" "cot")}))
         (set nil)))
  ;; (is (= (pick-words "dynamic" (->Pattern 0 0 across 2 3 "[a-z][a-z]" "") (list (->Pattern 0 0 across 2 3 "[a-z][a-z]" "")
  ;;                                                                               (->Pattern 1 0 across 2 3 "[a-z][a-z]" "")
  ;;                                                                               (->Pattern 0 0 down 2 3 "[a-z][a-z]" "")
  ;;                                                                               (->Pattern 0 1 down 2 3 "[a-z][a-z]" "")) {:2 '("on" "ma" "om" "na")})
  ;;        '()))
  )

(deftest test-get-affected-patterns
  (is (= (get-affected-patterns (create-patterns (format-grid test-affected-pattern-grid-across)) (->Pattern 0 0 across 2 0 "" ""))
         (list (->Pattern 0 0 down 2 0 "[a-z][a-z]" "")
               (->Pattern 0 1 down 2 0 "[a-z][a-z]" "") ))
      (= (get-affected-patterns (create-patterns (format-grid test-affected-pattern-grid-down)) (->Pattern 0 1 down 2 0 "" ""))
         (list (->Pattern 0 0 across 4 0 "[a-z][a-z][a-z][a-z]" "")
               (->Pattern 1 0 across 4 0 "[a-z][a-z][a-z][a-z]" "")))))

(deftest test-update-regex
  (is (= (update-regex (->Pattern 0 0 across 3 0 "cat" "") (list (->Pattern 0 0 down 3 0 "[a-z][a-z][a-z]" "")
                                                              (->Pattern 0 1 down 3 0 "[a-z][a-z][a-z]" "")
                                                              (->Pattern 0 2 down 3 0 "[a-z][a-z][a-z]" "")))
         (list (->Pattern 0 0 down 3 0 "c[a-z][a-z]" "")
               (->Pattern 0 1 down 3 0 "a[a-z][a-z]" "")
               (->Pattern 0 2 down 3 0 "t[a-z][a-z]" ""))))
  (is (= (update-regex (->Pattern 0 1 down 3 0 "dog" "") (list (->Pattern 0 0 across 3 0 "[a-z][a-z][a-z]" "")
                                                            (->Pattern 1 0 across 3 0 "[a-z][a-z][a-z]" "")
                                                            (->Pattern 2 0 across 3 0 "[a-z][a-z][a-z]" "")))
         (list (->Pattern 0 0 across 3 0 "[a-z]d[a-z]" "")
               (->Pattern 1 0 across 3 0 "[a-z]o[a-z]" "")
               (->Pattern 2 0 across 3 0 "[a-z]g[a-z]" ""))))
  (is (= (update-regex (->Pattern 0 2 down 3 0 "dog" "") (list (->Pattern 0 1 across 4 0 "[a-z][a-z][a-z][a-z]" "")
                                                            (->Pattern 1 0 across 4 0 "[a-z][a-z][a-z][a-z]" "")
                                                            (->Pattern 2 0 across 3 0 "[a-z][a-z][a-z]" "")))
         (list (->Pattern 0 1 across 4 0 "[a-z]d[a-z][a-z]" "")
               (->Pattern 1 0 across 4 0 "[a-z][a-z]o[a-z]" "")
               (->Pattern 2 0 across 3 0 "[a-z][a-z]g" "")))))

(deftest test-solve
  (letfn [(word-legal?
            [w dict]
            (let [words (words-with-length (count w) dict)]
              (some #(= w %) words)))
          (solve-grid [patterns wordlist]
            (let [res (solve patterns wordlist #{})]
              (if (nil? res)
                false
                (and (not (some false? (map #(word-legal? (:word %) wordlist) (last res))))
                     (= (count patterns) (count (last res)))))))]
    (is (= true (solve-grid (-> test-grid-simple format-grid create-patterns) (-> (read-wordlist) hash-wordlist))))
    (is (= true (solve-grid (-> test-grid-medium format-grid create-patterns) (-> (read-wordlist) hash-wordlist))))
    (is (= true (solve-grid (-> test-grid-hard format-grid create-patterns) (-> (read-wordlist) hash-wordlist))))
    (is (= true (solve-grid (-> test-grid-harder format-grid create-patterns) (-> (read-wordlist) hash-wordlist))))
    ))

(deftest test-patterns-into-grid
  (is (= (patterns-into-grid (list (->Pattern 0 0 across 2 1 "" "ad")
                                   (->Pattern 1 0 across 2 0 "" "nn")
                                   (->Pattern 0 0 down 2 0 "" "an")
                                   (->Pattern 0 1 down 2 0 "" "dn")) ["##"
                                                                      "##"]) ["a d"
                                                                              "n n"]))
  (is (= (patterns-into-grid (list (->Pattern 0 1 across 2 0 "" "ad")
                                   (->Pattern 1 0 across 2 0 "" "nn")
                                   (->Pattern 0 1 down 2 0 "" "an"))
                             ["#__"
                              "__#"]) ["# a d"
                                       "n n #"]))
  (is (= (patterns-into-grid (list (->Pattern 0 0 across 2 0 "" "ad")
                                   (->Pattern 0 3 across 2 0 "" "nn")) ["__#__"]) ["a d # n n"])))

  ;; (is (= (patterns-into-grid (list (->Pattern 0 0 down 2 0 "" "ad")) ["#"
  ;;                                                                     "#"]) ["a"
  ;;                                                                            "d"]))

(deftest test-arc-consistency
  (is (= true (arc-consistency? "ratio" "first-n" (list (->Pattern 0 0 across 3 1 "[a-z]at" "")) {:3 '("cat")})))
  (is (= false (arc-consistency? "ratio" "first-n" (list (->Pattern 0 0 across 3 1 "[a-z]ot" "")) {:3 '("cat" "cit")})))
  (is (= true (arc-consistency? "most-constrained" "first-n" (list (->Pattern 0 0 across 3 1 "[a-z]ot" "")) {:3 '("cat" "cot")})))
  ;; (is (= false (arc-consistency? "most-constrained" "first-n" (list (->Pattern 0 0 across 3 1 "[a-z]ot" "")
  ;;                                                                   (->Pattern 0 0 across 3 0 "[a-z]ig" "")) {:3 '("cat" "cot")})))
  )

