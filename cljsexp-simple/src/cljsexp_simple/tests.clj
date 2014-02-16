(ns cljsexp-simple.core
  (:use clojure.test))

(deftest test-clearToken
  (is (=
       {:code [ " (println '123')" "(dothing '3')"],
        :line 1,
        :col 2,
        :token :none
        :val :none
        :expressions []}
       (clearToken {:code [ " (println '123')" "(dothing '3')"],
                    :line 1,
                    :col 2,
                    :token :lparen,
                    :val :none,
                    :expressions []}))))

(deftest test-currentLine
  (is (= (currentLine  {:code [ " (println '123')" "(println '3')"],
                        :line 0,
                        :col 1,
                        :token :none,
                        :val :none,
                        :expressions []})
         " (println '123')" )))


(deftest test-currentChar
  (is (= (currentChar  {:code [ " (println '123')" "(println '3')"],
                        :line 0,
                        :col 4,
                        :token :none,
                        :val :none,
                        :expressions []})
         \i)))


(deftest moveRight-sameLine
  (is (=

       {:code [ " (println '123')" "(println '3')"],
        :line 0,
        :col 7,
        :token :none,
        :val :none,
        :expressions []}
       (moveRight {:code [ " (println '123')" "(println '3')"],
                   :line 0,
                   :col 4,
                   :token :none,
                   :val :none,
                   :expressions []} 3))))


(deftest moveRight-nextLine
  (is (=
       {:code [ " (println '123')" "(println '3')"],
        :line 1,
        :col 0,
        :token :none,
        :val :none,
        :expressions []}
       (moveRight {:code [ " (println '123')" "(println '3')"],
                   :line 0,
                   :col 4,
                   :token :none,
                   :val :none,
                   :expressions []} 99))))


(deftest test-parseRegex
  (is (=
       {:code [ " (println '123')" "(dothing '3')"],
        :line 1,
        :col 8,
        :token :test,
        :val "dothing",
        :expressions []}
       (parseRegex {:code [ " (println '123')" "(dothing '3')"],
                    :line 1,
                    :col 1,
                    :token :none,
                    :val :none,
                    :expressions []} "test" :test #"[a-z]+"))))

(deftest test-parseName
  (is (=
       {:code [ " (+ '123')" "(dothing '3')"],
        :line 0,
        :col 3,
        :token :name
        :val "+",
        :expressions []}
       (parseName {:code [ " (+ '123')" "(dothing '3')"],
                   :line 0,
                   :col 2,
                   :token :none,
                   :val :none,
                   :expressions []} ))))


(deftest test-nextToken-char
  (is (=
       {:code [ " (println '123')" "(dothing '3')"],
        :line 0,
        :col 2,
        :token :lparen
        :val \(,
        :expressions []}
       (nextToken {:code [ " (println '123')" "(dothing '3')"],
                   :line 0,
                   :col 0,
                   :token :none
                   :val :none,
                   :expressions []}))))

(deftest test-nextToken-regex
  (is (=
       {:code [ " (println '123')" "(dothing '3')"],
        :line 1,
        :col 8,
        :token :name
        :val "dothing",
        :expressions []}
       (nextToken {:code [ " (println '123')" "(dothing '3')"],
                   :line 1,
                   :col 1,
                   :token :lparen,
                   :val :none,
                   :expressions []}))))


(deftest test-nextToken-char-from-space
  (is (=
       {:code [ " (println '123')" "(dothing '3')"],
        :line 0,
        :col 2,
        :token :lparen
        :val \(,
        :expressions []}
       (nextToken {:code [ " (println '123')" "(dothing '3')"],
                   :line 0,
                   :col 0,
                   :token :none,
                   :val :none,
                   :expressions []}))))


(run-all-tests #"cljsexp-simple.core")
