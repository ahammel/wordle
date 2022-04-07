(ns ahammel.wordle-test
  (:require [clojure.test :refer [deftest testing is are]]
            [ahammel.wordle :refer [guess-word
                                    guess-result->wordprint]]))

(deftest guess-word-test
  (testing "guess-word"
    (are [args result]
         (= result
            (guess-word (:answer args) (:guess args)))

         {:answer "guess" :guess "guess"}
         {:wordle/correct   {0 \g
                             1 \u
                             2 \e
                             3 \s
                             4 \s}
          :wordle/wrong-pos {}
          :wordle/not-in    #{}
          :wordle/size      5}

         {:answer "ropes" :guess "robes"}
         {:wordle/correct   {0 \r
                             1 \o
                             3 \e
                             4 \s}
          :wordle/wrong-pos {}
          :wordle/not-in    #{\b}
          :wordle/size      5}

         {:answer "loops" :guess "blood"}
         {:wordle/correct   {2 \o}
          :wordle/wrong-pos {1 \l
                             3 \o}
          :wordle/not-in    #{\b \d}
          :wordle/size      5})))

(deftest guess-result->wordprint-test
  (testing "guess-result->wordprint"
    (are [args result]
         (= result
            (guess-result->wordprint
              (guess-word (:answer args) (:guess args))))

         {:answer "guess" :guess "guess"}
         {:wordle/positions {0 {:wordle/is \g}
                             1 {:wordle/is \u}
                             2 {:wordle/is \e}
                             3 {:wordle/is \s}
                             4 {:wordle/is \s} }
          :wordle/unguessed  #{}}

         {:answer "ropes" :guess "robes"}
         {:wordle/positions {0 {:wordle/is \r}
                             1 {:wordle/is \o}
                             2 {:wordle/is-not #{\b}}
                             3 {:wordle/is \e}
                             4 {:wordle/is \s} }
          :wordle/unguessed  #{}}

         {:answer "loops" :guess "blood"}
         {:wordle/positions {0 {:wordle/is-not #{\b \d}}
                             1 {:wordle/is-not #{\b \d \l} }
                             2 {:wordle/is \o}
                             3 {:wordle/is-not #{\b \d \o}}
                             4 {:wordle/is-not #{\b \d}} }
          :wordle/unguessed  #{\l \o}})))
