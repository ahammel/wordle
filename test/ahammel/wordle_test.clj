(ns ahammel.wordle-test
  (:require [clojure.test :refer [deftest testing is are]]
            [ahammel.wordle :refer [guess-word
                                    guess-result->wordprint
                                    matches-wordprint?]]))

(deftest guess-word-test
  (testing "guess-word"
    (are [args result]
         (= result
            (guess-word (:answer args) (:guess args)))

         {:answer "guess" :guess "guess"}
         [[:wordle/+ \g]
          [:wordle/+ \u]
          [:wordle/+ \e]
          [:wordle/+ \s]
          [:wordle/+ \s]]

         {:answer "ropes" :guess "robes"}
         [[:wordle/+ \r]
          [:wordle/+ \o]
          [:wordle/o \b]
          [:wordle/+ \e]
          [:wordle/+ \s]]

         {:answer "loops" :guess "blood"}
         [[:wordle/o \b]
          [:wordle/- \l]
          [:wordle/+ \o]
          [:wordle/- \o]
          [:wordle/o \d]])))

(deftest guess-result->wordprint-test
  (testing "guess-result->wordprint"
    (are [args result]
         (= result
            (guess-result->wordprint
              (guess-word (:answer args) (:guess args))))

         {:answer "guess" :guess "guess"}
         {:wordle/positions     [[:wordle/is \g]
                                 [:wordle/is \u]
                                 [:wordle/is \e]
                                 [:wordle/is \s]
                                 [:wordle/is \s] ]
          :wordle/has-at-least  {\g 1 \u 1 \e 1 \s 2}}

         {:answer "ropes" :guess "robes"}
         {:wordle/positions     [[:wordle/is     \r]
                                 [:wordle/is     \o]
                                 [:wordle/is-not #{\b}]
                                 [:wordle/is     \e]
                                 [:wordle/is     \s]]
          :wordle/has-at-least  {\r 1 \o 1 \e 1 \s 1}}

         {:answer "loops" :guess "blood"}
         {:wordle/positions [[:wordle/is-not #{\b \d}]
                             [:wordle/is-not #{\b \d \l}]
                             [:wordle/is     \o]
                             [:wordle/is-not #{\b \d \o}]
                             [:wordle/is-not #{\b \d}]]
          :wordle/has-at-least  {\l 1 \o 2}}))

  (testing "multiple guesses"
    (are [args result]
         (= result
            (let [first-result  (guess-word (:answer args) (:first-guess args))
                  second-result (guess-word (:answer args) (:second-guess args))]
              (-> (guess-result->wordprint first-result)
                  (guess-result->wordprint second-result))))

         {:answer "foray" :first-guess "adieu" :second-guess "boats"}
         {:wordle/positions    [[:wordle/is-not #{\a \d \i \e \u \b \t \s}]
                                [:wordle/is     \o]
                                [:wordle/is-not #{\a \d \i \e \u \b \t \s}]
                                [:wordle/is-not #{\d \i \e \u \b \t \s}]
                                [:wordle/is-not #{\d \i \e \u \b \t \s}]]
          :wordle/has-at-least {\a 1 \o 1}}

         {:answer "foray" :first-guess "comma" :second-guess "polar"}
         {:wordle/positions    [[:wordle/is-not #{\c \l \m \p}]
                                [:wordle/is     \o]
                                [:wordle/is-not #{\c \l \m \p}]
                                [:wordle/is     \a]
                                [:wordle/is-not #{\a \c \l \m \p \r}]]
          :wordle/has-at-least {\o 1 \a 1 \r 1}}

         {:answer "foray" :first-guess "polar" :second-guess "comma"}
         {:wordle/positions    [[:wordle/is-not #{\c \l \m \p}]
                                [:wordle/is     \o]
                                [:wordle/is-not #{\c \l \m \p}]
                                [:wordle/is     \a]
                                [:wordle/is-not #{\a \c \l \m \p \r}]]
          :wordle/has-at-least {\r 1
                                \o 1
                                \a 1}}

         {:answer "blood" :first-guess "loops" :second-guess "polar"}
         {:wordle/positions    [[:wordle/is-not #{\a \l \p \r \s}]
                                [:wordle/is-not #{\a \o \p \r \s}]
                                [:wordle/is     \o]
                                [:wordle/is-not #{\a \p \r \s}]
                                [:wordle/is-not #{\a \p \r \s}]]
          :wordle/has-at-least {\l 1 \o 2}}

         {:answer "loops" :first-guess "brood" :second-guess "plotz"}
         {:wordle/positions    [[:wordle/is-not #{\b \d \p \r \t \z}]
                                [:wordle/is-not #{\b \d \l \r \t \z}]
                                [:wordle/is     \o]
                                [:wordle/is-not #{\b \d \o \r \t \z}]
                                [:wordle/is-not #{\b \d \r \t \z}]]
          :wordle/has-at-least {\l 1
                                \p 1
                                \o 2}})))

(deftest matches-wordprint?-test
  (testing "dictionary narrowing"
    (let [wordprint {:wordle/positions    [[:wordle/is-not #{\l \p \s}]
                                           [:wordle/is-not #{\o \p \s}]
                                           [:wordle/is \o]
                                           [:wordle/is-not #{\p \s}]
                                           [:wordle/is-not #{\p \s}]]
                     :wordle/has-at-least {\l 1 \o 2}}]
      (is (= (filterv
               (partial matches-wordprint? wordprint)
               ["blood" "loops" "frock" "aloof"])
             ["blood" "aloof"])))))
