(ns ahammel.wordle
  (:require [clojure.string :as string])
  (:gen-class))

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn lowercase? [s] (= s (string/lower-case s)))

(def dict
  (->> "/usr/share/dict/words"
       slurp
       string/split-lines
       (filter lowercase?)))

(defn string->guess-result
  [guess string]
  (let [letter-result (fn [letter res]
                        (case res
                          \+ [:wordle/+ letter]
                          \- [:wordle/- letter]
                          \o [:wordle/o letter]))]
    (mapv letter-result guess string)))


(defn guess-word
  [answer guess]
  (let [letterset (set answer)
        match (fn [a g]
                (cond (= a g) :wordle/+
                      (letterset g) :wordle/-
                      :else :wordle/o))]
    (mapv (fn [a g] [(match a g) g]) answer guess)))

(defn empty-wordprint
  [size]
  {:wordle/positions (->> (range size)
                          (mapv (constantly [:wordle/is-not #{}]))),
   :wordle/has-at-least {}})

(defn ^:private count-where
  [pred coll]
  (->> coll
       (filter pred)
       count))

(defn ^:private guess-result->has-at-least
  [guess-result]
  (let [letters (map second guess-result)]
    (->> guess-result
         (filter (fn [[tag _]] (not= tag :wordle/o)))
         (map (fn [[_ char]] [char
                              (count-where (fn [[tag letter]]
                                             (and (not= :wordle/o tag)
                                                  (= char letter)))
                                           guess-result)]))
         (into {}))))

(defn guess-result->wordprint
  ([result & results]
   (let [->wordprint
           (fn [{:wordle/keys [positions has-at-least], :as wordprint}
                guess-result]
             (let [wrong-pos (->> guess-result
                                  (filter (fn [[tag _]] (= tag :wordle/-)))
                                  (map second)
                                  set)
                   none (->> guess-result
                             (filter (fn [[tag _]] (= tag :wordle/o)))
                             (map second)
                             (remove wrong-pos)
                             set)
                   update-pos
                     (fn [[result-tag result-val] [pos-tag pos-val]]
                       (case [result-tag pos-tag]
                         [:wordle/+ :wordle/is] [:wordle/is pos-val]
                         [:wordle/+ :wordle/is-not] [:wordle/is result-val]
                         [:wordle/- :wordle/is] [:wordle/is pos-val]
                         [:wordle/- :wordle/is-not] [:wordle/is-not
                                                     (-> none
                                                         (into pos-val)
                                                         (conj result-val))]
                         [:wordle/o :wordle/is] [:wordle/is pos-val]
                         [:wordle/o :wordle/is-not] [:wordle/is-not
                                                     (into none pos-val)]))]
               {:wordle/positions (mapv update-pos guess-result positions),
                :wordle/has-at-least (merge-with max
                                                 has-at-least
                                                 (guess-result->has-at-least
                                                   guess-result))}))]
     (reduce ->wordprint
       (empty-wordprint (count result))
       (conj results result)))))

(defn ^:private position-matches?
  [letter [tag value]]
  (case tag
    :wordle/is (= letter value)
    :wordle/is-not (not (value letter))))

(defn ^:private has-at-least-matches?
  [word [letter n]]
  (>= (count-where #(= letter %) word) n))

(defn matches-wordprint?
  [{:wordle/keys [positions has-at-least]} word]
  (let [length-matches? (= (count word) (count positions))
        positions-match? (every? true? (map position-matches? word positions))
        has-at-least-match? (every? true?
                                    (map (partial has-at-least-matches? word)
                                      has-at-least))]
    (and length-matches? positions-match? has-at-least-match?)))

(comment
  (let [wordprint (guess-result->wordprint (guess-word "blood" "loops"))]
    (->> dict
         (filter #(matches-wordprint? wordprint %))))
  (let [wordprint (guess-result->wordprint (guess-word "blood" "loops"))]
    (matches-wordprint? wordprint "frock")))

(defn dict->position-frequency-table
  [dict]
  (let [counts->frequencies (fn [m]
                              (->> m
                                   (map (fn [[k v]] [k (/ v (count dict))]))
                                   (into {})))]
    (->> dict
         (apply map vector)
         (map frequencies)
         (map counts->frequencies))))

(defn dict->multiples-frequency-table
  [n dict]
  (let [length (count dict)
        freq (fn [letter]
               (let [len (count-where (fn [word]
                                        (has-at-least-matches? word [letter n]))
                                      dict)]
                 (/ len length)))]
    (->> alphabet
         (map (fn [letter] [letter (freq letter)]))
         (into {}))))

(comment
  (dict->multiples-frequency-table 2 dict))

(defn ^:private **2 [x] (* x x))

(defn least-squares-score
  [word position-frequency-table singles-frequency-table
   doubles-frequency-table]
  (let [frequency-score (fn [letter freqs] (**2 (- (get freqs letter 0) 0.5)))
        singles-score (fn [letter]
                        (if (has-at-least-matches? word [letter 1])
                          (**2 (- (get singles-frequency-table letter 0) 0.5))
                          0))
        doubles-score (fn [letter]
                        (if (has-at-least-matches? word [letter 2])
                          (**2 (- (get doubles-frequency-table letter 0) 0.5))
                          0))]
    (+ (->> (map frequency-score word position-frequency-table)
            (reduce +))
       (->> (map singles-score word)
            (reduce +))
       (->> (map doubles-score word)
            (reduce +)))))

(comment
  (float (least-squares-score "soree" dict)))

(defn solve
  [word dict]
  (loop [*dict dict
         wordprint (empty-wordprint (count word))
         tries []
         search-space-sizes []]
    (cond (and (= 1 (count *dict)) (= word (first *dict)))
            {:solved true,
             :tries tries,
             :search-space-sizes search-space-sizes,
             :word word}
          (>= 1 (count *dict)) {:failed true,
                                :tries tries,
                                :search-space-sizes search-space-sizes,
                                :word word}
          :else
            (let [filtered-dict (filter #(matches-wordprint? wordprint %) *dict)
                  score (fn [word]
                          (least-squares-score
                            word
                            (dict->position-frequency-table filtered-dict)
                            (dict->multiples-frequency-table 1 filtered-dict)
                            (dict->multiples-frequency-table 2 filtered-dict)))
                  guess (min-key score filtered-dict)]
              (recur filtered-dict
                     (guess-result->wordprint (guess-word word guess))
                     (conj tries guess)
                     (conj search-space-sizes (count filtered-dict)))))))

(comment
  (solve "sonar" dict))

(comment
  (def n 5)
  (def filtered-dict (filter #(= (count %) n) dict))
  (->> filtered-dict
       (filter (fn [word] (has-at-least-matches? word [\i 2])))
       (take 20))
  (let [guesses [["sonar" "+oo-o"] ["stale" "+o-+o"] ["sally" "++o++"]]
        wordprint (if (empty? guesses)
                    (empty-wordprint n)
                    (->> guesses
                         (map (fn [[guess result]]
                                (string->guess-result guess result)))
                         (apply guess-result->wordprint)))
        dictionary (filter #(matches-wordprint? wordprint %) filtered-dict)
        position-frequency-table (dict->position-frequency-table dictionary)
        singles-frequency-table (dict->multiples-frequency-table 1 dictionary)
        doubles-frequency-table (dict->multiples-frequency-table 2 dictionary)]
    {:wordprint wordprint,
     :search-space (count dictionary),
     :guesses (->> dictionary
                   (map (fn [word]
                          {:word word,
                           :least-squares-score (float
                                                  (least-squares-score
                                                    word
                                                    position-frequency-table
                                                    singles-frequency-table
                                                    doubles-frequency-table))}))
                   (sort-by :least-squares-score)
                   (take 10))}))

(defn -main "I don't do a whole lot ... yet." [& args] (println "hi"))
