(ns ahammel.wordle
  (:require [clojure.string :as string])
  (:gen-class))

(defn lowercase?
  [s]
  (= s (string/lower-case s)))

(def dict
  (->> "/usr/share/dict/words"
       slurp
       string/split-lines
       (filter lowercase?)
       (filter #(= (count %) 5))))

(comment (count dict))

(defn guess-word
  [answer guess]
  (let [letterset (set answer)
        match     (fn [a g]
                    (cond
                      (= a g)       :wordle/+
                      (letterset g) :wordle/-
                      :else         :wordle/o))]
    (mapv (fn [a g] [(match a g) g]) answer guess)))

(defn empty-wordprint
  [size]
  {:wordle/positions    (->> (range size)
                             (mapv (constantly [:wordle/is-not #{}])))
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
         (filter
           (fn [[tag _]]
             (not= tag :wordle/o)))
         (map
           (fn [[_ char]]
             [char (count-where #(= char %) letters)]))
         (into {}))))

(defn guess-result->wordprint
  ([result & results]
   (let
     [->wordprint
      (fn [{:wordle/keys [positions has-at-least] :as wordprint}
           guess-result]
        (let
          [none
           (->> guess-result
                (filter (fn [[tag _]] (= tag :wordle/o)))
                (map second)
                set)

           update-pos
           (fn
             [[result-tag result-val] [pos-tag pos-val]]
             (case [result-tag pos-tag]
               [:wordle/+ :wordle/is]     [:wordle/is pos-val]
               [:wordle/+ :wordle/is-not] [:wordle/is result-val]
               [:wordle/- :wordle/is]     [:wordle/is pos-val]
               [:wordle/- :wordle/is-not] [:wordle/is-not (-> none
                                                              (into pos-val)
                                                              (conj result-val))]
               [:wordle/o :wordle/is]     [:wordle/is pos-val]
               [:wordle/o :wordle/is-not] [:wordle/is-not (into none pos-val)]))]
          {:wordle/positions
           (mapv update-pos guess-result positions)

           :wordle/has-at-least
           (merge-with max
                       has-at-least
                       (guess-result->has-at-least guess-result))}))]
     (reduce
       ->wordprint
       (empty-wordprint (count result))
       (conj results result)))))

(defn ^:private position-matches?
  [letter [tag value]]
  (case tag
    :wordle/is (= letter value)
    :wordle/is-not (not (value letter))))

(defn ^:private has-at-least-matches?
  [word [letter n]]
  (>=
    (count-where #(= letter %) word)
    n))

(defn matches-wordprint?
  [{:wordle/keys [positions has-at-least]} word]
  (let [positions-match?
        (every?
          true?
          (map position-matches? word positions))

        has-at-least-match?
        (every?
          true?
          (map (partial has-at-least-matches? word) has-at-least))]
    (and positions-match? has-at-least-match?)))

(comment
  (let [wordprint (guess-result->wordprint (guess-word "blood" "loops"))]
    (->> dict
         shuffle
         (filterv #(matches-wordprint? wordprint %))))

  (let [wordprint (guess-result->wordprint (guess-word "blood" "loops"))]
    (matches-wordprint? wordprint "frock")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "hi"))
