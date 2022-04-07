(ns ahammel.wordle
  (:require [clojure.set :as set])
  (:gen-class))

(defn guess-word
  [answer guess]
  (let [correct   (->> (map (fn [a g] (when (= a g) g))
                            answer
                            guess)
                       (map-indexed vector)
                       (filter (fn [[_ letter]] (some? letter)))
                       (into {}))
        unguessed (->> (map-indexed vector answer)
                       (filter (fn [[ix _]] (not (correct ix))))
                       (map second)
                       set)
        wrong-pos (->> (map (fn [a g]
                              (when (and (not= a g) (unguessed g))
                                g))
                            answer
                            guess)
                       (map-indexed vector)
                       (filter (fn [[_ letter]] (some? letter)))
                       (into {}))
        not-in    (set/difference (set guess)
                                  (set answer))]
    {:wordle/correct   correct
     :wordle/wrong-pos wrong-pos
     :wordle/not-in    not-in
     :wordle/size      (count answer)}))

(def ^:private conj-some
  ((remove nil?) conj))

(defn guess-result->wordprint
  [{:wordle/keys [correct not-in wrong-pos size]}]
  (let [wrong (->> (range size)
                   (map (fn [i]
                          [i {:wordle/is-not (conj-some
                                               not-in
                                               (get wrong-pos i))}]))
                   (into {}))
        right (->> correct
                   (map
                     (fn [[k v]]
                       [k {:wordle/is v}]))
                   (into {}))]
    {:wordle/positions (merge wrong right)
     :wordle/unguessed (->> wrong-pos
                            vals
                            set)}))

(comment
  (guess-result->wordprint
    (guess-word "loops" "blood")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "hi"))
