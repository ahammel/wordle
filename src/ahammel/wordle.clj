(ns ahammel.wordle
  (:gen-class))

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
  ([guess-result]
   (guess-result->wordprint
     (empty-wordprint (count guess-result))
     guess-result))

  ([{:wordle/keys [positions has-at-least] :as wordprint}
    guess-result]
   (let [none
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
                  (guess-result->has-at-least guess-result))})))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "hi"))
