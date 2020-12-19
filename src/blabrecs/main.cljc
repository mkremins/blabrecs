(ns blabrecs.main
  (:require [clojure.string :as str]
            [blabrecs.neural :as neural]))

;;; common utility functions

(def ngram-size 3)

(defn normalize [word]
  (str "^" (str/lower-case word) "$"))

(defn word->ngrams [ngram-size word]
  (->> (normalize word)
       (partition ngram-size 1)
       (map str/join)))

(defn ngram->path
  "Convert an `ngram` to a path into the Markov model."
  [ngram]
  (let [prefix (subs ngram 0 (dec (count ngram)))
        next-char (subs ngram (dec (count ngram)))]
    [prefix next-char]))

;;; Markov model training

(defn counts->probs
  "Convert a map where keys are items and vals are item counts
  to a map where keys are items and vals are item probabilities."
  [counts]
  (let [total (apply + (vals counts))]
    (reduce-kv (fn [probs char char-count]
                 (assoc probs char (double (/ char-count total))))
               {} counts)))

(defn process-word
  "Add the given `word` to the given Markov `model`."
  [model word]
  (->> (word->ngrams ngram-size word)
       (reduce (fn [model ngram]
                 (update-in model (ngram->path ngram) (fnil inc 0)))
               model)))

(defn build-model
  "Given a seq of `words`, build a Markov model."
  [words]
  (->> words
       (reduce process-word {})
       (reduce-kv (fn [model ngram counts]
                    (assoc model ngram (counts->probs counts)))
                  {})))

;;; set probability baselines for words of different lengths

(defn avg-transition-prob [model]
  (let [all-probs (mapcat vals (vals model))]
    (/ (apply + all-probs) (count all-probs))))

(defn gen-baseline-probs [model]
  (let [avg-prob (avg-transition-prob model)]
    (reduce (fn [baselines n]
              (assoc baselines n (apply * (repeat n avg-prob))))
            {} (range 25))))

;;; probability calculation via trained Markov model

(defn probability*
  "Given a Markov `model` and a `word`, return a vector whose first item is
  the model's total probability for this word and whose second item is
  a seq of the model's individual subprobabilities for this word."
  [model word]
  (js/console.log word)
  (js/console.log (neural/vectorize-word word))
  (let [vect-word (neural/vectorize-word word)
        demo-array (apply array [2 3 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])
        demo-tensor (js/tf.tensor vect-word (apply array [1 24]))
        model (js/tf.loadLayersModel "model.json")
        ]
    (js/console.log demo-tensor)
    (.then model
      #(js/console.log (first (. (. % (predict demo-tensor {:verbose true})) dataSync)))
      #(js/console.log %)))
  (let [subprobs (->> (word->ngrams ngram-size word)
                      (map #(get-in model (ngram->path %))))
        prob (apply * subprobs)]
    [prob subprobs]))

(defn probability
  "Given a Markov `model` and a `word`, return the model's total probability
  for this word."
  [model word]

  (first (probability* model word)))

(js/console.log "main loaded")
