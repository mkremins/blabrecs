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

(defn neural-vectorize-word [word]
  """Convert a string into the tokenized vector that the tensorflow
  model can understand."""
  (let [tokenizer
        {"@" 1,"e" 2,"i" 3,"s" 4,"a" 5,"n" 6,"o" 7,"r" 8,"t" 9,
        "l" 10, "c" 11,"u" 12,"p" 13,"d" 14,"m" 15,"h" 16,"g" 17,
        "y" 18,"b" 19, "f" 20,"v" 21,"k" 22,"w" 23,"z" 24,"x" 25,
        "q" 26,"j" 27, "'" 28, "/" 29, "\"" 30, "1" 31,"0" 32,
        "8" 33,"5" 34,"7" 35,"6" 36,"9" 37,"2" 38,"3" 39,"4" 40}]
          (apply array
            (first
              (partition 24 24 (repeat 0) (map #(get tokenizer % 0) word))
              ))))

(defn probability*
  "Given a Markov `model` and a `word`, return a vector whose first item is
  the model's total probability for this word and whose second item is
  a seq of the model's individual subprobabilities for this word."
  [model word]
   (let [v1 (js/console.log word)
   v2 (js/console.log "test")
     word-tensor (js/tf.tensor (neural-vectorize-word word)
                               (apply array [1 24]))
     ;word-tensor (js/tf.tensor word-vector (apply array [1 24]))
     v3 (js/console.log word-tensor)
         model (js/tf.loadLayersModel "model.json")]
     (.then model
       #(js/console.log (first (. (. % (predict word-tensor {:verbose true})) dataSync)))
       #(js/console.log %)))
  (js/console.log (neural/probability nil word))
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
