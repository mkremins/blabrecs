(ns blabrecs.neural
  )

(def neural-state (atom {:word-map {}}))

(defn vectorize-word [word]
  "Convert a string into the tokenized vector that the tensorflow
  model can understand."
  (let [tokenizer
        {"@" 1,"e" 2,"i" 3,"s" 4,"a" 5,"n" 6,"o" 7,"r" 8,"t" 9,
        "l" 10, "c" 11,"u" 12,"p" 13,"d" 14,"m" 15,"h" 16,"g" 17,
        "y" 18,"b" 19, "f" 20,"v" 21,"k" 22,"w" 23,"z" 24,"x" 25,
        "q" 26,"j" 27, "'" 28, "/" 29, "\"" 30, "1" 31,"0" 32,
        "8" 33,"5" 34,"7" 35,"6" 36,"9" 37,"2" 38,"3" 39,"4" 40}
        result (partition 24 24 (repeat 0) (map #(get tokenizer % 0) word))]
          (js/console.log result)
          (first result)))

(defn load-model []
  (js/tf.loadLayersModel "model.json"))


;(js/tf.tensor (neural/vectorize-word word)
;                                (apply array [1 24]))



(defn probability*
  "Given a tensorflow `model` and `words` (as a vector of strings), return the model's prediction as to whether the strings are English words or not."
  [model words]
  (let [
    ; v1 (js/console.log "vectorizing...")
    ;     v5 (js/console.log (map #(string? %) words))
    ;     v4 (js/console.log words)
    ;     word-vectors (map #(vectorize-word %) words)
    ;     v2 (js/console.log word-vectors)
    ;     word-tensor (js/tf.tensor (clj->js word-vectors) [(count words) 24])
    ;     v3 (js/console.log word-tensor)
        word-vectors (map vectorize-word words)
        ;v4 (js/console.log word-vectors)
        ;v5 (js/console.log (str word-vectors))
        word-tensor (js/tf.tensor (clj->js word-vectors) (apply array [(count words) 24]))

        ]
    (.then model
      (fn [word]
      (let [predictions
              (. (. word (predict word-tensor {:verbose true})) dataSync)
            word-map (into {} (map vector words predictions)) ]
        ;(js/console.log predictions)
        ;(js/console.log word-map)
        ;(js/console.log (str word-map))
        (swap! neural-state assoc :predictions predictions :words words)
        (swap! neural-state assoc
          :word-map (merge (:word-map @neural-state) word-map))
        ))
      #(js/console.log %))
      1.0))

(defn probability
  "
  Given a tensorflow `model` and a string `word` return the probability of
  whether the string is an English word.
  If the first argument is false or nil, load the default model and use that.
  "
  [model word]
  (let [the-model (or model (load-model))
        ;v1 (js/console.log word)
        ;v2 (js/console.log [word])
        ;v3 (js/console.log the-model)
        prediction (probability* the-model [word "test"])]
        (js/console.log (str (:word-map @neural-state)))
        (js/console.log (str (get (:word-map @neural-state) word)))
        prediction))
