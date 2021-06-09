(ns blabrecs.neural
  "Functions for obtaining predictions from the TensorFlow.js convolutional
  neural network (CNN) classifier, provided by Isaac Karth.")

(defn vectorize-word
  "Convert a string into the tokenized vector that the tensorflow
  model can understand."
  [word]
  (let [tokenizer
        {"@" 1,"e" 2,"i" 3,"s" 4,"a" 5,"n" 6,"o" 7,"r" 8,"t" 9,
        "l" 10, "c" 11,"u" 12,"p" 13,"d" 14,"m" 15,"h" 16,"g" 17,
        "y" 18,"b" 19, "f" 20,"v" 21,"k" 22,"w" 23,"z" 24,"x" 25,
        "q" 26,"j" 27, "'" 28, "/" 29, "\"" 30, "1" 31,"0" 32,
        "8" 33,"5" 34,"7" 35,"6" 36,"9" 37,"2" 38,"3" 39,"4" 40}
        result (partition 24 24 (repeat 0) (map #(get tokenizer % 0) word))]
    (first result)))

(defn probability*
  "Given a tensorflow `model` and `words` (as a vector of strings), return the
  model's prediction as to whether the strings are English words or not."
  [model words]
  (let [word-vectors (map vectorize-word words)
        word-tensor (js/tf.tensor (clj->js word-vectors)
                                  (apply array [(count words) 24]))]
    (.dataSync (.predict model word-tensor {:verbose true}))))

(defn probability
  "Given a tensorflow `model` and a string `word` return the probability of
  whether the string is an English word."
  [model word]
  (first (probability* model [word])))
