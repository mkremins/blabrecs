(ns blabrecs.markov-train
  (:require [blabrecs.markov :as markov]
            [clojure.string :as str]))

(-> (slurp "enable.txt")
    (str/split-lines)
    (blabrecs/build-model)
    (pr-str)
    (#(spit "model.edn" %)))
