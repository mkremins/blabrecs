(ns blabrecs.train
  (:require [blabrecs.main :as blabrecs]
            [clojure.string :as str]))

(-> (slurp "enable.txt")
    (str/split-lines)
    (blabrecs/build-model)
    (pr-str)
    (#(spit "model.edn" %)))
