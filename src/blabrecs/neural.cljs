(ns blabrecs.neural
  )

(defn vectorize-word [word]
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
