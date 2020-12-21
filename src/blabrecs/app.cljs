(ns blabrecs.app
  (:require [blabrecs.main :as blabrecs]
            [blabrecs.neural :as neural]
            [clojure.edn :as edn]
            [clojure.string :as str]))

;;; util

(defn load-file! [path cb]
  (let [req (js/XMLHttpRequest.)]
    (.addEventListener req "load" #(this-as this (cb this)))
    (.open req "GET" path)
    (.send req)))

;;; app-specific

(def app-state
  (atom {}))

(defn sufficiently-probable? [word]
  (let [{:keys [model baselines cnn mode]} @app-state]
      (cond
        (= mode "CNN")
          (> (neural/probability cnn word) 0.82)
        :else
          (> (blabrecs/probability model word)
             (get baselines (count word))))))

(defn test-word [word]
  (let [word (str/trim (str/lower-case word))
        state @app-state]
    (cond
      (or (not (:model state)) (not (:baselines state)) (not (:words state)))
        {:status :empty :msg "hang on a sec, still loading…"}
      (= word "")
        {:status :empty :msg "type in a word!"}
      (re-find #"[^a-z]" word)
        {:status :err :msg "hey! letters only!"}
      (< (count word) 3)
        {:status :err :msg "that's too short to be a word!"}
      (> (count word) 15)
        {:status :err :msg "that's too long, it won't fit on the board!"}
      (contains? (:words @app-state) word)
        {:status :err :msg "can't play that, it's in the dictionary!"}
      (not (sufficiently-probable? word))
        {:status :err :msg "no way that's a word!"}
      (some #(str/includes? word %) (:badwords state))
        {:status :err :msg "no way that's a word!"}
      :else
        {:status :ok :msg "looks good to me!"})))

(defn test-word! []
  (let [word-tester (js/document.getElementById "wordtester")
        status-line (js/document.getElementById "wordinfo")
        submit-button (js/document.getElementById "playit")
        result (test-word (.-value word-tester))]
    (set! (.-className status-line) (name (:status result)))
    (set! (.-innerText status-line) (:msg result))
    (set! (.-disabled submit-button) (not (= (:status result) :ok)))))

(defn try-submit-word! []
  (let [word-tester (js/document.getElementById "wordtester")
        lexicon-table (js/document.getElementById "lexicon")
        word (str/trim (str/lower-case (.-value word-tester)))
        result (test-word word)]
    (when (= (:status result) :ok)
      (set! (.-className lexicon-table) "") ; remove disabled state
      (let [row (.insertRow lexicon-table 1)
            word-cell (.insertCell row 0)
            def-cell (.insertCell row 1)
            textarea (js/document.createElement "textarea")]
        (set! (.-innerText word-cell) word)
        (.appendChild def-cell textarea))
      (set! (.-value word-tester) "")
      (test-word!))))

(defn switch-mode! []
  (let [modes ["Markov" "CNN"]
        {:keys [mode]} @app-state
        next-mode (get modes (mod (+ (.indexOf modes mode) 1) (count modes)))]
    (swap! app-state assoc :mode next-mode)
    (aset (js/document.getElementById "modeswitch") "textContent" (str "Current Mode: " next-mode))
    (test-word!)))

;;; init

(js/console.log "hello from JS!")

(load-file! "model.edn"
  (fn [res]
    (js/console.log "loaded model!")
    (let [model (edn/read-string (.-responseText res))
          baselines (blabrecs/gen-baseline-probs model)]
      (swap! app-state assoc :model model :baselines baselines)
      (test-word!))))

(load-file! "enable.txt"
  (fn [res]
    (js/console.log "loaded words!")
    (swap! app-state assoc :words (set (str/split-lines (.-responseText res))))
    (test-word!)))

(load-file! "https://raw.githubusercontent.com/dariusk/wordfilter/master/lib/badwords.json"
  (fn [res]
    (js/console.log "loaded badwords!")
    (swap! app-state assoc :badwords (js->clj (js/JSON.parse (.-responseText res))))))

(let [model (js/tf.loadLayersModel "model.json")]
  (.then model
    #(let []
      (swap! app-state assoc :cnn %)
      (js/console.log "loaded tf cnn model!")
      (swap! app-state assoc :mode "CNN")
      (aset (js/document.getElementById "modeswitch") "textContent" (str "Current Mode: " "CNN")))
    #(let []
      (js/console.log "failed to load tf cnn model!")
      (swap! app-state assoc :cnn nil)
      (swap! app-state assoc :mode "Markov"))
      (aset (js/document.getElementById "modeswitch") "textContent" (str "Current Mode: " "Markov"))))

(.addEventListener (js/document.getElementById "wordtester") "input" test-word!)

(.addEventListener (js/document.getElementById "wordtester") "keypress"
  #(when (= (.-key %) "Enter") (try-submit-word!)))

(.addEventListener (js/document.getElementById "playit") "click" try-submit-word!)

(.addEventListener (js/document.getElementById "modeswitch") "click" switch-mode!)

(test-word!)
