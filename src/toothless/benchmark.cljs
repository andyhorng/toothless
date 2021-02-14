(ns toothless.benchmark
  (:require [clojure.string :as string]
            [toothless.core :as c]
            [hiccups.runtime :as hs]
            [taoensso.tufte :as tufte :refer-macros (p profile)]))

;; https://gist.github.com/rboyd/5053955

(defn rand-str [a b]
  (apply str (take (+  (rand-int b) a) (repeatedly #(char (+ (rand 26) 65))))))

(defn gen-test-hiccup [children-size]
   (let [tag (rand-nth (->> [:div]
                            (mapv name)))
         id (rand-str 3 10)
         cls (->> (take (+ 1 (rand-int 10)) (repeatedly #(rand-str 3 10)))
                  (string/join "."))
         attrs (->> (repeatedly (fn []
                                  [(rand-str 3 10)
                                   (rand-str 3 10)]))
                    (take (+ 1 (rand-int 10)))
                    (into {}))
         text (rand-str 50 100)]
     [(keyword (str tag "#" id "." cls))
      attrs
      text
      (repeatedly
        (rand-int children-size)
        #(gen-test-hiccup (- children-size 1)))]))


(comment
  (tufte/add-basic-println-handler! {})
  (profile
    {}
    (dotimes [_ 100]
      (p :hiccups (hs/render-html (gen-test-hiccup 7)))
      (p :toothless (c/html (gen-test-hiccup 7))))))
