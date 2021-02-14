(ns toothless.core
  (:require [clojure.string :as string]
            [promesa.core :as p]))

(def re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(def ^{:doc "A list of elements that must be rendered without a closing tag."
       :private true}
  void-tags
  #{"area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen" "link"
    "meta" "param" "source" "track" "wbr"})

(defn- ->str [x]
  (cond 
    (string? x) x
    (keyword? x) (name x)
    :else (str x)))

(defn escape-html
  "Change special characters into HTML character entities."
  [text]
  (-> (->str text)
    (string/replace "&"  "&amp;")
    (string/replace "<"  "&lt;")
    (string/replace ">"  "&gt;")
    (string/replace "\"" "&quot;")
    (string/replace "'" "&apos;")))

(defn normalize-tag [x]
  (try
    (let [ [_ tag id cls] (re-matches re-tag (name x))]
      [tag id cls])
    (catch js/Error _
      (throw (new js/Error "Invalid tag")))))

(defn merge-attrs [attrs id cls]
  (cond-> attrs
    (some? id) (update :id #(if % % id))
    (some? cls) (update :class 
                        (fn [attr-cls]
                          (let [tag-cls (->> (string/replace cls "." " "))]
                            (cond
                              (and (coll? attr-cls)
                                   (seq attr-cls)) 
                              (str
                                tag-cls
                                " "
                                (string/join " " (map ->str attr-cls)))
                              (and
                                (string? attr-cls)
                                (not
                                  (string/blank? attr-cls)))
                              (str tag-cls " " attr-cls)
                              :else tag-cls))))))

(defn render-attrs [attrs]
  (when (seq attrs)
    (->> attrs
         (sort-by (comp ->str first))
         (map (fn [[attr value]]
                (cond
                  (true? value) (->str attr)
                  (not value) nil
                  :else (str (->str attr) "=" "\"" (escape-html (->str value)) "\""))))
         (remove nil?)
         (string/join " "))))

(comment
  (render-attrs {:class nil}))


(defn parse-tag [v]
  (let [[tag id cls] (normalize-tag (first v))
        attrs (merge-attrs (if (map? (second v)) (second v) {}) id cls)
        content (if (map? (second v))
                  (nthrest v 2)
                  (nthrest v 1))]
    [tag attrs content]))


(defn html [x]
  (cond
    (vector? x) (let [[tag attrs content] (parse-tag x)
                      attrs-str (render-attrs attrs)]
                  (if (void-tags tag)
                    (str "<" tag (when-not (string/blank? attrs-str) " ") attrs-str ">")
                    (str "<" tag (when-not (string/blank? attrs-str) " ") attrs-str ">" 
                         (html content) 
                         "</" tag ">")))
    (sequential? x) (string/join "" (map html x))
    (keyword? x) (name x)
    :else x))

(defn phtml [px]
  (-> px
      (p/then (fn [x]
                (cond
                  (vector? x) 
                  (let [[tag attrs content] (parse-tag x)
                        attrs-str (render-attrs attrs)]
                    (if (void-tags tag)
                      (str "<" tag (when-not (string/blank? attrs-str) " ") attrs-str ">")
                      (-> (phtml content)
                          (p/then #(str "<" tag (when-not (string/blank? attrs-str) " ") attrs-str ">" 
                                        %
                                        "</" tag ">")))))
                  (sequential? x)
                  (-> (p/all (mapv phtml x))
                      (p/then #(string/join "" %)))
                  :else (html x))))))

(comment
  (->
    (phtml [:h1 "Hello"
            (p/delay 1000
                     [:h2 "World"])
            (p/delay 1000
                     [:h2 "!"])
            (for [x (range 20)]
              [:h2 (p/delay x "!")])])
    (p/then prn)
    (p/catch prn))


  (html [:div {:class nil} "baz"])
  (html [:div.foo {:class "bar"} "baz"])
  (html [:body [:p] [:br]])
  (html [:div.foo.bar (str "bar" "baz")]))
