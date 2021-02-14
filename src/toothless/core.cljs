(ns toothless.core
  (:require [clojure.string :as string]))

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

(defn html [x]
  (cond
    (vector? x) (let [[tag id cls] (normalize-tag (first x))
                      attrs (merge-attrs (if (map? (second x)) (second x) {}) id cls)
                      attrs-str (render-attrs attrs)
                      content (if (map? (second x))
                                (nthrest x 2)
                                (nthrest x 1))]
                  (if (void-tags tag)
                    (str "<" tag (when-not (string/blank? attrs-str) " ") attrs-str ">")
                    (str "<" tag (when-not (string/blank? attrs-str) " ") attrs-str ">" 
                         (->> content 
                              (map html) 
                              (string/join "")) 
                         "</" tag ">")))
    (sequential? x) (string/join "" (map html x))
    (keyword? x) (name x)
    :else x))

(comment
  (html [:div {:class nil} "baz"])
  (html [:div.foo {:class "bar"} "baz"])
  (html [:body [:p] [:br]])
  (html [:div.foo.bar (str "bar" "baz")]))
