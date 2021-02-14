(ns toothless.core-test
  (:require [clojure.test :refer [deftest is testing async]]
            [toothless.core :refer [html phtml]]
            [promesa.core :as p]))

(deftest tag-names
  (testing "basic tags"
    (is (= (html [:div]) "<div></div>"))
    (is (= (html ["div"]) "<div></div>"))
    (is (= (html ['div]) "<div></div>")))
  (testing "tag syntax sugar"
    (is (= (html [:div#foo]) "<div id=\"foo\"></div>"))
    (is (= (html [:div.foo]) "<div class=\"foo\"></div>"))
    (is (= (html [:div.foo (str "bar" "baz")])
           "<div class=\"foo\">barbaz</div>"))
    (is (= (html [:div.a.b]) "<div class=\"a b\"></div>"))
    (is (= (html [:div.a.b.c]) "<div class=\"a b c\"></div>"))
    (is (= (html [:div#foo.bar.baz])
           "<div class=\"bar baz\" id=\"foo\"></div>"))))

(deftest tag-contents
  (testing "empty tags"
    (is (= (html [:div]) "<div></div>"))
    (is (= (html [:h1]) "<h1></h1>"))
    (is (= (html [:script]) "<script></script>"))
    (is (= (html [:text]) "<text></text>"))
    (is (= (html [:a]) "<a></a>"))
    (is (= (html [:iframe]) "<iframe></iframe>"))
    (is (= (html [:title]) "<title></title>"))
    (is (= (html [:section]) "<section></section>"))
    (is (= (html [:select]) "<select></select>"))
    (is (= (html [:object]) "<object></object>"))
    (is (= (html [:video]) "<video></video>")))
  (testing "void tags"
    (is (= (html [:br]) "<br>"))
    (is (= (html [:link]) "<link>"))
    #_(is (= (html [:colgroup {:span 2}] "<colgroup span=\"2\">")))
    #_(is (= (html [:colgroup [:col]] "<colgroup><col></colgroup>"))))
  (testing "tags containing text"
    (is (= (html [:text "Lorem Ipsum"]) "<text>Lorem Ipsum</text>")))
  (testing "contents are concatenated"
    (is (= (html [:body "foo" "bar"]) "<body>foobar</body>"))
    (is (= (html [:body [:p] [:br]]) "<body><p></p><br></body>")))
  (testing "seqs are expanded"
    (is (= (html [:body (list "foo" "bar")]) "<body>foobar</body>"))
    (is (= (html (list [:p "a"] [:p "b"])) "<p>a</p><p>b</p>")))
  (testing "keywords are turned into strings"
    (is (= (html [:div :foo]) "<div>foo</div>")))
  (testing "vecs don't expand - error if vec doesn't have tag name"
    (is (thrown? js/Error
                 (html (vector [:p "a"] [:p "b"])))))
  (testing "tags can contain tags"
    (is (= (html [:div [:p]]) "<div><p></p></div>"))
    (is (= (html [:div [:b]]) "<div><b></b></div>"))
    (is (= (html [:p [:span [:a "foo"]]])
           "<p><span><a>foo</a></span></p>"))))

(deftest tag-attributes
  (testing "tag with blank attribute map"
    (is (= (html [:xml {}]) "<xml></xml>")))
  (testing "tag with populated attribute map"
    (is (= (html [:xml {:a "1", :b "2"}]) "<xml a=\"1\" b=\"2\"></xml>"))
    (is (= (html [:img {"id" "foo"}]) "<img id=\"foo\">"))
    (is (= (html [:img {'id "foo"}]) "<img id=\"foo\">"))
    (is (= (html [:xml {:a "1", 'b "2", "c" "3"}])
           "<xml a=\"1\" b=\"2\" c=\"3\"></xml>")))
  (testing "attribute values are escaped"
    (is (= (html [:div {:id "\""}]) "<div id=\"&quot;\"></div>")))
  (testing "boolean attributes"
    (is (= (html [:input {:type "checkbox" :checked true}])
           "<input checked type=\"checkbox\">"))
    (is (= (html [:input {:type "checkbox" :checked false}])
           "<input type=\"checkbox\">")))
  (testing "nil attributes"
    (is (= (html [:span {:class nil} "foo"])
           "<span>foo</span>")))
  (testing "resolving conflicts between attributes in the map and tag"
    (is (= (html [:div.foo {:class "bar"} "baz"])
           "<div class=\"foo bar\">baz</div>"))
    (is (= (html [:div#bar.foo {:id "baq"} "baz"])
           "<div class=\"foo\" id=\"baq\">baz</div>")))
  (testing "tag with vector class"
    (is (= (html [:div.foo {:class ["bar"]} "baz"])
           "<div class=\"foo bar\">baz</div>"))
    (is (= (html [:div.foo {:class [:bar]} "baz"])
           "<div class=\"foo bar\">baz</div>"))
    (is (= (html [:div.foo {:class [:bar "box"]} "baz"])
           "<div class=\"foo bar box\">baz</div>"))
    (is (= (html [:div.foo {:class ["bar" "box"]} "baz"])
           "<div class=\"foo bar box\">baz</div>"))
    (is (= (html [:div.foo {:class [:bar :box]} "baz"])
           "<div class=\"foo bar box\">baz</div>"))))

(deftest compiled-tags
  (testing "tag content can be vars"
    (is (= (let [x "foo"] (html [:span x])) "<span>foo</span>")))
  (testing "tag content can be forms"
    (is (= (html [:span (str (+ 1 1))]) "<span>2</span>"))
    (is (= (html [:span ({:foo "bar"} :foo)]) "<span>bar</span>")))
  (testing "attributes can contain vars"
    (let [x "foo"]
      (is (= (html [:xml {:x x}]) "<xml x=\"foo\"></xml>"))
      (is (= (html [:xml {x "x"}]) "<xml foo=\"x\"></xml>"))
      (is (= (html [:xml {:x x} "bar"]) "<xml x=\"foo\">bar</xml>"))))
  (testing "attributes are evaluated"
    (is (= (html [:img {:src (str "/foo" "/bar")}])
           "<img src=\"/foo/bar\">"))
    (is (= (html [:div {:id (str "a" "b")} (str "foo")])
           "<div id=\"ab\">foo</div>")))
  (testing "type hints"
    (let [string "x"]
      (is (= (html [:span ^String string]) "<span>x</span>"))))
  (testing "optimized forms"
    (is (= (html [:ul (for [n (range 3)]
                        [:li n])])
           "<ul><li>0</li><li>1</li><li>2</li></ul>"))
    (is (= (html [:div (if true
                         [:span "foo"]
                         [:span "bar"])])
           "<div><span>foo</span></div>")))
  (testing "values are evaluated only once"
    (let [times-called (atom 0)
          foo #(swap! times-called inc)]
      (html [:div (foo)])
      (is (= @times-called 1))))
  (testing "defer evaluation of non-literal class names when combined with tag classes"
    (let [x "attr-class"]
      (is  (= (html [:div.tag-class {:class x}])
              "<div class=\"tag-class attr-class\"></div>")))))


(deftest promisa
  (testing "handle nested promises"
    (async done
           (->
             (phtml [:div
                     (p/do! 
                       [:h1 (p/do! "Foo")])
                     (for [x (range 3)]
                       (p/do!
                         [:h1 (p/do! x)]))])
             (p/then (fn [html]
                       (is (= html
                              "<div><h1>Foo</h1><h1>0</h1><h1>1</h1><h1>2</h1></div>"))))
             (p/then done)))))
