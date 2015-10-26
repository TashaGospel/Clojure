(defproject clojurebreaker "0.1.0-SNAPSHOT"
  :description "ClojureBreaker game."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/math.combinatorics "0.0.1"]
                 [org.clojure/test.generative "0.1.3"]]
  :main ^:skip-aot clojurebreaker.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
