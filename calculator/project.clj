(defproject calculator "0.1.0"
  :description "A simple calculator."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [seesaw "1.4.5"]]
  :main ^:skip-aot calculator.gui
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[midje "1.8.2"]]}})
