(defproject motosega "0.1.0-SNAPSHOT"
  :description "utilities for generalized input driven languages"
  :url "https://github.com/bzoto/motosega.git"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot motosega.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
