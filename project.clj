(defproject com.pettomato/kanren "0.1.1-SNAPSHOT"
  :description "Clojure implementations of the Kanren family of relational programming languages."
  :url "http://pettomato.com/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :jar-exclusions [#"\.cljx|\.swp|\.swo|\.DS_Store"]
  :source-paths ["src/cljx"]
  :test-paths ["target/test-classes"]
  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :clj}

                  {:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :cljs}

                  {:source-paths ["test/cljx"]
                   :output-path "target/test-classes"
                   :rules :clj}

                  {:source-paths ["test/cljx"]
                   :output-path "target/test-classes"
                   :rules :cljs}]}
  :hooks [cljx.hooks]
  :profiles {:dev {:plugins [[org.clojure/clojurescript "0.0-2202"]
                             [com.keminglabs/cljx "0.3.2"]
                             [lein-cljsbuild "1.0.3"]]}})
