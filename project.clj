(defproject com.pettomato/kanren "0.1.2-SNAPSHOT"
  :description "Clojure implementations of the Kanren family of relational programming languages."
  :url "https://github.com/austinhaas/kanren"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2277"]
                 [com.keminglabs/cljx "0.4.0"]]
  :plugins [[com.keminglabs/cljx "0.4.0"]
            [lein-cljsbuild "1.0.3"]
            [com.cemerick/clojurescript.test "0.3.1"]]
  :hooks [cljx.hooks leiningen.cljsbuild]
  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/generated/src/clj"
                   :rules :clj}
                  {:source-paths ["src/cljx"]
                   :output-path "target/generated/src/cljs"
                   :rules :cljs}
                  {:source-paths ["test/cljx"]
                   :output-path "target/generated/test/clj"
                   :rules :clj}
                  {:source-paths ["test/cljx"]
                   :output-path "target/generated/test/cljs"
                   :rules :cljs}]}
  :jar-exclusions [#"\.cljx|\.swp|\.swo|\.DS_Store"]
  :source-paths ["target/generated/src/clj" "src/clj"]
  :resource-paths ["target/generated/src/cljs"]
  :test-paths ["target/generated/test/clj" "test/clj"]
  :cljsbuild {:test-commands {"unit" ["phantomjs" :runner
                                      "target/unit-test.js"]}
              :builds {:dev  {:source-paths ["src/clj" "target/generated/src/cljs"]
                              :compiler {:output-to "target/main.js"
                                         :optimizations :whitespace
                                         :pretty-print true}}
                       :test {:source-paths ["src/clj" "test/clj"
                                             "target/generated/src/cljs"
                                             "target/generated/test/cljs"]
                              :compiler {:output-to "target/unit-test.js"
                                         :optimizations :whitespace
                                         :pretty-print true}}}})
