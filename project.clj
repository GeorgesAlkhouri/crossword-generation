(defproject crossword "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.trace "0.7.9"]
                 [org.clojure/core.async "0.2.374"]
                 [criterium "0.4.3"]
                 [clojail "1.0.5"]]
  :main ^:skip-aot crossword.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
;;  :jvm-opts ^:replace ["-XX:-OmitStackTraceInFastThrow"]
  )
