(defproject lunch-bot "0.1.0-SNAPSHOT"
  :description "A Slack bot to manage group lunches"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clj-slack-client "0.1.4-SNAPSHOT"]
                 [clj-time "0.9.0"]
                 [org.clojure/math.combinatorics "0.0.8"]
                 [org.clojure/core.incubator "0.1.3"]
                 [ring/ring-core "1.4.0"]
                 [ring/ring-jetty-adapter "1.4.0"]
                 [compojure "1.4.0"]
                 [http-kit "2.1.16"]]
  :main ^:skip-aot lunch-bot.core
  :target-path "target/%s"

  :plugins [[lein-ring "0.8.10"]]

  :ring {:handler lunch-bot.handler/app
         :nrepl   {:start? true
                   :port   9998}}

  :profiles {:uberjar {:aot :all}
             :dev     {:dependencies [[javax.servlet/servlet-api "2.5"]
                                      [ring-mock "0.1.5"]]}})
