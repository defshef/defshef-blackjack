(defproject defshef.blackjack "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "MIT"}
  :pedantic? :abort
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [expectations "2.0.9" :exclusions [org.clojure/clojure]]]
  :plugins [[lein-autoexpect "1.8.1-SNAPSHOT"]])
