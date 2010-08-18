(defproject carte/carte "0.2.0-SNAPSHOT"
  :description "Relational Persistence for Clojure."
  :dependencies [[org.clojure/clojure "1.2.0-RC1"]
                 [org.clojure/clojure-contrib "1.2.0-RC1"]
                 [inflections "0.4-SNAPSHOT"]]
  :dev-dependencies [[jline "0.9.94"]
                     [mysql/mysql-connector-java "5.1.6"]
                     [deview/lein-deview "1.2.0-SNAPSHOT"]]
  :deview-server 9002)
