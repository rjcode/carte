(defproject carte/carte "0.1.2"
  :description "Relational Persistence for Clojure."
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]
                 [inflections "0.3"]]
  :dev-dependencies [[lein-clojars "0.5.0-SNAPSHOT"]
                     [jline "0.9.94"]
                     [mysql/mysql-connector-java "5.1.6"]
                     [deview/lein-deview "1.0.5"]]
  :namespaces [carte.core
               carte.sql
               carte.model
               carte.migrations]
  :deview-server 9002)
