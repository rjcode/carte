;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns carte.fixtures
  (:use (clojure test)
        (carte sql model core migrations)))

(defmacro t [desc & body] `(testing ~desc ~@body))

(defn attr-as [table col]
  (let [table (name table)
        col (name col)]
    (str table "." col " AS " table "_" col)))

(defn comma-delim-str [coll]
  (apply str (interpose ", " coll)))

(defn attr-list [coll]
  (str (comma-delim-str
        (map #(let [[table attrs] %]
                (comma-delim-str
                 (reduce (fn [a b]
                           (conj a (attr-as table b)))
                         []
                         attrs)))
             coll))))

(def page-table [:page [:id :name :current_version]])
(def category-table [:category [:id :name]])
(def version-table [:version [:id :content]])

(defn select-from [attrs]
  (str "SELECT " attrs " FROM page"))

(def many-to-many-join-query
     (str (select-from
           (attr-list [page-table category-table]))
          " LEFT JOIN page_category ON page.id = page_category.page_id "
          "LEFT JOIN category ON page_category.category_id = category.id"))

(def one-to-many-join-query
     (str (select-from
           (attr-list [page-table version-table]))
          " LEFT JOIN version ON page.id = version.page_id"))

(def one-and-many-to-many-join-query
     (str (select-from
           (attr-list [page-table category-table version-table]))
          " LEFT JOIN page_category ON page.id = page_category.page_id "
          "LEFT JOIN category ON page_category.category_id = category.id "
          "LEFT JOIN version ON page.id = version.page_id"))

(def fixture-join-flat
     [[{:page_id 7 :page_name "one" :page_current_version 1
        :category_id nil :category_name nil} 
       {:page_id 8 :page_name "two" :page_current_version 2
        :category_id 1 :category_name "Clojure"}
       {:page_id 8 :page_name "two" :page_current_version 2
        :category_id 3 :category_name "SICP"}
       {:page_id 9 :page_name "three" :page_current_version 3
        :category_id 1 :category_name "Clojure"}]])

(def fixture-join-nested
     [{:id 7 :name "one" :current_version 1 :categories []}
      {:id 8 :name "two" :current_version 2
       :categories [{:id 1 :name "Clojure"}
                    {:id 3 :name "SICP"}]}
      {:id 9 :name "three" :current_version 3
       :categories [{:id 1 :name "Clojure"}]}])

(def table-key :carte.core/table)
(def orig-key :carte.core/original)

(def fixture-model-many-to-many
     (model
      (category [:id :name])
      (page [:id :name :current_version]
            (many-to-many :category))))

(def fixture-model-one-to-many
     (model
      (version [:id :content])
      (page [:id :name :current_version]
            (one-to-many versions :version :page_id))))

(def fixture-model-one-and-many-to-many
     (model
      (category [:id :name])
      (version [:id :content])
      (page [:id :name :current_version]
            (many-to-many :category)
            (one-to-many :version))))

(defmulti join-fixture (fn [type _ _] type))

(defmethod join-fixture :many-to-many [type from to]
  {:type type
   :table to
   :alias (pluralize-table to)
   :link (keyword (str (name from) "_" (name to)))
   :from (keyword (str (name from) "_id"))
   :to (keyword (str (name to) "_id"))})

(def fixture-join-category
     (join-fixture :many-to-many :page :category))

(def fixture-join-page
     {:type :many-to-many
      :table :page
      :alias :pages
      :link :page_category
      :from :category_id
      :to :page_id})

(def fixture-join-version
     {:type :one-to-many
      :table :version
      :alias :versions
      :link :page_id
      :cascade-delete false})

(def fixture-artist-album-model
     {:model {:album {:attrs [:id :title]
                      :alias :albums
                      :joins #{(join-fixture :many-to-many :album :artist)}}
              :artist {:attrs [:id :name]
                       :alias :artists
                       :joins #{{:type :many-to-many
                                 :table :album
                                 :alias :albums
                                 :link :album_artist
                                 :from :artist_id
                                 :to :album_id}}}}})

;; In order for the following functions to work, you will need to
;; create the database carte_test_db.

;; CREATE DATABASE carte_test_db;
;; GRANT ALL PRIVILEGES ON carte_test_db.* TO carte_db_user@localhost
;;   IDENTIFIED BY '123456789';

(def db {:connection {:classname "com.mysql.jdbc.Driver"
                      :subprotocol "mysql"
                      :subname "//localhost/carte_test_db"
                      :user "carte_db_user"
                      :password "123456789"}})

(def sample-data-model
     (model
      (genre [:id :name])
      (track [:id :name]
             (belongs-to :album))
      (album [:id :title :release_date]
             (many-to-many :artist)
             (many-to-one :genre))
      (artist [:id :name])))

(def sample-db (merge db sample-data-model))

(def ! (partial save-or-update sample-db))
(def $ (partial fetch sample-db))
(def $1 (partial fetch-one sample-db))

(defn build-test-database []
  (do
    (try
     (create-key-value-table (partial sql-do-commands db))
     (catch Exception _ false))
    (try
     (! :key_value {:key_name "database-version" :value "20100524000"})
     (migrate db "carte.sample-migrations")
     true
     (catch Exception _ false))))

;; Notice that we do not explicitly delete tracks here because they
;; belong-to albums.

(defn delete-all-test-data []
  (do
    (doseq [next ($ :album_artist)]
      (delete-record sample-db next))
    (doseq [next ($ :album)]
      (delete-record sample-db next))
    (doseq [next ($ :genre)]
      (delete-record sample-db next))
    (doseq [next ($ :artist)]
      (delete-record sample-db next)))
  (let [tracks ($ :track)]
    (if (seq tracks)
      (do (println "!Warning! tracks were not automatically deleted.")
          (doseq [next tracks]
            (delete-record sample-db next))))))

(def the-black-keys #{"Dan Auerbach" "Patrick Carney"})
(def the-white-stripes #{"Jack White" "Meg White"})
(def the-raconteurs #{"Jack White"
                      "Brenden Benson"
                      "Jack Lawrence"
                      "Patrick Keeler"})

(def artists (set
              (distinct
               (concat the-black-keys
                       the-white-stripes
                       the-raconteurs))))

(def albums #{"Magic Potion"
              "Thickfreakness"
              "Elephant"
              "White Blood Cells"
              "Broken Boy Soldiers"})

(def magic-potion-tracks #{"Just Got To Be"
                           "Your Touch"
                           "You're the One"
                           "Strange Desire"})

(def elephant-tracks #{"Seven Nation Army"
                       "Black Math"
                       "Girl, You Have No Faith In Medicine"})

(def broken-boy-soldiers-tracks #{"Steady As She Goes"
                                  "Level"
                                  "Hands"
                                  "Together"
                                  "Call It a Day"})

(def genres #{"Blues/Rock"
              "Rock"})

(defn add-artists-to-album [album artists]
  (if (seq artists)
    (do
      (! (conj-in ($1 :album {:title album} :with :artists)
                  [:artists]
                  ($1 :artist {:name (first artists)})))
      (recur album (rest artists)))))

(defn add-tracks-to-album [album tracks]
  (if (seq tracks)
    (do
      (! (conj-in ($1 :album {:title album} :with :tracks)
                  [:tracks]
                  ($1 :track {:name (first tracks)})))
      (recur album (rest tracks)))))

(defn add-release-date-to-album [album date]
  (! (assoc ($1 :album {:title album}) :release_date date)))

(defn set-genre [album genre]
  (! (-> ($1 :album {:title album} :with :genre)
         (assoc :genre ($1 :genre {:name genre})))))

(defn default-test-data []
  (do
    (! :album (map #(hash-map :title %) albums))
    (! :artist (map #(hash-map :name %) artists))
    (! :genre (map #(hash-map :name %) genres))
    (! :track (map #(hash-map :name %) (concat magic-potion-tracks
                                               elephant-tracks
                                               broken-boy-soldiers-tracks)))
    (add-artists-to-album "Magic Potion" the-black-keys)
    (add-artists-to-album "Thickfreakness" the-black-keys)
    (add-artists-to-album "Elephant" the-white-stripes)
    (add-artists-to-album "Broken Boy Soldiers" the-raconteurs)

    (add-release-date-to-album "Magic Potion" (carte-date-time 2006 9 12))
    (add-release-date-to-album "Thickfreakness" (carte-date-time 2003 4 8))
    (add-release-date-to-album "Elephant" (carte-date-time 2003 4 1))
    (add-release-date-to-album "Broken Boy Soldiers" (carte-date-time 2006 5 16))

    (add-tracks-to-album "Magic Potion" magic-potion-tracks)
    (add-tracks-to-album "Elephant" elephant-tracks)
    (add-tracks-to-album "Broken Boy Soldiers" broken-boy-soldiers-tracks)

    (set-genre "Magic Potion" "Blues/Rock")
    (set-genre "Thickfreakness" "Blues/Rock")
    (set-genre "Elephant" "Rock")
    (set-genre "Broken Boy Soldiers" "Rock")))

(defn ensure-test-database
  "Ensure that the database exists and contains the tables that you will
   need."
  []
  (do
    (try
     (let [v ($ :key_value {:key_name "database-version"})]
       (if (not (seq v))
         (build-test-database)))
     (catch Exception _ (build-test-database)))
    (try
     (migrate db "carte.sample-migrations")
     (catch Exception _ false))))

(defmacro with-test-database [data-set & body]
  `(do
     (if (~'ensure-test-database)
       (do
         (~data-set)
         (try
          ~@body
          (catch Exception ~'_ false)
          (finally (~'delete-all-test-data))))
       (println "!!WARNING!! Not running tests against mysql."))))
