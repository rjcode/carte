(ns carte.fixtures
  (:use (clojure test)
        (carte sql model core migrations
               [sample-migrations :only (create-key-value-table)])))

(defmacro t [desc & body] `(testing ~desc ~@body))

(defn attr-as [table col]
  (let [table (name table)
        col (name col)]
    (str table "." col " as " table "_" col)))

(defn comma-delim-str [coll]
  (apply str (interpose ", " coll)))

(defn attr-list [coll]
  (str " "
       (comma-delim-str
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
  (str "SELECT" attrs " FROM page"))

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

(def fixture-join-category
     {:type :many-to-many
      :table :category
      :alias :categories
      :link :page_category
      :from :page_id
      :to :category_id})

(def fixture-join-version
     {:type :one-to-many
      :table :version
      :alias :versions
      :link :page_id})

(def fixture-artist-album-model
     {:album {:attrs [:id :title]
              :joins #{{:type :many-to-many
                        :table :artist
                        :alias :artists
                        :link :album_artist
                        :from :album_id
                        :to :artist_id}}}
      :artist {:attrs [:id :name]
               :alias :artists}})

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

(def data-model
     (model
      (album [:id :title]
             (many-to-many :artist))
      (artist [:id :name]
              (many-to-many :album => :album_artist))))

(def ! (partial save-or-update db data-model))
(def $ (partial query db data-model))
(def $1 (partial query-1 db data-model))

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

(defn delete-all-test-data []
  (do
    (doseq [next ($ :album_artist)]
      (delete-record db next))
    (doseq [next ($ :album)]
      (delete-record db next))
    (doseq [next ($ :artist)]
      (delete-record db next))))

(defn add-artists-to-album [album artists]
  (if (seq artists)
    (do
      (! (conj-in ($1 :album {:title album} :with :artists)
                  [:artists]
                  ($1 :artist {:name (first artists)})))
      (recur album (rest artists)))))

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
              "White Blook Cells"
              "Broken Boy Soldiers"})

(defn default-test-data []
  (do
    (! :album (map #(hash-map :title %) albums))
    (! :artist (map #(hash-map :name %) artists))
    (add-artists-to-album "Magic Potion" the-black-keys)
    (add-artists-to-album "Thickfreakness" the-black-keys)
    (add-artists-to-album "Elephant" the-white-stripes)
    (add-artists-to-album "Broken Boy Soldiers" the-raconteurs)))

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
     (migrate db "sandbar.sample-database-migrations")
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
