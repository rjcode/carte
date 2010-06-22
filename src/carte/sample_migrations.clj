(ns carte.sample-migrations
  (:require [carte.sql :as database]))

(defn create-key-value-table [create-table-fn]
  (create-table-fn
    "create table key_value 
       (id bigint not null auto_increment,
        key_name varchar(255) not null,
        value varchar(255) not null,
        primary key (id)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8;"))

(defn migration-20100524000 []
  {:forward (fn [db] nil)})

(defn & [fn-seq]
  (fn [db]
    (doseq [next fn-seq]
      (next db))))

(defn drop-table [table]
  (fn [db]
    (database/sql-do-commands
     db
     (str "DROP TABLE " (name table)))))

(defn create-table [table & spec]
  (fn [db]
    (database/sql-do-commands
     db
     (str
      "CREATE TABLE " (name table)
      " (id bigint(20) NOT NULL AUTO_INCREMENT,"
         (apply str (interpose "," spec))
         ", PRIMARY KEY (id)
         ) ENGINE=InnoDB DEFAULT CHARSET=utf8;"))))

(defn alter-table [table spec]
  (fn [db]
    (database/sql-do-commands
     db
     (str "ALTER TABLE " (name table)
          spec))))

(defn add-col [col & spec]
  (str " ADD COLUMN " col
       (let [after (drop-while #(not (= % :after)) spec)]
         (if (seq after)
           (str " AFTER " (name (last after)) ";")))))

(defn drop-col [col]
  (str " DROP COLUMN " (name col) ";"))

(defmulti col (fn [_ type & spec] type))

(defmethod col :string [col-name type & spec]
  (str (name col-name) " varchar(255)"
       (if (contains? (set spec) :not-null)
         " NOT NULL"
         "")))

(defmethod col :id [col-name type & spec]
  (str (name col-name) " bigint(20)"
       (if (contains? (set spec) :not-null)
         " NOT NULL"
         "")))

(defn constraint
  ([this-col _ table that-col]
     (constraint this-col table that-col))
  ([this-col table that-col]
     (let [this-name (name this-col)
           foreign-key (str "FK_" this-name "_key")]
       (str "KEY " foreign-key " (" this-name "),"
            "CONSTRAINT " foreign-key " FOREIGN KEY (" this-name ") "
            "REFERENCES " (name table) " (" (name that-col) ")"))))

(defn migration-20100524001 []
  {:forward
   (create-table :album
                 (col :title :string :not-null))
   :back
   (drop-table :album)})

(defn migration-20100525000 []
  {:forward
   (create-table :artist
                 (col :name :string :not-null))
   :back
   (drop-table :artist)})

(defn migration-20100525001 []
  {:forward
   (create-table :album_artist
                 (col :album_id :id :not-null)
                 (col :artist_id :id :not-null)
                 (constraint :album_id :=> :album :id)
                 (constraint :artist_id :=> :artist :id))
   :back
   (drop-table :album_artist)})

(defn migration-20100601000 []
  {:forward
   (create-table :genre
                 (col :name :string :not-null))
   :back
   (drop-table :genre)})

;; TODO - add constraints for the new stuff that you added to the
;; table.

(defn migration-20100601001 []
  {:forward
   (alter-table :album
                (add-col (col :genre_id :id) :after :title))
   :back
   (alter-table :album (drop-col :genre_id))})

(defn migration-20100601002 []
  {:forward
   (create-table :track
                 (col :name :string :not-null)
                 (col :album_id :id))
   :back
   (drop-table :track)})
