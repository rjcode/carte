;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns carte.sql
  "Simple layer on top of clojure.contrib.sql and functions for generating
   sql."
  (:use (carte model)
        (clojure.contrib [str-utils :only (re-gsub re-split)]))
  (:require (clojure.contrib [sql :as sql])
            (clojure [string :as string])
            (clj-time [core :as time]
                      [coerce :as coerce])))


(def *debug* false)

(defmulti ^String to-string type)

(defmethod ^String to-string clojure.lang.Keyword
  [x]
  (name x))

(defmethod ^String to-string :default
  [^Object x]
  (.toString x))

(defn filter-values [rec filter-fn]
  (let [keys (keys rec)
        vals (vals rec)]
    (zipmap keys (map filter-fn vals))))

(defn- wildcard-string?
  "Does this string start with or end with wildcards."
  [^String s]
  (or (. (to-string s) endsWith "*")
      (. (to-string s) startsWith "*")))

(defn- replace-wildcard
  "Replace all wildcard characters with SQL % characters."
  [s]
  (cond (wildcard-string? s) (re-gsub #"[*]" "%" s)
        :else s))

(defmulti coerce-out class)

(defmethod coerce-out :default [field] field)

(defmethod coerce-out org.joda.time.DateTime [field]
           (coerce/to-date
            (time/from-time-zone field (time/default-time-zone))))

(defmethod coerce-out String [field]
           (replace-wildcard field))

(defmulti coerce-in class)

(defmethod coerce-in :default [field] field)

(defmethod coerce-in java.util.Date [field]
           (time/to-time-zone (coerce/from-date field)
                              (time/default-time-zone)))

(defmethod coerce-in java.sql.Timestamp [^java.sql.Timestamp field]
           (time/to-time-zone (coerce/from-long (.getTime field))
                              (time/default-time-zone)))

(defn filter-into-db
  "Filter values before going into the database."
  [rec]
  (filter-values rec coerce-out))

(defn filter-out-of-db
  "Filter values as they come out of the database."
  [rec]
  (filter-values rec coerce-in))

(defn with-transaction
  "Execute a no argument function within a transaction."
  [db f]
  (sql/with-connection (:connection db)
    (sql/transaction (f))))

(defn sql-drop-table
  "Drop the table from the database."
  [db table]
  (sql/with-connection (:connection db)
      (sql/drop-table table)))

(defn sql-do-commands
  "Execute arbitrary SQL commands."
  [db commands]
  (sql/with-connection (:connection db)
    (sql/do-commands commands)))

(defn sql-insert
  "Insert the record into the table. Map keys correspond to column names."
  [db table rec]
  (when *debug*
    (println (str "inserting record into " table ": " rec)))
  (let [rec (filter-into-db rec)]
    (with-transaction db
      #(sql/insert-records table rec))))

(defn sql-update
  "Update the record with this id with the values in the map. Map keys
   correspond to column names."
  [db table id rec]
  (when *debug*
    (println (str "updating record in " table ": " rec)))
  (let [rec (filter-into-db rec)]
    (with-transaction db 
      #(sql/update-values table ["id=?" id] rec))))

(defn sql-delete
  "Delete records from the table using the provided where vector."
  [db table where]
  (when *debug*
    (println (str "deleting record in " table " where " where)))
  #(sql/delete-rows table where))

(defn sql-query
  "Run a query using raw sql of the form
   [\"SELECT * FROM table WHERE name = ? and age = ?\" \"joe\" 42]."
  [db sql]
  (sql/with-connection (:connection db)
    (sql/with-query-results res
      sql
      (into [] (map filter-out-of-db res)))))

(defn execute-multiple-selects
  "Given multiple select queries, run each of them and return a vector of
   results."
  [db table selects]
  (when *debug*
    (doseq [next-select selects]
      (println next-select)))
  (sql/with-connection (:connection db)
    (reduce (fn [results next-select]
              (sql/with-query-results res
                next-select
                (conj results (into [] (map filter-out-of-db res)))))
            []
            selects)))

(defmulti schema-value (fn [k _] k))

(defmethod schema-value :default [k value]
           value)

(defmethod schema-value :nullable? [k value]
           (case value
                 "YES" true
                 false))

(defmethod schema-value :key [k value]
           (case value
                 "PRI" :primary
                 "MUL" :multiple
                 :other))

(defmethod schema-value :table [k value]
           (keyword value))

(defmethod schema-value :column [k value]
           (keyword value))

(defmethod schema-value :type [k value]
           (case value
                 "bigint" :bigint
                 "varchar" :string
                 :other))

(defn schema-information [db]
  (let [db-name (last (string/split (-> db :connection :subname) #"/"))
        query (str "select * from information_schema.columns "
                   "where table_schema=?")]
    (map #(reduce (fn [a b]
                    (if-let [new-key (case (key b)
                                           :table_name :table
                                           :column_name :column
                                           :ordinal_position :position
                                           :column_default :default
                                           :is_nullable :nullable?
                                           :data_type :type
                                           :character_maximum_length :length
                                           :column_key :key
                                           nil)]
                      (let [new-val (schema-value new-key (val b))]
                        (assoc a new-key new-val))
                      a))
                  {}
                  %)
         (sql-query db [query db-name]))))

(defn columns-in-order [db table]
  (sort-by :position
           (filter #(= (:table %) table)
                   (schema-information db))))

;;
;; SQL Generation
;;

(defn interpose-str [sep coll]
  (apply str (interpose sep coll)))

(defn- create-column-name-pattern-from-sql
  "Given any sql where string, generate a pattern that will match all of the
   column names in the string."
  [^String s]
  (let [re #"\W|and|is|not|null|or|like|\d"]
    (re-pattern
     (interpose-str "|"
                    (distinct
                     (filter #(not (empty? %))
                             (re-split re
                                       (.toLowerCase s))))))))

(defn- qualify-where-string
  "Given the where part of an SQL query string, qualify each column name with
   prefix."
  [prefix s]
  (let [p (create-column-name-pattern-from-sql s)]
    (re-gsub p #(str prefix "." %) s)))

;; The next few functions create where-seqs. A where-seq is a
;; sequence where the first element is a parameterized query string
;; and the remaining elements are the query parameters. For exmaple:
;; ["name = ? AND and < ?" "John" 20].

(defn- key-value->where-seq*
  "Create where-seq when the key is not :where."
  [prefix key value]
  [(str (if prefix
          (str (name prefix) "."))
        (name key)
        (cond (nil? value) " IS NULL"
              (wildcard-string? value) " like ?"
              :else " = ?"))
   value])

(defn key-value->where-seq
  "Given a criteria key and value, create a where-seq [string p1 p2 ... pN].
   For exmaple:
   {:name \"John\"} => [\"name = ?\" \"John\"]
   {:where [\"id > ?\" 7]} => [\"id > ?\" 7]"
  ([key value]
     (key-value->where-seq nil key value))
  ([prefix key value]
     (if (= key :where)
       (if prefix
           (concat [(qualify-where-string (name prefix) (first value))]
                   (rest value))
           value)
       (key-value->where-seq* prefix key value))))

(defn- criteria->queries-and-params
  "Reduce the criteria map to a list of query string and a list of params.
   Returns two lists."
  [prefix m]
  (reduce
   (fn [a b]
     (let [c (key-value->where-seq prefix
                                    (key b)
                                    (val b))]
       [(conj (first a) (first c))
        (concat (last a) (rest c))]))
   [[] []]
   m))

(defn map->where-seq
  "Given a single map with multiple keys and values, create a where-seq
   [string p1 p2 ... pN]. The result that corresponds to each key/value
   pair will be interposed with an AND."
  ([m]
     (map->where-seq nil m))
  ([prefix m]
     (let [result (criteria->queries-and-params prefix m)]
       (concat
        [(interpose-str " AND " (first result))] (last result)))))

(defn- or-where-seq
  "Build a where-seq from a seq of query strings and a seq of values
   interposed by OR. If there is more that one query string then group with
   parens."
  [strings values]
  (if (seq strings)
    (cons
     (interpose-str " OR " (if (> (count strings) 1)
                             (map #(str "(" % ")") strings)
                             strings))
     (map coerce-out (filter #(not (nil? %)) values)))))

(defn criteria->where-seq
  "Given a list of maps for a single table, create a where-seq
   [string p1 p2 ... pN]. All items within a map are interposed with AND
   and the results of each map are interposed with OR."
  ([criteria]
     (criteria->where-seq nil criteria))
  ([table criteria]
     (loop [query-strings []
            values []
            criteria criteria]
       (if-let [next (first criteria)]
         (if-let [[qs & v] (map->where-seq table next)]
           (recur (conj query-strings qs)
                  (concat values v)
                  (rest criteria))
           (recur query-strings values (rest criteria)))
         (or-where-seq query-strings values)))))

;; TODO - This code would be much simpler if we always qualify column
;; names. You wouldn't need to know about the table name here because
;; this information is included in the criteria-map.

;; TODO - You may need to put parns around each part of the new query string.
(defn- merge-where-seqs
  "Merge a sequence of where-seqs into one. These specs are for different
   tables in a join so they will be interposed with AND."
  [coll]
  (if (seq coll)
    (reduce
     (fn [a b]
       (concat
        (if (empty? a)
          [(first b)]
          [(apply str (interpose " AND " [(first a)
                                          (first b)]))])
        (rest a)
        (rest b)))
     []
     coll)))

(defn query->where-seq
  "Given a parsed query with criteria for one or more tables, create a
   where-seq [string p1 p2 ... pN]."
  [table query]
  {:pre [(or (= (count query) 1)
             (not (nil? table)))]}
  (if (nil? table)
    (criteria->where-seq table (val (first query)))
    (loop [result []
           query query]
      (if (seq query)
        (let [next (first query)]
          (recur (conj result
                       (criteria->where-seq (key next) (val next)))
                 (rest query)))
        (merge-where-seqs result)))))

(defn- criteria->order-seq [table columns]
  (loop [result []
         columns (partition 2 columns)]
    (if (seq columns)
      (recur (conj result (let [[col dir] (first columns)]
                            (str (if table
                                   (str (name table) "."))
                                 (name col)
                                 (if (= dir :desc) " DESC"))))
             (rest columns))
      result)))

(defn query->order-by [table query]
  {:pre [(or (= (count query) 2)
             (not (nil? table)))]}
  (let [order-seq (if (nil? table)
                    (criteria->order-seq table (first (rest query)))
                    (loop [result []
                           query (partition 2 query)]
                      (if (seq query)
                        (let [[table columns] (first query)]
                          (recur (concat result
                                         (criteria->order-seq table columns))
                                 (rest query)))
                        result)))]
    (if (empty? order-seq)
      ""
      (str " ORDER BY " (interpose-str ", " order-seq)))))

(defn- create-table-qualified-names
  "Given a table and a list of attributes, return a list of qualified names
   with aliases."
  [table attrs]
  (map #(let [r (name table)
              a (name %)]
          (str r "." a " AS " r "_" a))
       attrs))

;; TODO - You are forcing the :id to be one of the attributes because
;; you need it later when organizing records. You also need the id
;; when you save or delete a record. One way to get around this is to
;; always put the id in the metadata and use that for sorting and
;; saving records. 

(defn- column-seq
  "Get the list of attributes for a specific table. If the user has requested
   a list of attributes then use that list ensuring that it contains the id
   col. If no list is requested then use the list in model if it exists."
  [db table requested-attrs]
  {:pre [(not (nil? table))]}
  (let [all-attrs (-> db :model table :attrs)]
    (if-let [attrs (table requested-attrs)]
      (if (contains? (set attrs) :id)
        attrs
        (cons :id attrs))
      all-attrs)))

(defmulti left-join-sql (fn [db & args] (-> db :connection :subprotocol)))

(defmethod left-join-sql :default [db lt lcol rt alias rcol]
           (let [[lt lcol rt alias rcol] (map name [lt lcol rt alias rcol])
                 join-table (if (= rt alias)
                              rt
                              (str rt " " alias))]
             (str " LEFT JOIN " join-table
                  " ON " lt "." lcol " = " alias "." rcol)))

(defmulti join-sql (fn [_ _ join] (:type join)))

(defmethod join-sql :many-to-many [db table join]
  (let [{r-table :table link :link from :from to :to alias :alias} join]
    (str (left-join-sql db table :id link link from)
         (left-join-sql db link to r-table alias :id))))

(defmethod join-sql :one-to-many [db table join]
  (let [{r-table :table link :link alias :alias} join]
    (left-join-sql db table :id r-table alias link)))

(defmethod join-sql :many-to-one [db table join]
  (let [{r-table :table link :link alias :alias} join]
    (left-join-sql db table link r-table alias :id)))

(defmethod join-sql :default [db table join] "")

(defn joins-sql
  "Given a model, table and list of requested joins, create the join SQL
   string."
  [db alias requested-joins]
  (let [table (:table requested-joins)]
    (loop [join-seq []
           joins (:joins requested-joins)]
      (if (seq joins)
        (let [join (find-join-by (:model db) table :alias (first joins))]
          (recur
           (conj join-seq (join-sql db alias join))
           (rest joins))) 
        join-seq))))

(defn each-join
  "The joins map may have many entries. For each entry in the map, call an
   sql generator function accumulating the results in a sequence. After all
   entries are processed, create an SQL string from the distinct results."
  ([table joins sql-fn]
     (each-join table joins "" sql-fn))
  ([table joins sep sql-fn]
     (loop [sql (sql-fn table (table joins))
            joins (dissoc joins table)]
       (if (seq joins)
         (let [next (first joins)]
           (recur (concat sql
                          (sql-fn (key next) (val next)))
                  (rest joins)))
         (interpose-str sep (distinct sql))))))

(defn add-limit [query limit]
  (str query " LIMIT " limit))

(defn add-paging [query page]
  (str query
       " LIMIT "
       (:size page)
       " OFFSET "
       (* (:num page)
          (:size page))))

(defn select-attrs-from-joins
  "Create a list of aliased attributes for a select query."
  [db attrs joins]
  (loop [result []
         joins joins]
    (if (seq joins)
      (recur
       (concat result
               (let [next (first joins)
                     alias (key next)
                     table (:table (val next))
                     join-aliases (:joins (val next))]
                 (concat (create-table-qualified-names
                          alias
                          (column-seq db table attrs))
                         (apply concat
                                (map #(let [{t :table} (find-join-by (:model db)
                                                                     table
                                                                     :alias
                                                                     %)]
                                        (create-table-qualified-names
                                         %
                                         (column-seq db t attrs)))
                                     join-aliases)))))
       (rest joins))
      (distinct result))))

(defn select-attrs [db table attrs joins]
  (let [c (if (and joins (not (empty? joins)))
            (select-attrs-from-joins db attrs joins)
            (create-table-qualified-names table
                                          (column-seq db table attrs)))]
    (if (and c (not (empty? c)))
      (interpose-str ", " c)
      "*")))

(defn selects
  "Create a sequence of selects where each can be passed to with-query-results.
The parsed query may have the keys: attrs, criteria and joins. Currently,
this sequence contains one query but in the future it may return more than
one as we start to implement more complicated queries and support more
backends."
  [db table parsed-query]
  [(let [{:keys [attrs criteria joins order-by limit page]} parsed-query
         select-part
         (str "SELECT " (select-attrs db table attrs joins)
              " FROM " (name table)
              (each-join table joins
                         #(joins-sql db %1 %2)))
         where-part (query->where-seq table criteria)
         order-by-part (query->order-by table order-by)]
     (let [q-string (if where-part
                      (str select-part
                           " WHERE "
                           (first where-part)
                           order-by-part)
                      (str select-part order-by-part))
           params (if where-part (rest where-part))
           q-string (cond limit
                          (add-limit q-string limit)
                          page
                          (add-paging q-string page)
                          :else q-string)]
       (if params
         (vec (cons q-string params))
         [q-string])))])

(defn count-query-results
  "Return the number of records in the table."
  [db table parsed-query]
  (let [{:keys [criteria joins]} parsed-query
        where-part (query->where-seq table criteria)
        select-part (str "SELECT COUNT(*) AS c FROM "
                         (name table)
                         (each-join table joins
                                    #(joins-sql db %1 %2)))
        q-string (if where-part
                   (str select-part " WHERE " (first where-part))
                   select-part)
        params (if where-part (rest where-part))
        query (if params
                (vec (cons q-string params))
                [q-string])
        result (sql-query db query)]
    (:c (first result))))

;;
;; Schema Creation and Modification for use with Migrations
;;

(defn drop-table-fn [table]
  (fn [db]
    (sql-do-commands db
                     (str "DROP TABLE " (name table)))))

(defn create-table-fn [table & spec]
  (fn [db]
    (sql-do-commands
     db
     (str
      "CREATE TABLE " (name table)
      " (id bigint(20) NOT NULL AUTO_INCREMENT,"
      (apply str (interpose "," spec))
         ", PRIMARY KEY (id)
         ) ENGINE=InnoDB DEFAULT CHARSET=utf8;"))))

(defn alter-table-fn [table spec]
  (fn [db]
    (sql-do-commands
     db
     (str "ALTER TABLE " (name table)
          spec))))

(defn add-col [col & spec]
  (str " ADD " col
       (let [after (drop-while #(not (= % :after)) spec)]
         (if (seq after)
           (str " AFTER " (name (last after)) ";")))))

(defn drop-col [col]
  (str " DROP COLUMN " (name col) ";"))

(defmulti col (fn [_ type & spec] type))

(defn column-template [col-name datatype spec]
  (str (name col-name) " " datatype
       (if (contains? (set spec) :not-null)
         " NOT NULL"
         "")))

(defn get-spec [spec k default]
  (let [spec-map (first (filter map? spec))]
    (if-let [v (k spec-map)]
      v
      default)))

(defmethod col :string [col-name type & spec]
           (column-template col-name
                            (str "varchar(" (get-spec spec :size 255) ")")
                            spec))

(defmethod col :id [col-name type & spec]
           (column-template col-name
                            (str "bigint(" (get-spec spec :size 20) ")")
                            spec))

(defmethod col :decimal [col-name type & spec]
           (let [m (max 1 (min 65 (get-spec spec :left 65)))
                 d (max 0 (min 30 (get-spec spec :right 30)))]
             (column-template col-name
                              (str "decimal(" m ", " d ")")
                              spec)))

(defmethod col :char [col-name type & spec]
           (column-template col-name "char(1)" spec))

(defmethod col :date [col-name type & spec]
           (column-template col-name "date" spec))

(defmethod col :timestamp [col-name type & spec]
           (column-template col-name "timestamp" spec))

(defmethod col :text [col-name type & spec]
           (column-template col-name "text" spec))

(defn- foreign-key [col]
  (str "FK_" col "_key"))

(defn constraint
  ([this-col _ table that-col]
     (constraint this-col table that-col))
  ([this-col table that-col]
     (let [this-name (name this-col)
           foreign-key (foreign-key this-name)]
       (str "KEY " foreign-key " (" this-name "),"
            "CONSTRAINT " foreign-key " FOREIGN KEY (" this-name ") "
            "REFERENCES " (name table) " (" (name that-col) ")"))))

(defn add-constraint
  ([this-col _ table that-col]
     (add-constraint this-col table that-col))
  ([this-col table that-col]
     (let [this-name (name this-col)
           foreign-key (foreign-key this-name)]
       (str " ADD CONSTRAINT " foreign-key " FOREIGN KEY (" this-name ") "
            "REFERENCES " (name table) " (" (name that-col) ")"))))

(defn drop-constraint [col]
  (str " DROP FOREIGN KEY " (foreign-key (name col))))

(defn unique-key [& col-names]
  (let [names (map name col-names)
        key-name (apply str (interpose "_" names))
        field-list (apply str (interpose ", " names))]
    (str "UNIQUE KEY " key-name " (" field-list ")")))

(defn create-key-value-table [create-table-fn]
  (create-table-fn
    "create table key_value 
       (id bigint not null auto_increment,
        key_name varchar(255) not null,
        value varchar(255) not null,
        primary key (id)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8;"))

