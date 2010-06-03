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
        (clojure.contrib [sql :as sql]
                         [str-utils :only (re-gsub re-split)])))

(def *debug* false)

(defmulti to-string type)

(defmethod to-string clojure.lang.Keyword
  [x]
  (name x))

(defmethod to-string :default
  [x]
  (.toString x))

(defn with-transaction
  "Execute a no argument function within a transaction."
  [db f]
  (sql/with-connection (:connection db)
    (sql/transaction (f)))
  nil)

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
  (with-transaction db
    #(sql/insert-records table rec)))

(defn sql-update
  "Update the record with this id with the values in the map. Map keys
   correspond to column names."
  [db table id rec]
  (when *debug*
    (println (str "updating record in " table ": " rec)))
  (with-transaction db 
    #(sql/update-values table ["id=?" id] rec)))

(defn sql-delete
  "Delete records from the table using the provided where vector."
  [db table where]
  (when *debug*
    (println (str "deleting record in " table " where " where)))
  (with-transaction db
    #(sql/delete-rows table where)))

(defn sql-query
  "Run a query using raw sql of the form
   [\"SELECT * FROM table WHERE name = ? and age = ?\" \"joe\" 42]."
  [db sql]
  (sql/with-connection (:connection db)
    (sql/with-query-results res
      sql
      (into [] res))))

(defn execute-multiple-selects
  "Given multiple select queries, run each of them and return a vector of
   results."
  [conn model table selects]
  (when *debug*
    (doseq [next-select selects]
      (println next-select)))
  (sql/with-connection conn
    (reduce (fn [results next-select]
              (sql/with-query-results res
                next-select
                (conj results (into [] res))))
            []
            selects)))

;; SQL Generation
;;;;;;;;;;;;;;;;;;;

(defn- interpose-str [sep coll]
  (apply str (interpose sep coll)))

(defn- create-column-name-pattern-from-sql
  "Given any sql where string, generate a pattern that will match all of the
   column names in the string."
  [s]
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

(defn- wildcard-string?
  "Does this string start with or end with wildcards."
  [s]
  (or (. (to-string s) endsWith "*")
      (. (to-string s) startsWith "*")))

(defn- replace-wildcard
  "Replace all wildcard characters with SQL % characters."
  [s]
  (cond (wildcard-string? s) (re-gsub #"[*]" "%" s)
        :else s))

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
     (map replace-wildcard (filter #(not (nil? %)) values)))))

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

;; TODO - This code whould be much simpler if we always qualify column
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

;; TODO - Get this working and test it. This is left over from a
;; previous implementation. We don't have a params map in the current
;; implementation.
(defn- create-order-by [params]
  (let [sort-vec (:sort params)
        order-by-str (if (seq sort-vec)
                       (reduce
                        (fn [a b]
                          (if (= 1 (count b))
                            (str a (first b))
                            (str a " " (to-string (last b)) " "
                                 (to-string (first b)))))
                        ""
                        (interpose [","] (partition 2 sort-vec))) nil)]
    (if order-by-str (str " ORDER BY" order-by-str) "")))

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
  [model table requested-attrs]
  {:pre [(not (nil? table))]}
  (if-let [attrs (table requested-attrs)]
    (if (contains? (set attrs) :id)
      attrs
      (conj attrs :id))
    (-> model table :attrs)))

(defn columns-sql
  "Create a comma delimited string containing the all the attributes to query.
   Each attribute will be table qualified and aliased. If there is no model
   and no attributes are requested then return *."
  ([model table]
     (columns-sql model table nil nil))
  ([model table req-attrs]
     (columns-sql model table req-attrs nil))
  ([model table req-attrs joins]
     {:pre [(not (nil? table))]}
     (if-let [attrs (column-seq model table req-attrs)]
       (loop [result (create-table-qualified-names table
                                                   attrs)
              joins joins]
         (if (seq joins)
           (let [{type :type many-side :table}
                 (find-join-by model table :alias (first joins))
                 attrs (column-seq model many-side req-attrs)]
             (recur (concat result (create-table-qualified-names many-side
                                                                 attrs))
                    (rest joins)))
           result))
       ["*"])))

(defn- many-to-many-join-sql
  "Given the left table and the map that describes the join, create the
   many-to-many join SQL string."
  [l-table join]
  (let [{r-table :table link :link from :from to :to} join]
    (let [[l-table r-table link from to]
          (map name [l-table r-table link from to])]
      (str " LEFT JOIN " link " ON " l-table ".id = " link "." from
           " LEFT JOIN " r-table " ON " link "." to " = " r-table ".id"))))

(defn- one-to-many-join-sql
  "Given the left table and the map that describes the join, create the
   one-to-many join SQL string."
  [l-table join]
  (let [{r-table :table link :link} join]
    (let [[l-table r-table link] (map name [l-table r-table link])]
      (str " LEFT JOIN " r-table
           " ON " l-table ".id = " r-table "." link))))

(defn joins-sql
  "Given a model, table and list of requested joins, create the join SQL
   string."
  [model table requested-joins]
  (loop [join-sql []
         joins requested-joins]
    (if (seq joins)
      (let [join (find-join-by model table :alias (first joins))]
        (recur
         (cond (many-to-many? join)
               (conj join-sql (many-to-many-join-sql table join))
               (one-to-many? join)
               (conj join-sql (one-to-many-join-sql table join))
               :else join-sql)
         (rest joins))) 
      join-sql)))

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

(defn selects
  "Create a sequence of selects where each can be passed to with-query-results.
   The parsed query may have the keys: attrs, criteria and joins. Currently,
   this sequence contains one query but in the future it may return more than
   one as we start to implement more complicated queries and support more
   backends."
  [model table parsed-query]
  [(let [{:keys [attrs criteria joins]} parsed-query
         select-part (str "SELECT " (each-join table joins ", "
                                     #(columns-sql model %1 attrs %2))
                          " FROM " (name table)
                          (each-join table joins
                           #(joins-sql model %1 %2)))
         where-part (query->where-seq table criteria)
         order-by-part (create-order-by {})]
     (if where-part
       (vec (cons (str select-part " WHERE " (first where-part) order-by-part)
                  (rest where-part)))
       [(str select-part order-by-part)]))])
