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
                         [java-utils :only (as-str)]
                         [str-utils :only (re-gsub re-split)])))

(def *debug* false)

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
;;;;;;;;;;;;;;;;;

(defn- create-column-name-pattern-from-sql
  "Given any sql where string, generate a pattern that will match all of the
   column names in the string."
  [s]
  (let [re #"\W|and|is|not|null|or|like|\d"]
    (re-pattern
     (apply str
            (interpose "|"
                       (distinct
                        (filter #(not (empty? %))
                                (re-split re
                                          (.toLowerCase s)))))))))

(defn- qualify-where-string
  "Given the where part of an SQL query string, qualify each column name with
   prefix."
  [prefix s]
  (let [p (create-column-name-pattern-from-sql s)]
    (re-gsub p #(str prefix "." %) s)))

(defn- wildcard-string?
  "Does this string start with or end with wildcards."
  [s]
  (or (. (as-str s) endsWith "*")
      (. (as-str s) startsWith "*")))

(defn replace-wildcard
  "Replace all wildcard characters with SQL % characters."
  [s]
  (cond (wildcard-string? s) (re-gsub #"[*]" "%" s)
        :else s))

(defn key-value->where-spec
  "Given a criteria key and value, create the appropriate SQL. Return a
   vector containing the query string followed by its parameters.
   For exmaple:
   {:name \"John\"} => [\"name = ?\" \"John\"]
   {:where [\"id > ?\" 7]} => [\"id > ?\" 7]"
  ([key value]
     (key-value->where-spec nil key value))
  ([prefix key value]
     (if (= key :where)
       (if prefix
           (concat [(qualify-where-string (name prefix) (first value))]
                   (rest value))
           value)
       [(str (if prefix
               (str (name prefix) "."))
             (name key)
             (cond (nil? value) " IS NULL"
                   (wildcard-string? value) " like ?"
                   :else " = ?"))
        value])))

(defn map->where-spec
  "Given a single map with multiple keys and values, return a vector where
   the first element is the query string and remaining elements are the query
   parameters. Because this is operating on a single map, all of the
   individual predicates will be interposed with AND."
  ([m]
     (map->where-spec nil m))
  ([prefix m]
     (let [result (reduce (fn [a b]
                            (let [c (key-value->where-spec prefix
                                                                 (key b)
                                                                 (val b))]
                              [(conj (first a) (first c))
                               (concat (last a) (rest c))]))
                          [[] []]
                          m)]
       (concat
        [(apply str (interpose
                     " AND "
                     (first result)))] (last result)))))

(defn criteria->where-spec
  "Given a list of maps for a single table, create a vector where the first
   element is the query string and the remaining elements are the parameters.
   All items within a map are interposed with AND and the results of each
   map are interposed with OR."
  ([criteria]
     (criteria->where-spec nil criteria))
  ([table criteria]
     (loop [query-strings []
            values []
            criteria criteria]
       (if-let [next (first criteria)]
         (if-let [[qs & v] (map->where-spec table next)]
           (recur (conj query-strings qs)
                  (concat values v)
                  (rest criteria))
           (recur query-strings values (rest criteria)))
         (if (seq query-strings)
           (cons
            (apply str (interpose " OR "
                                  (if (> (count query-strings) 1)
                                    (map #(str "(" % ")") query-strings)
                                    query-strings)))
            (map replace-wildcard (filter #(not (nil? %)) values)))
           nil)))))

;; TODO - This code whould be much simpler if we always qualify column
;; names. You wouldn't need to know about the table name here because
;; this information is included in the criteria-map.

(defn query->where-spec
  "TODO"
  [table query]
  {:pre [(or (= (count query) 1)
             (not (nil? table)))]}
  (if (nil? table)
    (criteria->where-spec table (val (first query)))
    (loop [result []
           query query]
      (if (seq query)
        (let [next (first query)]
          (recur (conj result
                       (criteria->where-spec (key next) (val next)))
                 (rest query)))
        (if (seq result)
            (reduce (fn [a b]
                      (concat
                       (if (empty? a)
                         [(first b)]
                         [(apply str (interpose " AND " [(first a)
                                                         (first b)]))])
                       (rest a)
                       (rest b)))
                    []
                    result)
            nil)))))

(defn to-string [a]
  (cond (keyword? a) (name a)
        :else (.toString a)))

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
    (if order-by-str (str " order by" order-by-str) "")))

(defn create-table-qualified-names [table attrs]
  (map #(let [r (name table)
              a (name %)]
          (str r "." a " as " r "_" a))
       attrs))

;; TODO - You are forcing the :id to be one of the attributes because
;; you need it later when organizing records. You also need the id
;; when you go to save or delete a record. One way to get around this
;; is to always put the id in the metadata and use that for sorting
;; and saving records. 

(defn get-attrs [model table requested-attrs]
  {:pre [(not (nil? table))]}
  (if-let [attrs (table requested-attrs)]
    (if (contains? (set attrs) :id)
      attrs
      (conj attrs :id))
    (-> model table :attrs)))

(defn create-attr-list
  ([model table]
     (create-attr-list model table nil nil))
  ([model table req-attrs]
     (create-attr-list model table req-attrs nil))
  ([model table req-attrs joins]
     {:pre [(not (nil? table))]}
     (if-let [attrs (get-attrs model table req-attrs)]
       (loop [result (create-table-qualified-names table
                                                      attrs)
              joins (table joins)]
         (if (seq joins)
           (let [{type :type many-side :table}
                 (find-join-by model table :alias (first joins))
                 attrs (get-attrs model many-side req-attrs)]
             (recur (concat result (create-table-qualified-names many-side
                                                                    attrs))
                    (rest joins)))
           (apply str " " (interpose ", " result))))
       " *")))

(defn create-many-to-many-join [base-rel result join]
  (let [{rel :table link :link from :from to :to} join]
    (let [[rel link from to] (map name [rel link from to])]
      (str result
           " LEFT JOIN " link " ON " (name base-rel) ".id = " link "." from
           " LEFT JOIN " rel " ON " link "." to " = " rel ".id"))))

(defn create-one-to-many-join [base-rel result join]
  (let [{rel :table link :link} join]
    (let [rel (name rel)
          link (name link)]
      (str result
           " LEFT JOIN " rel " ON " (name base-rel) ".id = " rel "." link))))

(defn create-joins [model table requested-joins]
  (loop [result ""
         joins (table requested-joins)]
    (if (seq joins)
      (let [join (find-join-by model table :alias (first joins))]
        (recur
         (cond (many-to-many? join)
               (create-many-to-many-join table result join)
               (one-to-many? join)
               (create-one-to-many-join table result join)
               :else "")
         (rest joins))) 
      result)))

(defn create-selects
  "Create the select vector that can be passed to with-query-results"
  [model table parsed-query]
  [(let [{:keys [attrs criteria joins]} parsed-query
         select-part (str "SELECT" (create-attr-list model table attrs joins)
                          " FROM " (as-str table)
                          (create-joins model table joins))
         where-part (query->where-spec table criteria)
         order-by-part (create-order-by {})]
     (if where-part
       (vec (cons (str select-part " WHERE " (first where-part) order-by-part)
                  (rest where-part)))
       [(str select-part order-by-part)]))])
