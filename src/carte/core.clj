;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns carte.core
  "Relational mapping, and data transformation."
  (:use (carte sql model)
        (clj-time [core :only (date-time
                               from-time-zone
                               default-time-zone)])
        (clojure [set :only (difference)])
        (clojure.contrib [map-utils :only (deep-merge-with)]))
  (:require (clojure [zip :as zip])))

;;
;; Data Transformations
;;

(defn map-map
  "Produce a new map where the value for each key is the result of applying
   f to each key and value. If this function returns :remove then that key
   will not be added to the new map. Preserves metadata."
  [f m]
  (let [metadata (meta m)]
    (with-meta
      (reduce (fn [a b]
               (let [[k v] b
                     new-v (f k v)]
                 (if (= new-v :remove)
                   a
                   (assoc a k new-v))))
             {}
             m)
      metadata)))

(defn conj-in
  "Returns a new collection with v added to a vector in a nested associative
   data structure. ks is a sequence of keys that will be used to locate the
   target collection."
  [data ks v]
  (let [size (count (get-in data ks))]
    (assoc-in data (conj ks size) v)))

(defn concat-in
  "Same as conj-in but concatinates values."
  [data ks v]
  {:pre [(vector? v)]}
  (let [before (get-in data ks)]
    (assoc-in data ks (vec (concat before v)))))

(defn remove-in
  "Remove the element from the nested collection that matches the given
   pattern."
  [data ks pattern]
  (let [coll (get-in data ks)]
    (assoc-in data
              ks
              (filter #(not (= pattern (select-keys % (keys pattern))))
                      coll))))

(defn find-in [ks data]
  (loop [ks ks
         result data]
    (if (seq ks)
      (recur (rest ks)
             (let [next-test (first ks)]
               (cond (map? next-test)
                     (filter (fn [record]
                               (= next-test
                                  (select-keys record (keys next-test))))
                             result)
                     (keyword? next-test)
                     (reduce (fn [a b]
                               (let [next (next-test b)]
                                 (if (or (vector? next) (list? next))
                                   (concat a next)
                                   (conj a next))))
                             []
                             result))))
      result)))

(declare vary-in-map)
(declare vary-in)

(defn- vary-within-map [m type pattern new-vals]
  (map-map (fn [k v]
             (cond (map? v) (vary-in-map v type pattern new-vals)
                   (or (vector? v) (list? v)) (vary-in v type pattern new-vals)
                   :else v))
           m))

(defn- vary-in-map [m type pattern new-vals]
  (let [m (vary-within-map m type pattern new-vals)]
    (if (and (= type (-> m meta ::table))
             (= (select-keys m (keys pattern)) pattern))
      (if (fn? new-vals)
        (new-vals m)
        (reduce (fn [result [key new-value]]
                  (assoc result key
                         (if (fn? new-value)
                           (new-value (key result))
                           new-value)))
                m
                new-vals))
      m)))

(defn vary-in [coll type pattern new-vals]
  (cond (map? coll) (first (vary-in [coll] type pattern new-vals))
        :else
        (vec (map #(vary-in-map % type pattern new-vals) coll))))

(defn find-first-in [ks v]
  (first (find-in ks v)))

;;
;; Queries
;;

(defn parse-query-part [table alias q]
  (loop [q q
         result {}]
    (if (seq q)
      (let [next (first q)]
        (recur (rest q)
               (cond (vector? next) (merge result {:attrs {alias next}})
                     (map? next)
                     (deep-merge-with concat
                                      result
                                      {:criteria {alias [next]}})
                     :else result)))
      result)))

(declare parse-query)

(defn parse-join-merge [one two]
  (cond (coll? one)
        (concat one two)
        :else two))

(defn parse-join-part
  "Given a model, root-table and a query, produce the joins portion of the
   query data structure. The first item in the query is always :with.
   For the root table :artist, [:with [:albums :with :tracks]] becomes
   {:joins {:artist {:table :artist :joins [:albums]}
            :albums {:table :album :joins [:tracks]}}}
   Each entry in the joins map is an alias for a table. Using the alias as the
   key allows us to join on the same table more than once.
   For the root table :bol, [:with [:origin :with :region]
                                   [:dest :with :region]] becomes
   {:joins {:bol {:table :bol :joins [:origin :dest]}
            :origin {:table :site :joins [:region]}
            :dest {:table :site :joins [:region]}}}"
  [model table alias q]
  (loop [result {}
         joins (rest q)]
    (if (seq joins)
      (let [next (first joins)]
        (recur (if (keyword? next)
                 (deep-merge-with parse-join-merge
                                  result
                                  {:joins {alias {:table table :joins [next]}}})
                 (let [join-alias (first next)
                       join-model (find-join-by model
                                                table
                                                :alias
                                                join-alias)]
                   (deep-merge-with parse-join-merge
                                    result
                                    {:joins {alias {:table table
                                                    :joins [join-alias]}}}
                                    (parse-query model
                                                 (:table join-model)
                                                 join-alias
                                                 (rest next)))))
               (rest joins)))
      result)))

(defn merge-order-vecs [v1 v2]
  (if (= (first v1) (first v2))
    [(first v1) (concat (last v1) (last v2))]
    (concat v1 v2)))

(defn parse-order-by-part [alias query]
  (loop [result {}
         query (rest query)]
    (if (seq query)
      (let [next (first query)]
        (recur (deep-merge-with merge-order-vecs
                                result
                                {:order-by
                                 [alias (if (keyword? next)
                                          [next :asc]
                                          (let [[col dir] next]
                                            [col dir]))]})
               (rest query)))
      result)))

(defn parse-limit-part [query]
  (if (seq query)
    (let [n (first query)
          c (rest query)]
      (if (= n :limit)
        {:limit (first c)}
        {}))
    {}))

(defn parse-page-part [query]
  (if (seq query)
    (let [n (first query)
          c (rest query)]
      (if (= n :page)
        {:page {:num (first c) :size (last c)}}
        {}))
    {})) 

(defn parse-query
  "Create a map with keys :attrs :criteria :joins :order-by :limit and :page"
  ([model table q]
     (parse-query model table table q))
  ([model table alias q]
     {:pre [(not (nil? table)), (not (nil? alias))]}
     (let [[q limit-or-page] (split-with #(not (contains? #{:limit :page} %)) q)
           [query-part join-part] (split-with #(not (= % :with)) q)
           [query-part order-part] (split-with #(not (= % :order-by)) query-part)]
       (deep-merge-with concat
                        (parse-query-part table alias query-part)
                        (parse-order-by-part alias order-part)
                        (parse-join-part model table alias join-part)
                        (parse-limit-part limit-or-page)
                        (parse-page-part limit-or-page)))))

(defn compile-query
  "Transform a sequence of query parameters into a query map."
  [model table q]
  (merge {:root-table table} (parse-query model table q)))

(defn- m-dissoc [m & keys]
  (apply dissoc (into {} m) keys))

(defn keyword-without-prefix [prefix s]
  (keyword
   (.substring s (+ 1 (count prefix)))))

(defn prefixed? [qualified prefix other]
  (and (.startsWith qualified prefix)
       (not (some true? (map #(and (not (.equals qualified %))
                                   (.startsWith qualified %)) other)))))

(defn tables-and-aliases
  "Get a list of all table names and aliases that are used in the model."
  [model]
  (concat (keys model)
          (distinct
           (flatten (map #(map :alias %) (map :joins (vals model)))))))

(defn dequalify-joined-map
  "Create a map that has dequalified key names for this table. The input
   map will have the alias name appended to the front of each key."
  [model table alias m]
  (let [prefix (name alias)
        other-tables (->> (map name (tables-and-aliases model))
                             (filter #(> (count %) (count prefix))))]
    (reduce (fn [a b]
              (let [k (name (key b))]
                (cond (not (table model))
                      (assoc a (if (.startsWith k prefix)
                                 (keyword-without-prefix prefix k)
                                 (key b))
                             (val b))
                      (prefixed? k prefix other-tables)
                      (assoc a (keyword-without-prefix prefix k) (val b))
                      :else a)))
            {}
            m)))

(defn distinct-ids-in-order [coll]
  (distinct (reduce (fn [a b]
                      (conj a (:id b)))
                    []
                    coll)))

(declare merge-nested)

(defn concat-assoc [a b]
  (cond (and (vector? a) (vector? b) (not (= a b)))
        (merge-nested (vec (concat a b)))
        :else a))

(defn merge-nested
  ([existing-rec new-rec]
     (let [existing-meta (meta existing-rec)
           merged-rec (merge-with concat-assoc existing-rec new-rec)]
       (with-meta merged-rec {::table (::table existing-meta)})))
  ([coll]
     (let [order (distinct-ids-in-order coll)
           index (reduce (fn [a b]
                           (assoc a (:id b)
                                  (if-let [existing-rec (get a (:id b))]
                                    (merge-nested existing-rec b)
                                    b)))
                         {}
                         coll)]
       (vec (map #(get index %) order)))))

(declare single-record-flat->nested)

(defn- nil-assoc [type]
  (condp = type
    :many-to-one nil
    :many-to-many []
    :one-to-many []))

(defn- associated-record [type rec]
  (cond (nil? (:id rec)) (nil-assoc type)
        (= :many-to-one type) rec
        :else [rec]))

(defn add-association [model joins join-model nested-rec flat-rec]
  (let [{type :type assoc-table :table alias :alias} join-model
        assoc-rec (single-record-flat->nested model
                                              assoc-table
                                              alias
                                              joins
                                              flat-rec)]
    (assoc nested-rec alias (associated-record type assoc-rec))))

(defn single-record-flat->nested [model table alias joins flat-rec]
  (loop [nested-rec (dequalify-joined-map model table alias flat-rec)
         associations (-> joins alias :joins)]
    (if (seq associations)
      (recur (add-association model
                              joins
                              (find-join-by model
                                            table
                                            :alias
                                            (first associations))
                              nested-rec
                              flat-rec)
             (rest associations))
      (with-meta nested-rec {::table table}))))

(defn remove-nested
  "Given a map, remove any elements that are vectors, lists or maps."
  [m]
  (map-map (fn [k v]
             (cond (or (vector? v)
                       (map? v)
                       (list? v)) :remove
                       :else v))
           m))

(defn canonical-form
  "Given a map containing nested data structures, create the cononical form
   for the map that can be put into the metadata and used to determine if a
   record has been changed."
  [form]
  (map-map (fn [k v]
             (cond (or (vector? v)
                       (list? v)) (vec (map remove-nested v))
                       (map? v) (remove-nested v)
                       :else v))
           form))

(defn put-original-in-meta
  "Update the metadata to include the original canonical form of this record."
  [m]
  (vary-meta m merge {::original (canonical-form m)}))

(defn add-original-meta
  "Recursively add original metadata to all elements of this collection."
  [coll]
  (vec
   (map #(put-original-in-meta
          (with-meta
            (map-map (fn [k v]
                       (cond (or (vector? v)
                                 (list? v)) (add-original-meta v)
                                 (map? v) (put-original-in-meta v)
                                 :else v))
                     %)
            (meta %)))
        coll)))

(defn flat->nested [db table joins records-seq]
  (let [model (or (:model db) {})]
    (add-original-meta
     (merge-nested
      (vec
       (map #(single-record-flat->nested model table table joins %)
            (first records-seq)))))))

(defn execute-query-plan [db query]
  (let [table (:root-table query)]
    (->> (selects db table query)
         (execute-multiple-selects db table)
         (flat->nested db table (:joins query)))))

(defn query-merge [a b]
  (cond (keyword? a) a
        :else (concat a b)))

(defn query [db table-or-query & q]
  (if (keyword? table-or-query)
    (compile-query (:model db) table-or-query q)
    (apply deep-merge-with query-merge table-or-query q)))

(defn fetch [db & q]
  (let [first-arg (first q)
        q (rest q)]
    (cond (vector? first-arg) (sql-query db first-arg)
          (keyword? first-arg)
          (execute-query-plan db
                              (compile-query (:model db) first-arg q))
          (map? first-arg) (execute-query-plan db first-arg)
          :else (throw
                 (Exception.
                  (str "Invalid fetch syntax. "
                       "First arg is not a vector, map or keyword "
                       "(raw sql, query map or table)."))))))

(defn fetch-one
  "Same as fetch but we expect to return a single record. Throws and
   exception if more than one result is received."
  [& args]
  (let [result (apply fetch args)
        result-count (count result)]
    (if (> 2 result-count)
      (first result)
      (throw
       (Exception. (str "Expecting 1 result but received "
                        result-count
                        "."))))))

(defn count-records
  "Return the total number of records in this table for the given query."
  [db & q]
  {:pre [(or (map? (first q))
             (keyword? (first q)))]}
  (let [first-arg (first q)
        q (rest q)
        query
        (cond (keyword? first-arg)
              (compile-query (:model db) first-arg q)
              (map? first-arg) first-arg
              :else {})]
    (count-query-results db (:root-table query) query)))

;;
;; Save, Update and Delete
;;

(declare save-or-update)

(defn dismantle-record
  "Return a map containing :base-record and each of the collections that
   were in the base record. The value of base record will be the passed
   record without the collections."
  [model record]
  (let [table (-> record meta ::table)
        joins (-> model table :joins)
        association-names (filter #(not (nil? %)) (map :alias joins))]
    (merge {:base-record (apply dissoc record association-names)}
           (reduce (fn [a b]
                     (if (contains? record b)
                       (assoc a b (b record))
                       a))
                   {}
                   association-names))))

(defn dirty?
  "Check the metadata to see if the value of this record has changed. If a
   record does not have an id key then it is considered to be dirty."
  [record]
  (let [keys (keys record)
        original (select-keys (-> record meta ::original) keys)]
    (or (not (:id record))
        (not (= (canonical-form record) original)))))

(defn save-and-get
  "If the record is dirty then save it. Return the id of the saved record."
  [db record]
  (if (dirty? record)
    (let [table (::table (meta record))]
      (if (:id record)
        (do
          (sql-update db table (:id record) (m-dissoc record :id))
          record)
        (do
          (sql-insert db table record)
          (first
           (fetch db table record)))))
    record))

(defn delete-record
  "Delete a record and recursively delete any records in one-to-many
   associations."
  ([db rec]
     (if-let [table (-> rec meta ::table)]
       (delete-record db table rec)
       (throw
        (Exception. (str "No table in record metadata for " rec ".")))))
  ([db table rec]
     (with-transaction db
       (do (let [joins (filter #(= :one-to-many (:type %))
                               (-> db :model table :joins))]
             (doseq [{table :table link :link cascade-delete :cascade-delete}
                     joins]
               (if cascade-delete
                 (doseq [next (fetch db table {link (:id rec)})]
                   (delete-record db next)))))
           (sql-delete db
                       table
                       (vec (criteria->where-seq [{:id (:id rec)}])))))))

(defmulti save-associations (fn [_ join-model _ _ _] (:type join-model)))

(defmethod save-associations :many-to-many
  [db join-model record alias coll]
  (let [{:keys [link from to]} join-model
        selected-ids (set (map :id coll))
        current-items (fetch db link {from (:id record)})
        current-item-ids (set (map to current-items))
        items-to-delete (filter #(not (contains? selected-ids (to %)))
                                current-items)
        items-to-add (map #(with-meta {to (:id %)
                                       from (:id record)}
                             {::table link})
                          (filter #(not (contains? current-item-ids (:id %)))
                                  coll))]
    (do
      (doseq [next items-to-delete]
        (delete-record db link next))
      (doseq [next items-to-add]
        (save-or-update db next)))))

;; TODO - the link column is not currently required to be in the
;; associated record in order for this work. If a record is updated
;; then it will be deleted and the new version will be saved. You
;; could make this more efficient by updating records that have been
;; changed. This would still not requre you to have the link column.

(defmethod save-associations :one-to-many
  [db join-model record alias coll]
  (let [{:keys [table link]} join-model
        old-list (set (fetch db table {link (:id record)}))
        new-list (set coll)
        items-to-delete (difference old-list new-list)
        items-to-add (difference new-list old-list)]
    (do
      (doseq [next items-to-delete]
        (delete-record db next))
      (doseq [next items-to-add]
        (save-or-update db (assoc next link (:id record)))))))

(defmethod save-associations :many-to-one
  [db join-model record alias new-value]
  (let [old-value (-> record meta ::original alias)
        link (:link join-model)]
    (if (not (= old-value new-value))
      (save-or-update db
                      (cond (nil? new-value) (assoc record link nil)
                            (or (nil? old-value)
                                (not (= (:id new-value) (:id old-value))))
                            (assoc record link (:id new-value)))))))

(defmethod save-associations :default
  [_ _ _ _ _]
  nil)

(declare save-or-update*)

(defn save-or-update-record* [db record]
  (let [model (:model db)
        {record :base-record :as m} (dismantle-record model record)
        base-table (-> record meta ::table)
        record (save-and-get db record)]
    (loop [associations (dissoc m :base-record)]
      (if (seq associations)
        (let [next (first associations)
              alias (key next)
              assoc-values (val next)]
          (do
            (save-associations db
                               (find-join-by model base-table :alias alias)
                               (assoc record :id (:id record))
                               alias
                               assoc-values)
            (save-or-update* db assoc-values)
            (recur (rest associations))))
        record))))

(defn save-or-update* [db record-or-coll]
  (cond (map? record-or-coll)
        (save-or-update-record* db record-or-coll)
        (seq record-or-coll)
        (doseq [next record-or-coll]
          (save-or-update-record* db next))
        :else "Error: input is neither a map or a sequence."))

(defn- set-table [table m]
  (with-meta m {::table table}))

(defn save-or-update
  ([db record-or-coll]
     (with-transaction db
       #(save-or-update* db record-or-coll)))
  ([db table record-or-coll]
     (if (map? record-or-coll)
       (save-or-update db (set-table table record-or-coll))
       (save-or-update db (map #(set-table table %) record-or-coll)))))

(defn carte-date-time [& args]
  (from-time-zone (apply date-time args) (default-time-zone)))
