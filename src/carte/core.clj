;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns carte.core
  (:use (carte sql model)
        (clojure.contrib [map-utils :only (deep-merge-with)])))

(defn parse-query-part [table q]
  (loop [q q
         result {}]
    (if (seq q)
      (let [next (first q)]
        (recur (rest q)
               (cond (vector? next) (merge result {:attrs {table next}})
                     (map? next)
                     (deep-merge-with concat
                                      result
                                      {:criteria {table [next]}})
                     :else result)))
      result)))

(declare parse-query)

(defn parse-join-part [model table q]
  (loop [result {}
         joins (rest q)]
    (if (seq joins)
      (let [next (first joins)]
        (recur (if (keyword? next)
                 (deep-merge-with concat
                                  result
                                  {:joins {table [next]}})
                 (let [join-rel (first next)
                       join-model (find-join-by model
                                                table
                                                :table
                                                join-rel)]
                   (deep-merge-with concat
                                    result
                                    {:joins {table [(:alias join-model)]}}
                                    (parse-query model
                                                 join-rel
                                                 (rest next)))))
               (rest joins)))
      result)))

(defn parse-query
  "Create a map with keys :attrs :criteria and :joins"
  [model table q]
  (let [[query-part join-part] (split-with #(not (= % :with)) q)]
    (merge (parse-query-part table query-part)
           (parse-join-part model table join-part))))

(defn- m-dissoc [m & keys]
  (apply dissoc (into {} m) keys))

(defn keyword-without-prefix [prefix s]
  (keyword
   (.substring s (+ 1 (count prefix)))))

(defn dequalify-joined-map
  "Create a map that has dequalified key names for this table. The input
   map will have the table named appended to the front of each key."
  [model table m]
  (let [prefix (name table)
        other-tables (->> (map name (keys model))
                             (filter #(> (count %) (count prefix))))]
    (reduce (fn [a b]
              (let [k (name (key b))]
                (cond (not (table model))
                      (assoc a (if (.startsWith k prefix)
                                 (keyword-without-prefix prefix k)
                                 (key b))
                             (val b))
                      (and (.startsWith k prefix)
                           (not (some true? (map #(.startsWith k %)
                                                 other-tables))))
                      (assoc a (keyword-without-prefix prefix k) (val b))
                      :else a)))
            {}
            m)))

(defn merge-many-collection [coll new-coll]
  (vec (concat coll new-coll)))

(defn merge-many
  "Create a map from the many side of the association and add it to the
   collection contained in the one side."
  [model table sub-table alias result-map next-result]
  (let [sub (dequalify-joined-map model sub-table next-result)
        id ((keyword (str (name table) "_id")) next-result)
        host (get result-map id)]
    (assoc result-map id
           (if (:id sub)
             (merge-with merge-many-collection host
                         {alias [(with-meta sub {::table sub-table
                                                 ::original sub})]})
             (assoc host alias [])))))

(defn order-and-result-map
  "In one pass through the results, create a vector containing the order of
   the results by id and a map with the unique results for this table."
  [model table results]
  (reduce
   (fn [a b]
     (let [m (dequalify-joined-map model table b)]
       [(conj (first a) (:id m))
        (assoc (last a) (:id m) m)]))
   [[] {}]
   results))

(defn transform-query-plan-results
  [model table q results]
  (map #(with-meta % {::table table
                      ::original %})
       (if (and model (table model))
         (let [results (first results)
               [order rec-map] (order-and-result-map model table results)
               {:keys [joins]} (parse-query model table q)]
           (loop [joins (table joins)
                  rec-map rec-map]
             (if (seq joins)
               (let [{sub-table :table alias :alias}
                     (find-join-by model table :alias (first joins))]
                 (recur
                  (rest joins)
                  (reduce (partial merge-many model table sub-table alias)
                          rec-map
                          results)))
               (vec (map #(get rec-map %) (distinct order))))))
         (map (partial dequalify-joined-map model table)
              (first results)))))

(defn execute-query-plan [conn model table q]
  (->> (create-selects model table (parse-query model table q))
       (execute-multiple-selects conn model table)
       (transform-query-plan-results model table q)))

(defn query [db & q]
  (let [conn (:connection db)
        first-arg (first q)
        q (rest q)]
    (cond (vector? first-arg) (sql-query db first-arg)
          (keyword? first-arg) (execute-query-plan conn nil first-arg q)
          (map? first-arg)
          (execute-query-plan conn first-arg (first q) (rest q))
          :else (throw
                 (Exception.
                  (str "Invalid query syntax. "
                       "First arg is not raw sql, a model or a table."))))))

(defn query-1
  "Same as query but we expect to return a single record. Throws and
   exception if more than one result is received."
  [& args]
  (let [result (apply query args)
        result-count (count result)]
    (if (> 2 result-count)
      (first result)
      (throw
       (Exception. (str "Expecting 1 result but received "
                        result-count
                        "."))))))

(declare save-or-update)

(defn dismantle-record
  "Return a map containing :base-record and each of the collections that
   were in the base record. The value of base record will be the passed
   record without the collections."
  [model record]
  (let [table (-> record meta ::table)
        joins (-> model table :joins)
        coll-names (map :alias joins)]
    (merge {:base-record (apply dissoc record coll-names)}
           (reduce (fn [a b]
                     (assoc a b (b record)))
                   {}
                   coll-names))))

;; TODO - You should also consider a record to be dirty if it does not
;; have an id.

(defn dirty?
  "Check the metadata to see if the value of this record has changed."
  [record]
  (not (= record (-> record meta ::original))))

(defn save-and-get-id
  "If the record is dirty then save it. Return the id of the saved record."
  [db record]
  (if (dirty? record)
    (let [table (::table (meta record))]
      (:id
       (if (:id record)
         (do
           (sql-update db table (:id record) (m-dissoc record :id))
           record)
         (do
           (sql-insert db table record)
           (first
            (query db table record))))))
    (:id record)))

(defn delete-record
  ([db rec]
     (if-let [table (-> rec meta ::table)]
       (delete-record db table rec)))
  ([db table rec]
     (when *debug*
       (println (str "deleting record from " table ": " rec)))
     (sql-delete db
                 table
                 (vec (criteria->where-spec [{:id (:id rec)}])))))

(defmulti save-associations (fn [_ join-model _ _ _] (:type join-model)))

(defmethod save-associations :many-to-many
  [db join-model record alias coll]
  (let [{:keys [link from to]} join-model
        selected-ids (set (map :id coll))
        current-items (query db link {from (:id record)})
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

(defmethod save-associations :default
  [_ _ _ _ _]
  nil)

(defn save-or-update-record* [db model record]
  (let [{record :base-record :as m} (dismantle-record model record)
        base-table (-> record meta ::table)
        base-id (save-and-get-id db record)]
    (loop [collections (dissoc m :base-record)]
      (if (seq collections)
        (let [next (first collections)
              alias (key next)
              coll (val next)]
          (do
            (save-associations db
                               (find-join-by model base-table :alias alias)
                               (assoc record :id base-id)
                               alias
                               coll)
            (map #(save-or-update-record* db model %) coll)
            (recur (rest collections))))
        base-id))))

(defn save-or-update* [db model record-or-coll]
  (cond (map? record-or-coll)
        (save-or-update-record* db model record-or-coll)
        (seq record-or-coll)
        (doseq [next record-or-coll]
          (save-or-update-record* db model next))
        :else "Error: input is neither a map or a sequence."))

(defn- set-table [table m]
  (with-meta m {::table table}))

(defn save-or-update
  ([db record-or-coll]
     (save-or-update db nil record-or-coll))
  ([db model record-or-coll]
     (if (keyword? model)
       (save-or-update db nil model record-or-coll)
       (with-transaction db
         #(save-or-update* db model record-or-coll))))
  ([db model table record-or-coll]
     (if (map? record-or-coll)
       (save-or-update db model (set-table table record-or-coll))
       (save-or-update db model
                       (map #(set-table table %) record-or-coll)))))

;; Functions for manipulating nested data structures

(defn conj-in [m ks v]
  (let [size (count (get-in m ks))]
    (assoc-in m (conj ks size) v)))

(defn remove-in [m ks v]
  (let [coll (get-in m ks)]
    (assoc-in m
              ks
              (filter #(not (= v (select-keys % (keys v)))) coll))))

(defn find-in [ks v]
  (loop [ks ks
         result v]
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

(defn find-first-in [ks v]
  (first (find-in ks v)))

(comment

  (def $ (partial query db))

  ;; example queries
  
  ($ :person)
  ($ :person {:id 8})
  ($ :person {:name "brent*"})

  ;; arbitrary SQL
  ($ ["SELECT * FROM person"])
  ($ ["SELECT * FROM person WHERE id = ?" 8])

  ;; using data models
  (def data-model 
       (model (car [:id :make])
              (person [:id :first_name :last_name]
                      (many-to-many :car))))

  (def $ (partial query db data-model))

  ($ :person)
  ($ :person :with :cars)
  
  ;; The above query will perform a join through the :person_car table
  ;; and will get all people, each one having a :cars key that contains
  ;; a list of associated cars.
  
  ($ :person {:id 8})

  ;; multiple associations
  ($ :person :with :cars :homes)
  
  ;; arbitrary queries
  ($ :person {:where ["id > ? AND name = ?" 5 "Brenton"]})

  ;; this should integrate with what you already have
  ($ :person {:where ["id > ?" 5] :name "Brenton"})
  ($ :person {:where ["id > ?" 5] :name "Brenton"} {:name "Jim"})
)
