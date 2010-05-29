;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns carte.core
  "map-relational-mapping for Clojure."
  (:use (clojure.contrib [sql :as sql]
	                 [java-utils :only (as-str)]
	                 [str-utils :only (re-gsub)]
                         [map-utils :only (deep-merge-with)])
        [inflections :only (pluralize)]))

(def *debug* false)

(defn set-merge [& body]
  (if (set? (first body))
    (do
      (set (apply concat body)))
    (apply merge body)))

(defn pluralize-relation [relation]
  (keyword (pluralize (name relation))))

(defn many-to-many-link-table-spec [relation-params]
  (condp = (count relation-params)
    3 (rest relation-params)
    2 [(pluralize-relation (last relation-params))
       (last relation-params)]))

(defn link-col [relation]
  (keyword (str (name relation) "_id")))

(defn many-to-many-link-spec [from-relation to-relation link-params]
  (condp = (count link-params)
    3 link-params
    2 (conj link-params
            (link-col to-relation))
    1 (concat link-params
              [(link-col from-relation)
               (link-col to-relation)])
    0 [(keyword
        (str (name from-relation) "_" (name to-relation)))
       (link-col from-relation)
       (link-col to-relation)]))

(defmulti compile-association (fn [a b] (first b)))

(defmethod compile-association :default [a b]
  {})

(defmethod compile-association :many-to-many
  [relation coll]
  (let [[relation-params link-params] (split-with #(not (= % :=>)) coll)
        [alias many-relation] (many-to-many-link-table-spec relation-params)
        [link from to] (many-to-many-link-spec relation
                                               many-relation
                                               (rest link-params))]
    {many-relation {:alias alias}
     relation {:joins #{{:type :many-to-many
                         :relation many-relation
                         :alias alias
                         :link link
                         :from from
                         :to to}}}}))

(defn one-to-one-link-table-spec [from-relation link-params]
  (condp = (count link-params)
    4 (rest link-params)
    3 (cons (pluralize-relation (second link-params))
            (rest link-params))
    2 [(pluralize-relation (last link-params))
       (last link-params)
       (link-col from-relation)]))

(defmethod compile-association :one-to-many
  [relation coll]
  (let [[alias many-relation link] (one-to-one-link-table-spec relation
                                                                 coll)]
    {many-relation {:alias alias}
     relation {:joins #{{:type :one-to-many
                         :relation many-relation
                         :alias alias
                         :link link}}}}))

(defn relation* [relation & config]
  (loop [result {relation
                 {:attrs (first config)}}
         associations (rest config)]
    (if (seq associations)
      (recur (deep-merge-with set-merge
                              result
                              (compile-association relation
                                                   (first associations)))
             (rest associations))
      result)))

(defmacro relation [relation & args]
  (let [relation (keyword relation)
        new-args (map (fn [v]
                        (vec (map keyword v)))
                      args)]
    `(relation* ~relation ~@new-args)))

(defn model* [& body]
  (apply deep-merge-with set-merge body))

(defmacro model [& body]
  (let [new-body (map #(apply list 'relation %) body)]
    `(model* ~@new-body)))

;; End define relationships

(defn map-values [f m]
  (let [keys (keys m)
	values (vals m)]
    (zipmap keys (map f values))))

(defn- with-transaction
  "Execute a function within a transaction"
  [db f]
  (sql/with-connection (:connection db)
    (sql/transaction (f)))
  nil)

(defn split-criteria
  "Create a sequence where the first element is the vector of keys and the
   remaining elements are the values"
  [criteria]
  (cons (vec (keys criteria)) (vals criteria)))

(defn create-comparison
  ([key value]
     (create-comparison nil key value))
  ([prefix key value]
     (str (if prefix
            (str (name prefix) "."))
          (name key)
          (if (nil? value)
            " IS NULL"
            (if (. (as-str value) endsWith "*")
              " like ?"
              " = ?")))))

(defn create-where-str
  "Create a where string out of the keys from a criteria map"
  ([keys values]
     (create-where-str nil keys values))
  ([prefix keys values]
     (apply str (interpose
                 " AND "
                 (map (partial create-comparison prefix) keys values)))))

(defn replace-wildcard [s]
  (if (. (as-str s) endsWith "*")
    (re-gsub #"[*]" "%" s)
    s))

(defn create-where-vec
  "Create a vector where the first element is the where query and the
   remaining elements are the parameters"
  ([criteria]
     (create-where-vec nil criteria))
  ([relation criteria]
     (loop [query-strings []
            values []
            criteria criteria]
       (if-let [next (first criteria)]
         (if-let [[qs & v] (split-criteria next)]
           (recur (conj query-strings
                        (create-where-str relation qs v))
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

(defn create-relation-qualified-names [relation attrs]
  (map #(let [r (name relation)
              a (name %)]
          (str r "." a " as " r "_" a))
       attrs))

(defn find-join-model
  "Find the map in a given model that describes the join between
   base-relation and alias."
  [model base-relation alias]
  (first
   (filter #(= (:alias %) alias)
           (-> model base-relation :joins))))

;; TODO You are forcing the :id to be one of the attributes because
;; you need it later when organizing records. You also need the id
;; when you go to save or delete a record. One way to get around this
;; is to always put the id in the metadata and use that for sorting
;; and saving records. 

(defn get-attrs [model relation requested-attrs]
  {:pre [(not (nil? relation))]}
  (if-let [attrs (relation requested-attrs)]
    (if (contains? (set attrs) :id)
      attrs
      (conj attrs :id))
    (-> model relation :attrs)))

(defn create-attr-list
  ([model relation]
     (create-attr-list model relation nil nil))
  ([model relation req-attrs]
     (create-attr-list model relation req-attrs nil))
  ([model relation req-attrs joins]
     {:pre [(not (nil? relation))]}
     (if-let [attrs (get-attrs model relation req-attrs)]
       (loop [result (create-relation-qualified-names relation
                                                      attrs)
              joins (relation joins)]
         (if (seq joins)
           (let [{type :type many-side :relation}
                 (find-join-model model relation (first joins))
                 attrs (get-attrs model many-side req-attrs)]
             (recur (concat result (create-relation-qualified-names many-side
                                                                    attrs))
                    (rest joins)))
           (apply str " " (interpose ", " result))))
       " *")))

(defn many-to-many? [join]
  (= (:type join) :many-to-many))

(defn one-to-many? [join]
  (= (:type join) :one-to-many))

;; TODO use map and destructuring
(defn create-many-to-many-join [base-rel result join]
  (let [{rel :relation link :link from :from to :to} join]
    (let [rel (name rel)
          link (name link)
          from (name from)
          to (name to)]
      (str result
           " LEFT JOIN " link " ON " (name base-rel) ".id = " link "." from
           " LEFT JOIN " rel " ON " link "." to " = " rel ".id"))))

(defn create-one-to-many-join [base-rel result join]
  (let [{rel :relation link :link} join]
    (let [rel (name rel)
          link (name link)]
      (str result
           " LEFT JOIN " rel " ON " (name base-rel) ".id = " rel "." link))))

(defn create-joins [model relation requested-joins]
  (loop [result ""
         joins (relation requested-joins)]
    (if (seq joins)
      (let [join (find-join-model model relation (first joins))]
        (recur
         (cond (many-to-many? join)
               (create-many-to-many-join relation result join)
               (one-to-many? join)
               (create-one-to-many-join relation result join)
               :else "")
         (rest joins))) 
      result)))

(defn parse-query-part [relation q]
  (loop [q q
         result {}]
    (if (seq q)
      (let [next (first q)]
        (recur (rest q)
               (cond (vector? next) (merge result {:attrs {relation next}})
                     (map? next) (merge-with concat result {:criteria [next]})
                     :else result)))
      result)))

(defn parse-join-part [relation q]
  (if (seq q)
    {:joins {relation (rest q)}}
    {}))

(defn parse-query
  "Create a map with keys :attrs :criteria and :joins"
  [relation q]
  (let [[query-part join-part] (split-with #(not (= % :with)) q)]
    (merge (parse-query-part relation query-part)
           (parse-join-part relation join-part))))

(defn subprotocol-dispatch [conn & p]
  (:subprotocol conn))

(defmulti create-selects subprotocol-dispatch)

(defn create-selects-mysql
  "Create the select vector that can be passed to with-query-results"
  [conn model relation q]
  [(let [{:keys [attrs criteria joins]} (parse-query relation q)
         select-part (str "SELECT" (create-attr-list model relation attrs joins)
                          " FROM " (as-str relation)
                          (create-joins model relation joins))
         where-part (create-where-vec relation criteria)
         order-by-part (create-order-by {})]
     (if where-part
       (vec (cons (str select-part " WHERE " (first where-part) order-by-part)
                  (rest where-part)))
       [(str select-part order-by-part)]))])

(defmethod create-selects "mysql"
  [conn relation criteria params]
  (create-selects-mysql conn relation criteria params))

(defmethod create-selects :default
  [conn relation criteria params]
  (create-selects-mysql conn relation criteria params))

(defn db-drop-table [db name]
  (sql/with-connection (:connection db)
      (sql/drop-table name)))

(defn db-do-commands [db commands]
  (sql/with-connection (:connection db)
    (sql/do-commands commands)))

(defn db-insert
  "Insert records, maps from keys specifying columns to values"
  [db name rec]
  (when *debug*
    (println (str "inserting record into " name ": " rec)))
  (with-transaction db
    #(sql/insert-records name rec)))

(defn db-update
  "Update a record"
  [db name id rec]
  (when *debug*
    (println (str "updating record in " name ": " rec)))
  (with-transaction db 
    #(sql/update-values name ["id=?" id] rec)))

(defn- m-dissoc [m & keys]
  (apply dissoc (into {} m) keys))

(defn keyword-without-prefix [prefix s]
  (keyword
   (.substring s (+ 1 (count prefix)))))

(defn dequalify-joined-map
  "Create a map that has dequalified key names for this relation. The input
   map will have the relation named appended to the front of each key."
  [model relation m]
  (let [prefix (name relation)
        other-relations (->> (map name (keys model))
                             (filter #(> (count %) (count prefix))))]
    (reduce (fn [a b]
              (let [k (name (key b))]
                (cond (not (relation model))
                      (assoc a (if (.startsWith k prefix)
                                 (keyword-without-prefix prefix k)
                                 (key b))
                             (val b))
                      (and (.startsWith k prefix)
                           (not (some true? (map #(.startsWith k %)
                                                 other-relations))))
                      (assoc a (keyword-without-prefix prefix k) (val b))
                      :else a)))
            {}
            m)))

(defn merge-many-collection [coll new-coll]
  (vec (concat coll new-coll)))

(defn merge-many
  "Create a map from the many side of the association and add it to the
   collection contained in the one side."
  [model relation sub-relation alias result-map next-result]
  (let [sub (dequalify-joined-map model sub-relation next-result)
        id ((keyword (str (name relation) "_id")) next-result)
        host (get result-map id)]
    (assoc result-map id
           (if (:id sub)
             (merge-with merge-many-collection host
                         {alias [(with-meta sub {::type sub-relation
                                                 ::original sub})]})
             (assoc host alias [])))))

(defn order-and-result-map
  "In one pass through the results, create a vector containing the order of
   the results by id and a map with the unique results for this relation."
  [model relation results]
  (reduce
   (fn [a b]
     (let [m (dequalify-joined-map model relation b)]
       [(conj (first a) (:id m))
        (assoc (last a) (:id m) m)]))
   [[] {}]
   results))

(defmulti transform-query-plan-results subprotocol-dispatch)

(defmethod transform-query-plan-results "mysql"
  [conn model relation q results]
  (map #(with-meta % {::type relation
                      ::original %})
       (if (and model (relation model))
         (let [results (first results)
               [order rec-map] (order-and-result-map model relation results)
               {:keys [joins]} (parse-query relation q)]
           (loop [joins (relation joins)
                  rec-map rec-map]
             (if (seq joins)
               (let [{sub-relation :relation alias :alias}
                     (find-join-model model relation (first joins))]
                 (recur
                  (rest joins)
                  (reduce (partial merge-many model relation sub-relation alias)
                          rec-map
                          results)))
               (vec (map #(get rec-map %) (distinct order))))))
         (map (partial dequalify-joined-map model relation)
              (first results)))))

(defn raw-sql
  "Run a query using raw sql of the form
   [\"SELECT * FROM table WHERE name = ? and age = ?\" \"joe\" 42]."
  [db sql]
  (sql/with-connection (:connection db)
    (sql/with-query-results res
      sql
      (into [] res))))

(defn execute-selects [conn model relation selects]
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

(defn execute-query-plan [conn model relation q]
  (->> (create-selects conn model relation q)
       (execute-selects conn model relation)
       (transform-query-plan-results conn model relation q)))

(defn query [db & q]
  (let [conn (:connection db)
        first-arg (first q)
        q (rest q)]
    (cond (vector? first-arg) (raw-sql db first-arg)
          (keyword? first-arg) (execute-query-plan conn nil first-arg q)
          (map? first-arg)
          (execute-query-plan conn first-arg (first q) (rest q))
          :else (throw
                 (Exception.
                  (str "Invalid query syntax. "
                       "First arg is not raw sql, a model or a relation."))))))

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

(defn delete-record
  ([db rec]
     (if-let [table (-> rec meta ::type)]
       (delete-record db table rec)))
  ([db table rec]
     (when *debug*
       (println (str "deleting record from " table ": " rec)))
     (with-transaction db 
       #(sql/delete-rows table
                         (vec (create-where-vec [{:id (:id rec)}]))))))

(declare save-or-update)

(defn dismantle-record
  "Return a map containing :base-record and each of the collections that
   were in the base record. The value of base record will be the passed
   record without the collections."
  [model record]
  (let [relation (-> record meta ::type)
        joins (-> model relation :joins)
        coll-names (map :alias joins)]
    (merge {:base-record (apply dissoc record coll-names)}
           (reduce (fn [a b]
                     (assoc a b (b record)))
                   {}
                   coll-names))))

(defn dirty?
  "Check the metadata to see if the value of this record has changed."
  [record]
  (not (= record (-> record meta ::original))))

(defn save-and-get-id
  "If the record is dirty then save it. Return the id of the saved record."
  [db record]
  (if (dirty? record)
    (let [relation (::type (meta record))]
      (:id
       (if (:id record)
         (do
           (db-update db relation (:id record) (m-dissoc record :id))
           record)
         (do
           (db-insert db relation record)
           (first
            (query db relation record))))))
    (:id record)))

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
                             {::type link})
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
        base-relation (-> record meta ::type)
        base-id (save-and-get-id db record)]
    (loop [collections (dissoc m :base-record)]
      (if (seq collections)
        (let [next (first collections)
              alias (key next)
              coll (val next)]
          (do
            (save-associations db
                               (find-join-model model base-relation alias)
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

(defn- set-relation [relation m]
  (with-meta m {::type relation}))

(defn save-or-update
  ([db record-or-coll]
     (save-or-update db nil record-or-coll))
  ([db model record-or-coll]
     (if (keyword? model)
       (save-or-update db nil model record-or-coll)
       (sql/with-connection (:connection db)
         (sql/transaction
          (save-or-update* db model record-or-coll)))))
  ([db model relation record-or-coll]
     (if (map? record-or-coll)
       (save-or-update db model (set-relation relation record-or-coll))
       (save-or-update db model
                       (map #(set-relation relation %) record-or-coll)))))

;; Functions for manipulating nested data structures

(defn conj-in [m ks v]
  (let [size (count (get-in m ks))]
    (assoc-in m (conj ks size) v)))

(defn remove-in [m ks v]
  (let [coll (get-in m ks)]
    (assoc-in m
              ks
              (filter #(not (= v (select-keys % (keys v)))) coll))))

(comment

  (def $ (partial query db))

  ;; various kinds of queries
  ($ :person)
  ($ :person {:id 8})
  ($ :person {:name "brent*"})

  ;; Arbitrary SQL
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
  ;; and will get all people, each one have a :cars key that contains
  ;; a list of associated cars.
  
  ($ :person {:id 8})
  
  )
