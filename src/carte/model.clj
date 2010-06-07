;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns carte.model
  (:use (clojure.contrib [map-utils :only (deep-merge-with)])
        [inflections :only (pluralize)]))

(defn set-merge [& body]
  (let [first (first body)]
    (cond (set? first) (set (apply concat body))
          (keyword? first) first
          (symbol? first) first
          :else (apply merge body))))

(defn pluralize-table [table]
  (keyword (pluralize (name table))))

(defn many-to-many-link-params [params]
  (condp = (count params)
    3 (rest params)
    2 [(pluralize-table (last params))
       (last params)]))

(defn link-col [table]
  (keyword (str (name table) "_id")))

(defn many-to-many-params [from-table to-table params]
  (condp = (count params)
    3 params
    2 (conj params
            (link-col to-table))
    1 (concat params
              [(link-col from-table)
               (link-col to-table)])
    0 [(keyword
        (str (name from-table) "_" (name to-table)))
       (link-col from-table)
       (link-col to-table)]))

(defmulti compile-association (fn [a b] (first b)))

(defmethod compile-association :default [a b]
  {})

(defmethod compile-association :many-to-many
  [table coll]
  (let [[table-params link-params] (split-with #(not (= % :=>)) coll)
        [alias many-table] (many-to-many-link-params table-params)
        [link from to] (many-to-many-params table
                                          many-table
                                          (rest link-params))]
    {many-table {:alias alias}
     table {:joins #{{:type :many-to-many
                      :table many-table
                      :alias alias
                      :link link
                      :from from
                      :to to}}}}))

(defn one-to-many-params [from-table params]
  (let [params (rest params)]
    (condp = (count params)
      3 params
      2 (cons (pluralize-table (first params))
              params)
      1 [(pluralize-table (first params))
          (first params)
          (link-col from-table)])))

(defmethod compile-association :one-to-many
  [table coll]
  (let [[alias many-table link] (one-to-many-params table coll)]
    {many-table {:alias alias}
     table {:joins #{{:type :one-to-many
                      :table many-table
                      :alias alias
                      :link link
                      :cascade-delete false}}}}))

(defn belongs-to-params [table params]
  (condp = (count params)
    4 (rest params)
    3 (conj (rest params)
            (link-col (second params)))
    2 [(last params)
       (pluralize-table table)
       (link-col (last params))]))

;; TODO - test this type of association

(defmethod compile-association :belongs-to
  [table coll]
  (let [[one-table alias link] (belongs-to-params table coll)]
    {table {:alias alias}
     one-table {:joins #{{:type :one-to-many
                          :table table
                          :alias alias
                          :link link
                          :cascade-delete true}}}}))

(defn many-to-one-params [table params]
  (condp = (count params)
    3 (rest params)
    2 [(second params)
       (link-col (second params))]))

(defmethod compile-association :many-to-one
  [table coll]
  (let [[one-table link] (many-to-one-params table coll)]
    {table {:joins #{{:type :many-to-one
                      :table one-table
                      :alias one-table
                      :link link}}}}))

(defn table* [table & config]
  (loop [result {table
                 {:attrs (first config)}}
         associations (rest config)]
    (if (seq associations)
      (recur (deep-merge-with set-merge
                              result
                              (compile-association table
                                                   (first associations)))
             (rest associations))
      result)))

(defmacro table [table & args]
  (let [table (keyword table)
        new-args (map (fn [v]
                        (vec (map keyword v)))
                      args)]
    `(table* ~table ~@new-args)))

(defn model* [& body]
  {:model (apply deep-merge-with set-merge body)})

(defmacro model [& body]
  (let [new-body (map #(apply list 'table %) body)]
    `(model* ~@new-body)))

(defn find-join-by
  "Find the map in a given model that describes the join between
   base-table and join which has value for key."
  [model base-table key value]
  (first
   (filter #(= (key %) value)
           (-> model base-table :joins))))

