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

(defn merge-join-sets [existing-set new-set]
  (let [new-join (first new-set)
        type (:type new-join)
        alias (:alias new-join)]
    (set (concat (filter #(not (and (= (:type %) type)
                                    (= (:alias %) alias))) existing-set)
                 new-set))))

(defn association-merge [& body]
  (let [first (first body)
        last (last body)]
    (cond (set? first) (apply merge-join-sets body)
          (keyword? first) last
          (symbol? first) last
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

(defn many-to-many-association [table alias many-table link from to]
  {table {:joins #{{:type :many-to-many
                    :table many-table
                    :alias alias
                    :link link
                    :from from
                    :to to}}}})

(defmethod compile-association :many-to-many
  [table coll]
  (let [[table-params link-params] (split-with #(not (= % :=>)) coll)
        [alias many-table] (many-to-many-link-params table-params)
        [link from to] (many-to-many-params table
                                            many-table
                                            (rest link-params))]
    (deep-merge-with association-merge
                     (many-to-many-association table
                                               alias
                                               many-table
                                               link
                                               from
                                               to)
                     (many-to-many-association many-table
                                               (pluralize-table table)
                                               table
                                               link
                                               to
                                               from))))

(defn one-to-many-params [from-table params]
  (let [params (rest params)]
    (condp = (count params)
      3 params
      2 (cons (pluralize-table (first params))
              params)
      1 [(pluralize-table (first params))
         (first params)
         (link-col from-table)])))

(defn one-to-many-association [table many-table alias link]
  {table {:joins #{{:type :one-to-many
                    :table many-table
                    :alias alias
                    :link link
                    :cascade-delete false}}}})

(defn many-to-one-association [table alias one-table link]
  {table {:joins #{{:type :many-to-one
                    :table one-table
                    :alias alias
                    :link link}}}})

(defmethod compile-association :one-to-many
  [table coll]
  (let [[alias many-table link] (one-to-many-params table coll)]
    (deep-merge-with association-merge
                     (one-to-many-association table many-table alias link)
                     (many-to-one-association many-table table table link))))

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
    (deep-merge-with association-merge
                     {table {:alias alias}
                      one-table {:joins #{{:type :one-to-many
                                           :table table
                                           :alias alias
                                           :link link
                                           :cascade-delete true}}}}
                     (many-to-one-association table one-table one-table link))))

(defn many-to-one-params [table params]
  (let [params (rest params)]
    (condp = (count params)
        3 params
        2 (cons (first params)
                params)
        1 (let [t (first params)]
            [t t (link-col t)]))))

(defmethod compile-association :many-to-one
  [table coll]
  (let [[alias one-table link] (many-to-one-params table coll)]
    (deep-merge-with association-merge
                     (many-to-one-association table alias one-table link)
                     (one-to-many-association one-table
                                              table
                                              (pluralize-table table)
                                              link))))

(defn table* [table & config]
  (loop [result {table
                 {:attrs (first config)}}
         associations (rest config)]
    (if (seq associations)
      (recur (deep-merge-with association-merge
                              result
                              (compile-association table
                                                   (first associations)))
             (rest associations))
      result)))

(defmacro table [table & args]
  (let [table (keyword table)
        f (first args)
        [attrs associations] (if (vector? f) [f (rest args)] [[] args])
        associations (map (fn [v]
                            (vec (map keyword v)))
                          associations)]
    `(table* ~table ~attrs ~@associations)))

(defn model* [& body]
  {:model (apply deep-merge-with association-merge body)})

(defmacro model [& body]
  (let [new-body (map #(apply list 'carte.model/table %) body)]
    `(model* ~@new-body)))

(defn find-join-by
  "Find the map in a given model that describes the join between
   'base-table' and 'join' which has 'value' for key."
  [model base-table key value]
  (first
   (filter #(= (key %) value)
           (-> model base-table :joins))))

