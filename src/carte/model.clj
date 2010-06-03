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
  (if (set? (first body))
    (do
      (set (apply concat body)))
    (apply merge body)))

(defn pluralize-table [table]
  (keyword (pluralize (name table))))

(defn many-to-many-link-table-spec [table-params]
  (condp = (count table-params)
    3 (rest table-params)
    2 [(pluralize-table (last table-params))
       (last table-params)]))

(defn link-col [table]
  (keyword (str (name table) "_id")))

(defn many-to-many-link-spec [from-table to-table link-params]
  (condp = (count link-params)
    3 link-params
    2 (conj link-params
            (link-col to-table))
    1 (concat link-params
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
        [alias many-table] (many-to-many-link-table-spec table-params)
        [link from to] (many-to-many-link-spec table
                                               many-table
                                               (rest link-params))]
    {many-table {:alias alias}
     table {:joins #{{:type :many-to-many
                      :table many-table
                      :alias alias
                      :link link
                      :from from
                      :to to}}}}))

(defn one-to-one-link-table-spec [from-table link-params]
  (condp = (count link-params)
    4 (rest link-params)
    3 (cons (pluralize-table (second link-params))
            (rest link-params))
    2 [(pluralize-table (last link-params))
       (last link-params)
       (link-col from-table)]))

(defmethod compile-association :one-to-many
  [table coll]
  (let [[alias many-table link] (one-to-one-link-table-spec table
                                                                 coll)]
    {many-table {:alias alias}
     table {:joins #{{:type :one-to-many
                      :table many-table
                      :alias alias
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
  (apply deep-merge-with set-merge body))

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

(defn many-to-many? [join]
  (= (:type join) :many-to-many))

(defn one-to-many? [join]
  (= (:type join) :one-to-many))

