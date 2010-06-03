;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns carte.test-sql
  (:use (clojure test)
        (carte core sql fixtures)))

(deftest test-key-value->where-seq
  (are [a b _ result] (= (key-value->where-seq a b) result) 
       :id 1        :=> ["id = ?" 1]
       :name "John" :=> ["name = ?" "John"]
       :name nil    :=> ["name IS NULL" nil]
       :name "Joh*" :=> ["name like ?" "Joh*"]
       :where ["name = ?" "John"] :=> ["name = ?" "John"])
  (are [a b _ result] (= (key-value->where-seq :person a b) result) 
       :id 1        :=> ["person.id = ?" 1]
       :name "John" :=> ["person.name = ?" "John"]
       :name nil    :=> ["person.name IS NULL" nil]
       :name "Joh*" :=> ["person.name like ?" "Joh*"]
       :where ["name = ?" "John"] :=> ["person.name = ?" "John"]
       :where ["name = ? AND age = ?" "John" 42] :=>
       ["person.name = ? AND person.age = ?" "John" 42]))

(deftest test-map->where-seq
  (are [a _ result] (= (map->where-seq a) result)
       {:id 1}              :=> ["id = ?" 1]
       {:id 1 :name "John"} :=> ["id = ? AND name = ?" 1 "John"]
       {:id 1 :name "John" :desc "Something*" :cost nil} :=>
       ["id = ? AND name = ? AND desc like ? AND cost IS NULL"
        1 "John" "Something*" nil]))

(deftest test-criteria->where-seq
  (are [x _ y] (= (criteria->where-seq x) y)
       [] :=> nil
       [{:name "a"}] :=> ["name = ?" "a"]
       [{:name "a" :age 7}] :=> ["name = ? AND age = ?" "a" 7]
       [{:name "a"} {:name "b"}] :=> ["(name = ?) OR (name = ?)" "a" "b"]
       [{:name "a" :age 7} {:name "b"}] :=>
       ["(name = ? AND age = ?) OR (name = ?)" "a" 7 "b"]
       [{:where ["name = ?" "a"]}] :=> ["name = ?" "a"]
       [{:where ["name = ? AND id < ?" "a" 6]}] :=>
       ["name = ? AND id < ?" "a" 6]
       [{:where ["name = ? AND id < ?" "a" 6] :age 7}] :=>
       ["name = ? AND id < ? AND age = ?" "a" 6 7]
       [{:where ["name = ? AND id < ?" "a" 6] :age 7} {:age 10}] :=>
       ["(name = ? AND id < ? AND age = ?) OR (age = ?)" "a" 6 7 10]))

(deftest test-query->where-seq
  (are [x _ y] (= (query->where-seq nil x) y)
       {:page [{:name "a"}]} :=> ["name = ?" "a"])
  (are [x _ y] (= (query->where-seq :page x) y)
       {:page [{:name "a"}]} :=> ["page.name = ?" "a"]
       {:page [{:name "a"}] :version [{:id 7}]} :=>
       ["page.name = ? AND version.id = ?" "a" 7]
       {:version [{:id 7}] :page [{:name "a"}]} :=>
       ["version.id = ? AND page.name = ?" 7 "a"]))

(deftest test-columns-sql
  (is (= (columns-sql nil :page) ["*"]))
  (is (= (columns-sql {} :page) ["*"]))
  (are [model q j _ attrs] (= (columns-sql model :page q j)
                              (attr-list attrs)))

  fixture-model-one-and-many-to-many
  nil
  {:page [:categories :versions]} :=> [page-table
                                       category-table
                                       version-table]
  
  
  fixture-model-many-to-many
  {:page [:name]}
  {:page [:categories]}           :=> [[:page [:name :id]]
                                       category-table]
  
  fixture-model-many-to-many
  {:page [:name] :category [:name]}
  {:page [:categories]}           :=> [[:page [:name :id]]
                                       [:category [:name :id]]])

(deftest test-selects
  (are [model q _ result] (= (selects model
                                      :page
                                      (parse-query model :page q))
                             result)
       
       {} []                          :=> [[(select-from "*")]]
       nil []                         :=> [[(select-from "*")]]
       nil [{:id 1}]                  :=> [[(str (select-from "*")
                                                 " WHERE page.id = ?") 1]]
       nil [{:name "brent*"}]         :=> [[(str (select-from "*")
                                                 " WHERE page.name like ?")
                                            "brent%"]]
       fixture-model-many-to-many
       []                             :=> [[(select-from
                                             (attr-list [page-table]))]]
       fixture-model-many-to-many
       [:with :categories]            :=> [[many-to-many-join-query]]
       fixture-model-many-to-many
       [{:id 1}]                      :=> [[(str (select-from
                                                  (attr-list [page-table]))
                                                 " WHERE page.id = ?") 1]]
       fixture-model-many-to-many
       [{:id 1} :with :categories]    :=> [[(str many-to-many-join-query
                                                 " WHERE page.id = ?") 1]]
       fixture-model-many-to-many
       [{:id 1 :name "brenton"}
        :with :categories]            :=> [[(str many-to-many-join-query
                                                 " WHERE page.id = ?"
                                                 " AND page.name = ?")
                                            1 "brenton"]]
        fixture-model-one-to-many
        [:with :versions]             :=> [[one-to-many-join-query]]
        fixture-model-one-and-many-to-many
        [:with :categories :versions] :=> [[one-and-many-to-many-join-query]]))

(deftest test-selects-with-nested-withs
  (are [table q result] (= (selects data-model
                                    table
                                    (parse-query data-model table q))
                           [[result]])
       
       :artist [:with [:album :with :tracks]]
       (str "SELECT "
            (attr-list [[:artist [:id :name]]
                        [:album [:id :title]]
                        [:track [:id :name]]])
            " FROM artist"
            " LEFT JOIN album_artist ON artist.id = album_artist.artist_id"
            " LEFT JOIN album ON album_artist.album_id = album.id"
            " LEFT JOIN track ON album.id = track.album_id")))

