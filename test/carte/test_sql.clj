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

(deftest test-query->order-by
  (are [query _ expected] (= (query->order-by nil query) expected)

       [:artist [:name :asc]] :=> " ORDER BY name"
       [:artist [:name :desc]] :=> " ORDER BY name DESC"
       [:artist [:name :asc :age :desc]] :=> " ORDER BY name, age DESC")
  
  (are [query _ expected] (= (query->order-by :artist query) expected)

       [:artist [:name :asc]] :=> " ORDER BY artist.name"
       [:artist [:name :desc]] :=> " ORDER BY artist.name DESC"
       [:artist [:name :asc :age :desc]] :=>
       " ORDER BY artist.name, artist.age DESC"
       [:artist [:name :asc] :album [:title :asc]] :=>
       " ORDER BY artist.name, album.title"
       [:artist [:name :asc :id :desc] :album [:title :desc]] :=>
       " ORDER BY artist.name, artist.id DESC, album.title DESC"))

(deftest test-columns-sql
  (is (= (columns-sql nil :page) ["*"]))
  (is (= (columns-sql {} :page) ["*"]))
  (are [model table query joins _ attrs]
       (= (interpose-str ", "
                         (columns-sql model table query joins))
          (attr-list attrs))

       fixture-model-one-and-many-to-many
       :page
       nil
       [:categories :versions] :=> [page-table
                                    category-table
                                    version-table]
    
       fixture-model-many-to-many
       :page
       {:page [:name]}
       [:categories]           :=> [[:page [:id :name]]
                                    category-table]

       fixture-model-many-to-many
       :page
       {:page [:name] :category [:name]}
       [:categories]           :=> [[:page [:id :name]]
                                    [:category [:id :name]]]

       sample-data-model
       :album
       {:album [:title]}
       [:genre]                :=> [[:album [:id :title]]
                                    [:genre [:id :name]]]))

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
       [{:id 1 :name "brenton"} :with :categories] :=>
       [[(str many-to-many-join-query
              " WHERE page.id = ?"
              " AND page.name = ?")
         1 "brenton"]]
       
       fixture-model-one-to-many
       [:with :versions]             :=> [[one-to-many-join-query]]
       fixture-model-one-and-many-to-many
       [:with :categories :versions] :=> [[one-and-many-to-many-join-query]]

       nil
       [:order-by :name] :=>
       [[(str (select-from "*") " ORDER BY page.name")]]

       nil
       [:order-by :name :with [:version :order-by :id]] :=>
       [[(str (select-from "*") " ORDER BY page.name, version.id")]]))

(deftest test-selects-with-nested-withs
  (are [table q result] (= (selects sample-data-model
                                    table
                                    (parse-query (:model sample-data-model)
                                                 table q))
                           [[result]])
       
       :artist [:with [:album :with :tracks]]
       (str "SELECT "
            (attr-list [[:artist [:id :name]]
                        [:album [:id :title :release_date]]
                        [:track [:id :name]]])
            " FROM artist"
            " LEFT JOIN album_artist ON artist.id = album_artist.artist_id"
            " LEFT JOIN album ON album_artist.album_id = album.id"
            " LEFT JOIN track ON album.id = track.album_id")

       :album [:with :genre]
       (str "SELECT "
            (attr-list [[:album [:id :title :release_date]]
                        [:genre [:id :name]]])
            " FROM album"
            " LEFT JOIN genre ON album.genre_id = genre.id")))

