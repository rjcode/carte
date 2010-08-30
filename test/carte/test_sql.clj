;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns carte.test-sql
  (:use (clojure test)
        (carte core sql model fixtures)))

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

(deftest test-select-attrs-from-joins
  (t "test select attrs from joins"
     (t "with single join"
        (is (= (select-attrs-from-joins
                (model (artist [:a])
                       (album [:b]
                              (many-to-many :artist)))
                {}
                {:artist {:table :artist :joins [:albums]}})
               ["artist.a AS artist_a" "albums.b AS albums_b"])))
     (t "with nested joins"
        (is (= (select-attrs-from-joins
                (model (track [:c]
                              (belongs-to :album))
                       (artist [:a]
                               (many-to-many :album))
                       (album [:b]))
                {}
                {:artist {:table :artist :joins [:albums]}
                 :albums {:table :album :joins [:tracks]}})
               ["artist.a AS artist_a"
                "albums.b AS albums_b"
                "tracks.c AS tracks_c"])))))

(deftest test-select-attrs
  (t "test select attrs"
     (t "with no model or joins"
        (is (= (select-attrs nil
                             :artist
                             {}
                             {})
               "*")))
     (t "with a model and no joins"
        (is (= (select-attrs (model (artist [:a])
                                    (album [:b]
                                           (many-to-many :artist)))
                             :artist
                             {}
                             {})
               "artist.a AS artist_a")))
     (t "with a model and no joins"
        (is (= (select-attrs (model (artist [:a])
                                    (album [:b]
                                           (many-to-many :artist)))
                             :artist
                             {}
                             {:artist {:table :artist :joins [:albums]}})
               "artist.a AS artist_a, albums.b AS albums_b")))))

(deftest test-selects
  (are [db q _ result] (= (selects db
                                   :page
                                   (parse-query (:model db) :page q))
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
       
       fixture-model-one-to-many
       [:order-by :name :with [:versions :order-by :id]] :=>
       [[(str (select-from (attr-list [page-table version-table]))
              " LEFT JOIN version versions ON page.id = versions.page_id"
              " ORDER BY page.name, versions.id")]]))

(deftest test-selects-with-nested-withs
  (are [table q result] (= (selects sample-data-model
                                    table
                                    (parse-query (:model sample-data-model)
                                                 table q))
                           [[result]])
       
       :artist [:with [:albums :with :tracks]]
       (str "SELECT "
            (attr-list [[:albums [:id :title :genre_id :release_date
                                  :lead_vocals_id]]
                        [:tracks [:id :name :album_id]]
                        [:artist [:id :name]]])
            " FROM artist"
            " LEFT JOIN album_artist ON artist.id = album_artist.artist_id"
            " LEFT JOIN album albums ON album_artist.album_id = albums.id"
            " LEFT JOIN track tracks ON albums.id = tracks.album_id")

       :album [:with :genre]
       (str "SELECT "
            (attr-list [[:album [:id :title :genre_id :release_date
                                  :lead_vocals_id]]
                        [:genre [:id :name]]])
            " FROM album"
            " LEFT JOIN genre ON album.genre_id = genre.id")))

(deftest test-unique-key
  (is (= (unique-key :name)
         "UNIQUE KEY name (name)"))
  (is (= (unique-key :name :age)
         "UNIQUE KEY name_age (name, age)")))

