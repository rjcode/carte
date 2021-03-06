;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns carte.test-model
  (:use (clojure test)
        (carte model fixtures)))

(deftest test-join-fixture
  (is (= (join-fixture :many-to-many :page :category)
         {:type :many-to-many
          :table :category
          :alias :categories
          :link :page_category
          :from :page_id
          :to :category_id})))

(deftest test-merge-join-sets
  (t "test merge join sets"
     (let [join1 (join-fixture :many-to-many :page :category)
           join2 {:type :many-to-many
                  :table :category
                  :alias :cats
                  :link :page_category
                  :from :page_id
                  :to :category_id}
           join3 (join-fixture :many-to-many :page :version)
           join4 {:type :many-to-many
                  :table :cats
                  :alias :cats
                  :link :page_cats
                  :from :page_id
                  :to :cat_id}]
       (t "when adding a different alias"
          (is (= (merge-join-sets #{join1}
                                  #{join2})
                 #{join1
                   join2})))
       (t "when adding the same alias"
          (is (= (merge-join-sets #{join2}
                                  #{join4})
                 #{join4})))
       (t "when they are concatinated"
          (is (= (merge-join-sets #{join1}
                                  #{join3})
                 #{join1
                   join3}))))))

(deftest test-model
  (t "test creating a model"
     (t "containing a many-to-many relationship"
        (is (= fixture-model-many-to-many
               {:model {:category {:attrs [:id :name]
                                   :joins #{fixture-join-page}}
                        :page {:attrs [:id :name :current_version]
                               :joins #{fixture-join-category}}}})))
     (t "with one-to-many"
        (is (= (model
                (genre [:id :name])
                (album [:id :title]
                       (many-to-one :genre :genre_id)))
               {:model {:genre {:attrs [:id :name]
                                :joins #{{:type :one-to-many
                                          :table :album
                                          :alias :albums
                                          :link :genre_id
                                          :cascade-delete false}}}
                        :album {:attrs [:id :title]
                                :joins #{{:type :many-to-one
                                          :table :genre
                                          :alias :genre
                                          :link :genre_id}}}}})))
     (t "containing a one-to-many relationship"
        (is (= fixture-model-one-to-many
               {:model {:version {:attrs [:id :content]
                                  :joins #{{:type :many-to-one
                                            :table :page
                                            :alias :page
                                            :link :page_id}}}
                        :page {:attrs [:id :name :current_version]
                               :joins #{fixture-join-version}}}})))
     (t "containing both one and many -to-many"
        (is (= fixture-model-one-and-many-to-many
               {:model {:category {:attrs [:id :name]
                                   :joins #{fixture-join-page}}
                        :version {:attrs [:id :content]
                                  :joins #{{:type :many-to-one
                                            :table :page
                                            :alias :page
                                            :link :page_id}}}
                        :page {:attrs [:id :name :current_version]
                               :joins #{fixture-join-category
                                        fixture-join-version}}}})))
     (t "using the model macro"
        (are [x] (= x fixture-artist-album-model)
             
             (model
              (album [:id :title]
                     (many-to-many artists :artist
                                   => :album_artist :album_id :artist_id))
              (artist [:id :name]))
             
             (model
              (album [:id :title]
                     (many-to-many artists :artist => :album_artist))
              (artist [:id :name]))
             
             (model
              (album [:id :title]
                     (many-to-many artists :artist))
              (artist [:id :name]))
             
             (model
              (album [:id :title]
                     (many-to-many :artist => :album_artist))
              (artist [:id :name]))
             
             (model
              (album [:id :title]
                     (many-to-many :artist))
              (artist [:id :name])))

        (are [x] (= x {:model {:version {:joins #{{:type :many-to-one
                                                   :table :page
                                                   :alias :page
                                                   :link :page_id}}}
                               :page {:attrs [:name]
                                      :joins #{{:type :one-to-many
                                                :alias :versions
                                                :table :version
                                                :link :page_id
                                                :cascade-delete false}}}}})
             (model (page [:name]
                          (one-to-many versions :version :page_id)))
             
             (model (page [:name]
                          (one-to-many :version :page_id)))
             
             (model (page [:name]
                          (one-to-many :version)))))))

(deftest test-various-models
  (t "test model"
     (t "with one table"
        (is (= (:model (model (bol [:name])))
               {:bol {:attrs [:name]}})))
     (t "with two joins on the same table"
        (is (= (:model (model (site [:name])
                              (bol [:name]
                                   (many-to-one origin :site :origin_id)
                                   (many-to-one dest :site :dest_id))))
               {:bol {:attrs [:name]
                      :joins #{{:alias :dest :link :dest_id :table :site
                                :type :many-to-one}
                               {:alias :origin :link :origin_id :table :site
                                :type :many-to-one}}}
                :site {:attrs [:name]
                       :joins #{{:alias :bols :cascade-delete false
                                 :link :dest_id :table :bol
                                 :type :one-to-many}}}})))))
