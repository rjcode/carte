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

(deftest test-model
  (t "test creating a model"
     (t "containing a many-to-many relationship"
        (is (= fixture-model-many-to-many
               {:category {:attrs [:id :name]
                            :alias :categories}
                 :page {:attrs [:id :name :current_version]
                        :joins #{fixture-join-category}}})))
     (t "containing a one-to-many relationship"
        (is (= fixture-model-one-to-many
               {:version {:attrs [:id :content]
                           :alias :versions}
                 :page {:attrs [:id :name :current_version]
                        :joins #{fixture-join-version}}})))
     (t "containing both one and many -to-many"
        (is (= fixture-model-one-and-many-to-many
               {:category {:attrs [:id :name]
                            :alias :categories}
                 :version {:attrs [:id :content]
                           :alias :versions}
                 :page {:attrs [:id :name :current_version]
                        :joins #{fixture-join-category
                                 fixture-join-version}}})))
     (t "using the relation macro"
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
        (are [x] (= x {:version {:alias :versions}
                       :page {:attrs [:name]
                              :joins #{{:type :one-to-many
                                        :alias :versions
                                        :table :version
                                        :link :page_id}}}})
             (model (page [:name]
                          (one-to-many versions :version :page_id)))
             
             (model (page [:name]
                          (one-to-many :version :page_id)))

             (model (page [:name]
                          (one-to-many :version)))))))
