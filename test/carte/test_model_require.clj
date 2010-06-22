;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns carte.test-model-require
  (:use (clojure test)
        (carte fixtures))
  (:require (carte [model :as model])))

(deftest test-model-when-required
  (are [x] (= x fixture-artist-album-model)
             
       (model/model
        (album [:id :title]
               (many-to-many artists :artist
                             => :album_artist :album_id :artist_id))
        (artist [:id :name]))))
