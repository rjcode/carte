;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns carte.test-core
  (:use (clojure test)
        (carte core model fixtures)))

(deftest test-parse-join-part
  (are [x _ y] (= (parse-join-part fixture-model-one-to-many
                                   :page
                                   x)
                  y)
       [:with :versions] :=> {:joins {:page [:versions]}}
       [:with [:version]] :=> {:joins {:page [:versions]}}
       [:with [:version {:name "a"}]] :=> {:criteria {:version [{:name "a"}]}
                                           :joins {:page [:versions]}}
       [:with [:version [:id] {:name "a"}]] :=> {:attrs {:version [:id]}
                                                 :criteria
                                                 {:version [{:name "a"}]}
                                                 :joins {:page [:versions]}}))

(deftest test-parse-query
  (are [x _ y] (= (parse-query fixture-model-one-to-many
                               :page
                               x)
                  y)
       []                        :=> {}
       [[:id]]                   :=> {:attrs {:page [:id]}}
       [{:name "a"}]             :=> {:criteria {:page [{:name "a"}]}}
       [{:name "a" :age 7}]      :=> {:criteria {:page [{:name "a" :age 7}]}}
       [{:name "a"} {:name "b"}] :=> {:criteria {:page [{:name "a"}
                                                        {:name "b"}]}}
       [[:id] {:name "a"}]       :=> {:attrs {:page [:id]}
                                      :criteria {:page [{:name "a"}]}}
       [:with :versions]         :=> {:joins {:page [:versions]}}
       [:with [:version]]        :=> {:joins {:page [:versions]}}
       [[:id] {:name "a"} :with :versions] :=> {:attrs {:page [:id]}
                                                :criteria {:page [{:name "a"}]}
                                                :joins {:page [:versions]}})
  
  (are [table query _ expected] (= (parse-query data-model table query)
                                   expected)
       :artist
       [:with [:album :with :tracks]] :=> {:joins {:artist [:albums]
                                                   :album [:tracks]}}))

(deftest test-dequalify-joined-map
  (t "test dequalify joind map"
     (are [x y z] (= (dequalify-joined-map fixture-model-many-to-many x y) z)

        :page
        {:page_id 1 :page_name "one" :category_id 2}
        {:id 1 :name "one"}
        
        :category
        {:page_id 1 :page_name "one" :category_id 2 :category_name "two"}
        {:id 2 :name "two"})
  
     (are [table map expected]
          (= (dequalify-joined-map
              (model
               (page_category [:id :name])
               (page [:id :name]
                     (one-to-many categories
                                  :page_category :page_id)))
              table
              map)
             expected)
          
          :page
          {:page_id 1 :page_name "one" :page_category_id 1
           :page_category_name "c1"}
          {:id 1 :name "one"})))

(defn verify-map-metadata [m]
  (let [md (meta m)
        result (and (= (orig-key md) m)
                    (not (nil? (table-key md))))]
    (if (not result)
      (do (println "metadata error: " md)
          result)
      result)))

(defn verify-nested-metadata [coll]
  (reduce
   (fn [result next-map]
     (and result
          (verify-map-metadata next-map)
          (reduce
           (fn [a b]
             (and a
                  (let [next-value (val b)]
                    (cond (vector? next-value)
                          (verify-nested-metadata next-value)
                          (map? next-value)
                          (verify-map-metadata next-value)
                          :else true))))
           true
           next-map)))
   true
   coll))

(deftest test-flat->nested
  (are [table joins recs expected]
       (let [result (flat->nested data-model table joins [recs])]
         (and (= result expected)
              (true? (verify-nested-metadata result))))
       
       :artist
       {}
       [{:artist_id 1 :artist_name "A"}]
       [{:id 1 :name "A"}]

       :artist
       nil
       [{:artist_id 1 :artist_name "A"}]
       [{:id 1 :name "A"}]

       :artist
       {}
       [{:artist_id 1 :artist_name "A"}
        {:artist_id 2 :artist_name "B"}]
       [{:id 1 :name "A"}
        {:id 2 :name "B"}]

       :artist
       {:artist [:albums]}
       [{:artist_id 1 :artist_name "A" :album_id 2 :album_title "B"}]
       [{:id 1 :name "A" :albums [{:id 2 :title "B"}]}]

       :album
       {:album [:artists :tracks]}
       [{:artist_id 1 :artist_name "A" :album_id 2 :album_title "B"
         :track_id 3 :track_name "C"}]
       [{:id 2
         :title "B"
         :artists [{:id 1 :name "A"}]
         :tracks [{:id 3 :name "C"}]}]

       :artist
       {:artist [:albums]}
       [{:artist_id 1 :artist_name "A" :album_id 2 :album_title "B"}
        {:artist_id 1 :artist_name "A" :album_id 3 :album_title "C"}]
       [{:id 1 :name "A" :albums [{:id 2 :title "B"}
                                  {:id 3 :title "C"}]}]

       :album
       {:album [:artists :tracks]}
       [{:artist_id 1 :artist_name "A" :album_id 2 :album_title "B"
         :track_id 3 :track_name "C"}
        {:artist_id 1 :artist_name "A" :album_id 2 :album_title "B"
         :track_id 4 :track_name "D"}]
       [{:id 2
         :title "B"
         :artists [{:id 1 :name "A"}]
         :tracks [{:id 3 :name "C"}
                  {:id 4 :name "D"}]}]

       :artist
       {:artist [:albums]}
       [{:artist_id 1 :artist_name "A" :album_id 3 :album_title "C"}
        {:artist_id 1 :artist_name "A" :album_id 4 :album_title "D"}
        {:artist_id 2 :artist_name "B" :album_id 5 :album_title "E"}
        {:artist_id 2 :artist_name "B" :album_id 6 :album_title "F"}]
       [{:id 1 :name "A" :albums [{:id 3 :title "C"}
                                  {:id 4 :title "D"}]}
        {:id 2 :name "B" :albums [{:id 5 :title "E"}
                                  {:id 6 :title "F"}]}]

       :artist
       {:artist [:albums] :album [:tracks]}
       [{:artist_id 1 :artist_name "A" :album_id 2 :album_title "B"
         :track_id 3 :track_name "C"}]
       [{:id 1 :name "A" :albums [{:id 2 :title "B"
                                   :tracks [{:id 3 :name "C"}]}]}]

       :artist
       {:artist [:albums] :album [:tracks]}
       [{:artist_id 1 :artist_name "A" :album_id 2 :album_title "B"
         :track_id 3 :track_name "C"}
        {:artist_id 1 :artist_name "A" :album_id 2 :album_title "B"
         :track_id 4 :track_name "D"}]
       [{:id 1 :name "A" :albums [{:id 2 :title "B"
                                   :tracks [{:id 3 :name "C"}
                                            {:id 4 :name "D"}]}]}]))

(deftest test-flat->nested-metadata
  (are [table joins recs f expected]
       (let [result (flat->nested data-model table joins recs)]
         (= (f result) expected))
       
       :artist
       {}
       [[{:artist_id 1 :artist_name "A"}]]
       #(meta (first %))
       {table-key :artist orig-key {:id 1 :name "A"}}

       ))

(deftest test-transform-query-plan-results
  (let [t-q-p-r
        (fn [model res]
          (flat->nested model
                        :page
                        (:joins
                         (parse-query model
                                      :page
                                      [:with :categories]))
                        res))]
    (t "test transform query plan results"
       (let [result (t-q-p-r fixture-model-many-to-many
                             fixture-join-flat)
             first-result (first result)
             categories (-> result second :categories)
             first-cat (first categories)]
         (t "- is the entire structure correct"
            (is (= result fixture-join-nested)))
         (t "- does a specific category contain the correct metadata"
            (is (= (meta first-cat)
                   {table-key :category
                    orig-key first-cat})))
         (t "- does metadata for category contain the original value"
            (is (= first-cat (-> first-cat meta orig-key)))))
       (t "with common prefixes"
          (is (= (t-q-p-r (model
                           (page_category [:id :name])
                           (page [:id :name]
                                 (one-to-many categories
                                              :page_category :page_id)))
                          [[{:page_id 1 :page_name "one" :page_category_id 1
                             :page_category_name "c1"}]])
                 [{:id 1 :name "one"
                   :categories [{:id 1 :name "c1"}]}]))))))

(deftest test-dismantle-record
  (let [result
        (dismantle-record
         fixture-model-many-to-many
         (with-meta
           {:id 1 :name "a" :current_version 2
            :categories [(with-meta {:id 1 :name "a"}
                                      {table-key :category
                                       orig-key
                                       {:id 1 :name "a"}})
                                    (with-meta {:id 2 :name "b"}
                                      {table-key :category
                                       orig-key
                                       {:id 2 :name "b"}})]}
           {table-key :page
            orig-key
            {:id 1 :name "a" :current_version 2}}))]
    (is (= result
           {:base-record {:id 1 :name "a" :current_version 2}
            :categories [{:id 1 :name "a"}
                         {:id 2 :name "b"}]}))
    (is (= (-> result :base-record meta)
           {table-key :page
            orig-key
            {:id 1 :name "a" :current_version 2}}))
    (is (= (-> result :categories first meta)
           {table-key :category
            orig-key
            {:id 1 :name "a"}}))))

(deftest test-dirty?
  (t "test dirty?"
     (let [record {:id 1 :name "a"}]
       (are [s x y] (t s (is (= (dirty? y) x)))
            "with no metadata"
            true record
            "with metadata but no original"
            true (with-meta record {table-key :a})
            "with metadata and different original"
            true (with-meta record {table-key :a
                                    orig-key
                                    {:id 1 :name "b"}})
            "with correct original"
            false (with-meta record {table-key :a
                                     orig-key record})))))

(deftest test-find-join-by
  (are [x y z m] (is (= (find-join-by x y :alias z) m))
       fixture-model-many-to-many :page :categories
       fixture-join-category
       fixture-model-one-to-many :page :versions
       fixture-join-version
       fixture-model-one-and-many-to-many :page :categories
       fixture-join-category
       fixture-model-one-and-many-to-many :page :versions
       fixture-join-version))

(deftest test-find-in
  (are [query _ expected]
       (= (find-in query
                   [{:id 4 
                     :title "Magic Potion" 
                     :artists [{:id 3 :name "The Black Keys"}]} 
                    {:id 5 
                     :title "Thickfreakness"
                     :artists [{:id 3 :name "The Black Keys"}]} 
                    {:id 6 
                     :title "Let's Dance" 
                     :artists [{:id 4 :name "David Bowie"}]}])
          expected)
       [:title] :=> ["Magic Potion" "Thickfreakness" "Let's Dance"
                                          ]
       [{:title "Magic Potion"}] :=> [{:id 4 
                                       :title "Magic Potion" 
                                       :artists
                                       [{:id 3 :name "The Black Keys"}]}]
       
       [{:title "Magic Potion"} :id] :=> [4]
       
       [{:title "Let's Dance"} :artists] :=> [{:id 4 :name "David Bowie"}]

       [{:title "Let's Dance"} :artists :name] :=> ["David Bowie"]

       [:artists :name] :=> ["The Black Keys" "The Black Keys" "David Bowie"]))

;; The following tests require that you have a mysql database
;; available on localhost. 

(deftest test-query
  (with-test-database default-test-data
    (are [q f expected] (= (f q) expected)
         
         (query db ["select * from album"]) count (count albums)
         
         (query db ["select * from album where title = \"Magic Potion\""])
         #(:title (first %))
         "Magic Potion"
         
         (query-1 db ["select * from album where title = \"Magic Potion\""])
         :title
         "Magic Potion"
         
         (query db :album) count (count albums)
         
         (query db :artist)
         #(set (map :name %))
         artists
         
         (query db :artist [:name])
         #(set (map :name %))
         artists
         
         ($ :artist [:name])
         #(set (map :name %))
         artists
         
         ($ :album) count (count albums)
                  
         ($1 :artist [:name] {:name "Jack White"})
         :name
         "Jack White"

         ($1 :artist [:name] {:name "Jack White"})
         #(-> % meta orig-key :name)
         "Jack White"

         ($1 :artist [:name] {:name "Jack White"})
         #(-> % meta table-key)
         :artist
         
         ($ :album {:title "Mag*"} {:title "Th*"})
         #(set (map :title %))
         #{"Magic Potion" "Thickfreakness"}
         
         ($ :album {:where ["title like ? or title like ?" "Mag%" "Th%"]})
         #(set (map :title %))
         #{"Magic Potion" "Thickfreakness"}
         
         ($ :album :with :artists)
         #(set (find-in [{:title "Magic Potion"} :artists :name] %))
         the-black-keys
         
         ($1 :album {:title "Magic Potion"} :with :artists)
         #(set (map :name (:artists %)))
         the-black-keys
         
         ($1 :album {:where ["title = ?" "Magic Potion"]} :with :artists)
         #(set (map :name (:artists %)))
         the-black-keys
         
         ($ :artist :with :albums)
         #(find-first-in [{:name (first the-black-keys)} :albums :title] %)
         "Magic Potion"
         
         ($ :album :with [:artist])
         #(set (find-in [{:title "Magic Potion"} :artists :name] %))
         the-black-keys
         
         ($ :album :with [:artist {:name "Jack White"}]) count 2
         
         ($ :album :with [:artist {:name "Jack White"}])
         #(set (map :title %))
         #{"Elephant" "Broken Boy Soldiers"}

         ($ :album :with :tracks)
         #(set (find-in [{:title "Magic Potion"} :tracks :name] %))
         magic-potion-tracks

         ($ :album :with [:track {:name "Seven*"}])
         #(set (map :title %))
         #{"Elephant"}

         ($ :album :with :tracks :artists)
         #(set (map :name (find-in [{:title "Elephant"} :artists] %)))
         the-white-stripes

         ($ :album :with :tracks :artists)
         #(set (map :name (find-in [{:title "Elephant"} :tracks] %)))
         elephant-tracks
         
         ($ :artist :with [:album :with :tracks])
         #(set (map :name (find-in [{:name "Meg White"} :albums :tracks] %)))
         elephant-tracks

         ($ :artist :with [:album :with [:track {:name "Call*"}]])         
         #(set (map :name %))
         the-raconteurs)
    
     (let [result ($1 :album {:title "Magic Potion"})]
       (is (= (:title result) "Magic Potion"))
       (is (= (-> result meta table-key) :album)))
     
     (let [q [:artist {:name "*White"}]
           r1 (apply $ q)
           r2 ($ :album :with q)]
       (is (= (set (map :name r1)) the-white-stripes))
       (is (= (set (map :title r2)) #{"Elephant" "Broken Boy Soldiers"})))))

