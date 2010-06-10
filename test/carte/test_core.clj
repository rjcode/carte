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

(def *hit-database* true)

;;
;; Test Data Transformations
;;

(def fixture-nested
     (add-original-meta
      [{:id 1 
        :title "A"
        :genre {:id 1 :name "a"}
        :artists [{:id 1 :name "x"}]} 
       {:id 2 
        :title "B"
        :genre {:id 1 :name "a"}
        :artists [{:id 1 :name "x"}]} 
       {:id 3 
        :title "C"
        :genre {:id 2 :name "b"}
        :artists [{:id 2 :name "y"}]}]))

(deftest test-conj-in
  (is (= (conj-in (first fixture-nested) [:artists] {:id 3 :name "z"})
         {:id 1 
          :title "A"
          :genre {:id 1 :name "a"}
          :artists [{:id 1 :name "x"} {:id 3 :name "z"}]})))

(deftest test-concat-in
  (is (= (concat-in (first fixture-nested) [:artists] [{:id 3 :name "z"}
                                                       {:id 4 :name "m"}])
         {:id 1 
          :title "A"
          :genre {:id 1 :name "a"}
          :artists [{:id 1 :name "x"} {:id 3 :name "z"} {:id 4 :name "m"}]})))

(deftest test-find-in
  (are [query _ expected]
       (= (find-in query
                   [{:id 4 
                     :title "Magic Potion"
                     :genre {:id 1 :name "Rock"}
                     :artists [{:id 3 :name "The Black Keys"}]} 
                    {:id 5 
                     :title "Thickfreakness"
                     :genre {:id 1 :name "Rock"}
                     :artists [{:id 3 :name "The Black Keys"}]} 
                    {:id 6 
                     :title "Let's Dance"
                     :genre {:id 1 :name "Weird"}
                     :artists [{:id 4 :name "David Bowie"}]}])
          expected)
       [:title] :=> ["Magic Potion" "Thickfreakness" "Let's Dance"]
       [{:title "Magic Potion"}] :=> [{:id 4 
                                       :title "Magic Potion"
                                       :genre {:id 1 :name "Rock"}
                                       :artists
                                       [{:id 3 :name "The Black Keys"}]}]
       
       [{:title "Magic Potion"} :id] :=> [4]
       
       [{:title "Let's Dance"} :artists] :=> [{:id 4 :name "David Bowie"}]

       [{:title "Let's Dance"} :artists :name] :=> ["David Bowie"]

       [:artists :name] :=> ["The Black Keys" "The Black Keys" "David Bowie"]

       [:genre :name] ["Rock" "Rock" "Weird"]))

(deftest test-vary-in
  (if *hit-database*
    (with-test-database default-test-data
     (are [table pattern new-vals transform-fn expected]
          (= (transform-fn
              (vary-in ($ :album :with :artists)
                       table
                       pattern
                       new-vals))
             expected)
        
          ;; Update a top level map
          :album
          {:title "Magic Potion"}
          {:title "Magic Motion"}
          #(-> % first :title)
          "Magic Motion"
        
          ;; Ensure that the top level map still has the correct metadata
          :album
          {:title "Magic Potion"}
          {:title "Magic Motion"}
          #(-> % first meta orig-key :title)
          "Magic Potion"
        
          ;; Update a map in a nested list
          :artist
          {:name "Jack White"}
          {:name "Jack Daniels"}
          #(-> (nth % 2) :artists first :name)
          "Jack Daniels"
        
          ;; Ensure that the nested map still has the correct metadata
          :artist
          {:name "Jack White"}
          {:name "Jack Daniels"}
          #(-> (nth % 2) :artists first meta orig-key :name)
          "Jack White"

          ;; Use a function to upate a value
          :artist
          {:name "Jack White"}
          {:name #(.substring % 1)}
          #(-> (nth % 2) :artists first :name)
          "ack White"))))

;;
;; Test Query
;;

(deftest test-parse-join-part
  (are [x _ y] (= (parse-join-part (:model fixture-model-one-to-many)
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
                                                 :joins {:page [:versions]}}
       [:with [:version :order-by :name]] :=> {:order-by [:version [:name :asc]]
                                               :joins {:page [:versions]}}))

(deftest test-parse-query
  (are [x _ y] (= (parse-query (:model fixture-model-one-to-many)
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
                                                :joins {:page [:versions]}}
       
       [:order-by :name] :=> {:order-by [:page [:name :asc]]}
       [:order-by :name [:id :desc]] :=> {:order-by [:page
                                                     [:name :asc :id :desc]]}
       [:order-by :name [:id :desc] :with :versions] :=>
       {:order-by [:page [:name :asc :id :desc]]
        :joins {:page [:versions]}}
       [:order-by :name [:id :desc] :with [:version :order-by :id]] :=>
       {:order-by [:page [:name :asc :id :desc] :version [:id :asc]]
        :joins {:page [:versions]}})
  
  (are [table query _ expected] (= (parse-query (:model sample-data-model)
                                                table
                                                query)
                                   expected)
       :artist
       [:with [:album :with :tracks]] :=> {:joins {:artist [:albums]
                                                   :album [:tracks]}}))

(deftest test-dequalify-joined-map
  (t "test dequalify joined map"
     (are [table flat-map expected]
          (= (dequalify-joined-map (:model fixture-model-many-to-many)
                                   table
                                   flat-map)
             expected)

        :page
        {:page_id 1 :page_name "one" :category_id 2}
        {:id 1 :name "one"}
        
        :category
        {:page_id 1 :page_name "one" :category_id 2 :category_name "two"}
        {:id 2 :name "two"})
  
     (are [table map expected]
          (= (dequalify-joined-map
              (:model (model
                       (page_category [:id :name])
                       (page [:id :name]
                             (one-to-many categories
                                          :page_category :page_id))))
              table
              map)
             expected)
          
          :page
          {:page_id 1 :page_name "one" :page_category_id 1
           :page_category_name "c1"}
          {:id 1 :name "one"})))

(deftest test-put-original-in-meta
  (are [record expected] (= (orig-key (meta (put-original-in-meta record)))
                            expected)
       {:id 1 :x "a"} {:id 1 :x "a"}
       
       {:id 1 :x "a" :l [{:id 2 :y "b"} {:id 5 :y "c"}]}
       {:id 1 :x "a" :l [{:id 2 :y "b"} {:id 5 :y "c"}]}

       {:id 1 :x "a" :l {:id 2 :y "b"}}
       {:id 1 :x "a" :l {:id 2 :y "b"}}

       {:id 1 :x "a" :l [{:id 2 :y "b" :l [{:id 5 :y "c"}]}]}
       {:id 1 :x "a" :l [{:id 2 :y "b"}]}

       {:id 1 :x "a" :l [{:id 2 :y "b" :l {:id 5 :y "c"}}]}
       {:id 1 :x "a" :l [{:id 2 :y "b"}]}

       {:id 1 :x "a" :l {:id 2 :y "b" :l {:id 5 :y "c"}}}
       {:id 1 :x "a" :l {:id 2 :y "b"}}))

(defn verify-map-metadata [m]
  (let [md (meta m)
        result (and (= (orig-key md) (canonical-form m))
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
       (let [result (flat->nested sample-data-model table joins [recs])]
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

       :album
       {:album [:artists :genre]}
       [{:artist_id 1 :artist_name "A" :album_id 2 :album_title "B"
         :genre_id 1 :genre_name "C"}
        {:artist_id 2 :artist_name "D" :album_id 3 :album_title "E"
         :genre_id 2 :genre_name "F"}]
       [{:id 2
         :title "B"
         :artists [{:id 1 :name "A"}]
         :genre {:id 1 :name "C"}}
        {:id 3
         :title "E"
         :artists [{:id 2 :name "D"}]
         :genre {:id 2 :name "F"}}]

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
       (let [result (flat->nested sample-data-model table joins recs)]
         (= (f result) expected))
       
       :artist
       {}
       [[{:artist_id 1 :artist_name "A"}]]
       #(meta (first %))
       {table-key :artist orig-key {:id 1 :name "A"}}))

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

(defn meta-rec [table rec]
  (with-meta rec {table-key table orig-key rec}))

(deftest test-dismantle-record
  (let [categories [(meta-rec :category {:id 1 :name "a"})
                    (meta-rec :category {:id 2 :name "b"})]
        result (dismantle-record
                (:model fixture-model-many-to-many)
                (meta-rec :page
                          {:id 1 :name "a" :current_version 2
                           :categories categories}))]
    (is (= result
           {:base-record {:id 1 :name "a" :current_version 2}
            :categories [{:id 1 :name "a"}
                         {:id 2 :name "b"}]}))
    (is (= (-> result :base-record meta)
           {table-key :page
            orig-key
            {:id 1 :name "a" :current_version 2 :categories categories}}))
    (is (= (-> result :categories first meta)
           {table-key :category
            orig-key
            {:id 1 :name "a"}})))
  (let [result (dismantle-record
                (:model sample-data-model)
                (meta-rec :album
                          {:id 1 :title "A"
                           :genre (meta-rec :genre {:id 2 :name "B"})}))]
    (is (= result
           {:base-record {:id 1 :title "A"}
            :genre {:id 2 :name "B"}})))
  (let [result (dismantle-record
                (:model sample-data-model)
                (meta-rec :album
                          {:id 1 :title "A"
                           :genre nil}))]
    (is (= result
           {:base-record {:id 1 :title "A"}
            :genre nil}))))

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
  (are [x y z m] (is (= (find-join-by (:model x) y :alias z) m))
       fixture-model-many-to-many :page :categories
       fixture-join-category
       fixture-model-one-to-many :page :versions
       fixture-join-version
       fixture-model-one-and-many-to-many :page :categories
       fixture-join-category
       fixture-model-one-and-many-to-many :page :versions
       fixture-join-version))

(deftest test-fetch
  (if *hit-database*
    (with-test-database default-test-data
     (are [q f expected] (= (f q) expected)
         
          (fetch db ["select * from album"]) count (count albums)
         
          (fetch db ["select * from album where title = \"Magic Potion\""])
          #(:title (first %))
          "Magic Potion"
         
          (fetch-one db ["select * from album where title = \"Magic Potion\""])
          :title
          "Magic Potion"
         
          (fetch db :album) count (count albums)
         
          (fetch db :artist)
          #(set (map :name %))
          artists
         
          (fetch db :artist [:name])
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
          the-raconteurs

          ($ :album :with :genre)
          #(-> % first :genre :name)
          "Blues/Rock"

          ($ :album :with [:genre {:name "Rock"}])
          #(set (map :title %))
          #{"Elephant" "Broken Boy Soldiers"}

          ($ :track {:name "*You*"} :with :album)
          #(set (distinct (find-in [:album :title] %)))
          #{"Magic Potion" "Elephant"}

          ($ :genre :with :albums)
          #(set (find-in [:albums :title] %))
          #{"Broken Boy Soldiers" "Elephant" "Magic Potion" "Thickfreakness"}

          ($ :album :order-by :title)
          #(map :title %)
          ["Broken Boy Soldiers" "Elephant" "Magic Potion" "Thickfreakness"
           "White Blood Cells"])
    
     (let [result ($1 :album {:title "Magic Potion"})]
       (is (= (:title result) "Magic Potion"))
       (is (= (-> result meta table-key) :album)))
     
     (let [q [:artist {:name "*White"}]
           r1 (apply $ q)
           r2 ($ :album :with q)]
       (is (= (set (map :name r1)) the-white-stripes))
       (is (= (set (map :title r2)) #{"Elephant" "Broken Boy Soldiers"}))))))

(comment
  
  ;; Order-by syntax
  ($ :album :order-by :title :id)

  ;; TODO - Test these examples
  ($ :album [:title] {:title "Y*"} :order-by :title [:id :desc] :with :genre)
  ($ :album :order-by :title :with [:artist :order-by :name])
  )

;;
;; Test Save, Update and Delete
;;

(deftest test-updates
  (if *hit-database*
    (with-test-database default-test-data
    
     (are [query update-fn test-fn expected]
          (do
            (! (update-fn (apply $1 query)))
            (= (test-fn (apply $1 query)) expected))

          ;; Add a track to an album
          [:album {:title "Magic Potion"} :with :tracks]
          #(conj-in % [:tracks] ($1 :track {:name "Black Math"}))
          #(set (find-in [:tracks :name] [%]))
          #{"Just Got To Be" "Strange Desire" "Your Touch" "You're the One"
            "Black Math"}

          ;; Remove a track from an album
          [:album {:title "Magic Potion"} :with :tracks]
          #(remove-in % [:tracks] {:name "Black Math"})
          #(set (find-in [:tracks :name] [%]))
          #{"Just Got To Be" "Strange Desire" "Your Touch" "You're the One"}
         
          ;; Associate a genre with an album
          [:album {:title "White Blood Cells"} :with :genre]
          #(assoc % :genre ($1 :genre {:name "Rock"}))
          :genre
          ($1 :genre {:name "Rock"})
         
          ;; Clear the genre
          [:album {:title "White Blood Cells"} :with :genre]
          #(assoc % :genre nil)
          :genre
          nil
         
          ;; Add an artist to an album
          [:album {:title "Magic Potion"} :with :artists]
          #(conj-in % [:artists] ($1 :artist {:name "Jack White"}))
          #(set (find-in [:artists :name] [%]))
          #{"Patrick Carney" "Dan Auerbach" "Jack White"}
         
          ;; Remove an artist from an album
          [:album {:title "Magic Potion"} :with :artists]
          #(remove-in % [:artists] {:name "Jack White"})
          #(set (find-in [:artists :name] [%]))
          #{"Patrick Carney" "Dan Auerbach"}
         
          ;; Add an album to an artist
          [:artist {:name "Jack White"} :with :albums]
          #(conj-in % [:albums] ($1 :album {:title "Magic Potion"}))
          #(set (find-in [:albums :title] [%]))
          #{"Elephant" "Broken Boy Soldiers" "Magic Potion"}
         
          ;; Remove an album from an artist
          [:artist {:name "Jack White"} :with :albums]
          #(remove-in % [:albums] {:title "Magic Potion"})
          #(set (find-in [:albums :title] [%]))
          #{"Elephant" "Broken Boy Soldiers"})
     
     ;; Test adding a new album
     (let [result (! :album {:title "New Album"})
           rec ($1 :album {:title "New Album"})]
       (is (= (:title rec) "New Album"))
       (is (= result rec)))
     
     ;; Test that deleting an album will also delete the associated tracks
     (let [all-tracks (count ($ :track))
           mp-tracks (count ($ :track :with [:album {:title "Magic Potion"}]))
           album (-> ($1 :album {:title "Magic Potion"} :with :artists)
                     (assoc :artists []))
           saved (! album)
           result (delete-record sample-db album)]
       (is (= (count ($ :track)) (- all-tracks mp-tracks)))))))


