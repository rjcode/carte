(ns carte.sample-migrations
  (:use (carte sql)))

(defn migration-20100524000 []
  {:forward (fn [db] nil)})

(defn migration-20100524001 []
  {:forward
   (create-table-fn :album
                    (col :title :string :not-null))
   :back
   (drop-table-fn :album)})

(defn migration-20100525000 []
  {:forward
   (create-table-fn :artist
                    (col :name :string :not-null))
   :back
   (drop-table-fn :artist)})

(defn migration-20100525001 []
  {:forward
   (create-table-fn :album_artist
                    (col :album_id :id :not-null)
                    (col :artist_id :id :not-null)
                    (constraint :album_id :=> :album :id)
                    (constraint :artist_id :=> :artist :id))
   :back
   (drop-table-fn :album_artist)})

(defn migration-20100601000 []
  {:forward
   (create-table-fn :genre
                    (col :name :string :not-null))
   :back
   (drop-table-fn :genre)})

;; TODO - add constraints for the new stuff that you added to the
;; table.

(defn migration-20100601001 []
  {:forward
   (alter-table-fn :album
                   (add-col (col :genre_id :id) :after :title))
   :back
   (alter-table-fn :album (drop-col :genre_id))})

(defn migration-20100601002 []
  {:forward
   (create-table-fn :track
                    (col :name :string :not-null)
                    (col :album_id :id))
   :back
   (drop-table-fn :track)})
