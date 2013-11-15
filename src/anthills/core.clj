(ns anthills.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [incanter.core :refer [dataset view]]
            [incanter.charts :refer [scatter-plot]]))

;;--------------------------------------------------------------------
;; parsing

;; these functions parse the data sets into vectors of points,
;; where each point is represented by a vector tuple [x y]

(defn parse-int
  [s]
  (Integer/parseInt s))

(defn parse-line
  [line]
  (mapv parse-int (string/split line #"\s")))

(defn parse-file
  [path]
  (->> (io/reader path)
       (line-seq)
       (mapv parse-line)))

;; now use these functions to the sample data

(def data1 (parse-file "resources/data1.txt"))
(def data2 (parse-file "resources/data2.txt"))
(def data3 (parse-file "resources/data3.txt"))


;;--------------------------------------------------------------------
;; k-means clustering as understood after hastily reading:
;; http://en.wikipedia.org/wiki/K-means_clustering

(defn pick-n
  "Randomly pick n distinct elements from coll"
  [n coll]
  (loop [ret #{}]
    (if (= n (count ret))
      ret
      (recur (conj ret (rand-nth coll))))))

(defn distance
  "Calculate the euclidean distance between two points"
  [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (Math/pow (- x1 x2) 2)
                (Math/pow (- y1 y2) 2))))

(defn closest-point
  "Given a sequence of points, return the one that is closest to p, as
  determined by having the shortest euclidean distance."
  [points p]
  (first (sort-by (partial distance p) points)))

(defn partition-into-clusters
  "Partition `points` into k (count means) groups, based on the
  proximity to each mean, where shortest distance wins."
  [means points]
  (vals (group-by (partial closest-point means) points)))

(defn add-points
  "Add two points."
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn sum-points
  "Calculate the sum of a sequence of points."
  [points]
  (reduce add-points [0 0] points))

(defn average-point
  "Calculate the average point of a sequence of points."
  [points]
  (let [[x y] (sum-points points)
        npoints (count points)]
    [(long (/ x npoints))
     (long (/ y npoints))]))

(defn clusters
  "Given a number n, and a sequence of points, try to assign each
  point to one of n clusters. Returns a sequence of clusters."
  ([n points]
     (clusters n points (pick-n n points)))
  ([n points initial-means]
     (loop [means    initial-means
            clusters (partition-into-clusters means points)]
       (let [new-means (map average-point clusters)]
         (if (= (set means) (set new-means))
           clusters
           (recur new-means (partition-into-clusters new-means points)))))))

;;--------------------------------------------------------------------
;; visualisation using incanter

(defn clusters->dataset
  [clusters]
  (dataset
   [:x :y :cluster]
   (for [[cluster n] (partition 2 (interleave clusters (range)))
         [x y] cluster]
     {:x x
      :y y
      :cluster n})))

(defn view-clusters
  [clusters]
  (view (scatter-plot :x :y
                      :data (clusters->dataset clusters)
                      :group-by :cluster)))

(defn view-dataset
  [data]
  (view (scatter-plot (map first data)
                      (map second data))))


(comment

  ;; data1
  (view-dataset data1)
  (view-clusters (clusters 3 data1))

  ;; data2
  (view-dataset data2)
  (view-clusters (clusters 3 data2))

  ;; data3
  (view-dataset data3)
  (view-clusters (clusters 5 data3))

  )

