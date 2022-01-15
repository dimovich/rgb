(ns rgb.util
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [cl-format]])
  (:import [java.io File]))

(set! *warn-on-reflection* true)


(defn round-double [^Double x]
  (Double. ^String (cl-format nil "~,2f" x)))



(defn normalize-path [^String path]
  (-> (.replaceFirst path "^~" (System/getProperty "user.home"))
      io/file
      .getCanonicalPath))



(defn merge-existing [m1 m2]
  (reduce-kv
   (fn [res k v]
     (if (res k)
       (update res k merge v)
       res))
   m1 m2))



(defn row-fields [row field-opts]
  (reduce
   (fn [res k]
     (let [[field f] (field-opts k)]
       (assoc res k (f row field))))
   {}
   (keys field-opts)))



(defn some-paths [roots files]
  (let [roots (if (coll? roots) roots [roots])]
    (some
     (fn [root]
       (when (->> files (every? #(.exists (io/file root %))))
         root))
     roots)))



(defn list-dirs [path]
  (->> (io/file path)
       (.listFiles)
       (filter #(.isDirectory ^File %))))




(defn open-explorer [path]
  (future
    (.. java.awt.Desktop
        getDesktop
        (open (io/file path)))))
