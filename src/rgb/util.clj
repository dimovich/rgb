(ns rgb.util
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [cl-format]]
            [clojure.string :as s]
            [clj-time.core :as ct]
            [clj-time.coerce :as ctc]
            [clj-time.format :as ctf])
  (:import [java.io File]
           [java.text DecimalFormat]
           [com.linuxense.javadbf DBFReader DBFRow]))

(set! *warn-on-reflection* true)


(defn round-double [^Double x]
  (Double. ^String (cl-format nil "~,2f" x)))


(def get-string #(.getString ^DBFRow %1 ^String %2))
(def get-double #(.getDouble ^DBFRow %1 ^String %2))
(def get-int #(.getInt ^DBFRow %1 ^String %2))
(def get-date
  #(-> (.getDate ^DBFRow %1 ^String %2)
       ctc/from-date
       (ct/from-time-zone (ct/time-zone-for-offset -3))))


(def out-fmt (ctf/formatter "dd.MM.yyyy"))
(def format-date #(ctf/unparse out-fmt %))


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



(defn row-fields [fields row]
  (reduce
   (fn [res k]
     (let [[field f] (fields k)]
       (assoc res k (f row field))))
   {}
   (keys fields)))



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




(def decimal-separator (.. (DecimalFormat.) getDecimalFormatSymbols getDecimalSeparator))
(def separator-pattern (re-pattern (str "\\" decimal-separator)))


(defn spacefy
  "123456.33555 --> 123'456.34
  cl-format options: ~mincol,padchar,commachar:D"
  [^Double num & [prec]]
  (if (number? num)
    (let [num' (Math/abs num)
          prec (or prec (if (< 1 num') 0.01 0.001))
          right-len (int (Math/abs (Math/log10 prec)))
          num' (round-double num')
          left (long num')]

      (str (cl-format nil "~,,'':D" left)
           (when-not (== num' left)
             (let [pat (str "%." right-len "f")
                   num'' (format pat num')
                   idx (s/index-of num'' decimal-separator)]
               (subs num'' idx (min (count num'')
                                    (inc (+ idx right-len))))))))
    num))



(defn whole-sep-decimal [num]
  (let [num' (if (int? num) (double num) num)
        [whole decimal] (-> (format "%.2f" num')
                            (s/split separator-pattern))]
    
    [whole decimal-separator decimal]))




(defn dbf-rows [dbf-file]
  (reify clojure.lang.IReduceInit
    (reduce [this f init]
      (with-open [rdr (DBFReader. (io/input-stream dbf-file))]
        (loop [acc init]
          (let [row (.nextRow rdr)]
            (if (and (some? row)
                     (not (reduced? acc)))
              (recur (f acc row))
              (unreduced acc))))))))




(defn gen-month-markup [^File month-dir]
  [:p {:style (str "font-family: consola;"
                   "margin-bottom: 2em;") }
   (let [[y m] (s/split (.getName month-dir) #"_")]
     (str m "-" y))])
