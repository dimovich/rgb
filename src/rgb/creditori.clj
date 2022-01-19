(ns rgb.creditori
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clj-htmltopdf.core :as hp]
            [hiccup.page :refer [html5]]
            [rgb.util :as u])
  (:import [com.linuxense.javadbf DBFReader]
           [java.io File]))


(set! *warn-on-reflection* true)


(defn read-entities [fname]
  (with-open [rdr (DBFReader. (io/input-stream fname))]
    (loop [row (.nextRow rdr)
           res []]
      (if row
        (let [company (.getString row "NAMZAK")
              credit (u/round-double (Double. (.getString row "KSUMAK")))]
          (recur
           (.nextRow rdr)
           (if (zero? credit)
             res
             (conj res (str company "," credit)))))

        (apply str (interpose "\n" res))))))



(defn read-entities2 [fname]
  (with-open [rdr (DBFReader. (io/input-stream fname))]
    (loop [row (.nextRow rdr)
           res []]
      (if row
        (let [company (.getString row "NAMZAK")
              credit (u/round-double (Double. (.getString row "KSUMAK")))]
          (recur
           (.nextRow rdr)
           (if (< 0.5 credit)
             (conj res [company credit])
             res)))
        res))))


(def s757b-path "ARM_10/S757B.DBF")
(def v521-path "ARM_10/V52110A.DBF")
(def prereqs [v521-path])

(def output-path-ptrn "ARM_10/RGB/Creditori_%s.pdf")

(defn get-output-file [^File month-dir]
  (->> (format output-path-ptrn (.getName month-dir))
       (io/file month-dir)))



(defn gen-markup [data]
  (->> data
       (map
        (fn [[company sum]]
          [:h3 {:style (str "font-family: serif;"
                            "border-bottom: 1px dotted gray;"
                            "width: 19em;"
                            "font-size: 19px;"
                            "font-weight: normal;"
                            "white-space:nowrap")}
           [:span {:style (str "width: 12em;"
                               "text-overflow: ellipsis;"
                               "white-space: nowrap;"
                               "overflow: hidden;"
                               "display: inline-block")}
            company]
           (let [[_ decimal] (clojure.string/split (format "%.2f" sum) #"\.|\,")]
             [:span {:style (str "width: 8.3em;"
                                 "overflow: hidden;"
                                 "font-size: 16px;"
                                 "font-family: consola;"
                                 ;;"font-weight: bold;"
                                 "text-align: right;"
                                 "display: inline-block")}
              (u/spacefy (quot sum 1)) "."
              [:span {:style "font-size: 12px; color: gray"}
               decimal]])]))
       (into [:div #_{:style "margin: 2em"}])))



(defn gen-month-markup [^File month-dir]
  [:p {:style (str "font-family: consola;"
                   "margin-bottom: 2em;") }
   (let [[y m] (s/split (.getName month-dir) #"_")]
     (str m "-" y))])



(defn gen [^File month-dir & [out]]
  (let [output-file (or out (get-output-file month-dir))]
    (io/make-parents output-file)
    (hp/->pdf
     (html5 {:encoding "UTF-8"}
            [:body
             (gen-month-markup month-dir)
             
             (->> (read-entities2 (io/file month-dir v521-path))
                  (gen-markup))])
     output-file
     {:page {:margin "0.7in"
             :size :a4
             :margin-box {:bottom-right-corner {:paging [:page]}}}
      :styles
      {:fonts [{:font-family "consola"
                :src "consola.ttf"}]}})))




(defn -main [& [in out]]
  (let [in (io/file in v521-path)
        out (or out "creditori.csv")]
    (if (.exists in)
      (spit out (read-entities in))
      (throw (Exception. (str "missing " in))))))



;; clojure -X:uberjar :jar creditori.jar :main-class rgb.creditori
;; java -jar creditori.jar "/run/user/1000/gvfs/smb-share:server=agroialserver,share=serverd/Bux2015/2021_07"
;; java -jar creditori.jar ./data/creditori



;; cli version
;; rgb.jar --creditori (folder | file) output.pdf
