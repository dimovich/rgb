(ns rgb.materiale
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clj-htmltopdf.core :as hp]
            [hiccup.page :refer [html5]]
            [rgb.util :as u])
  (:import [com.linuxense.javadbf DBFReader]
           [java.io File]))



(set! *warn-on-reflection* true)


(def s008-path "../S008.DBF")
(def s215-path "ARM_10/S215.DBF")

(def prereqs [s215-path])

(def output-path-fmt "ARM_10/RGB/Materiale_%s.pdf")


(def s215-fields
  {:account ["SSU" u/get-string]
   :uid ["KODMAT" u/get-int]
   :name ["NAME" u/get-string]
   :owner-id ["KODZAK" u/get-int]
   :owner-name ["NAMZAK" u/get-string]
   :unit ["NME1" (comp first
                       (partial re-find #"^(\w{1,3})")
                       u/get-string)]
   :amount ["NSHTUK" u/get-double]
   :value ["NSUMAD" u/get-double]})


(def s008-fields
  {:account ["SSU" u/get-string]
   :name ["NAME" u/get-string]})


(def accounts-to-gen
  ["21110" "21130" "21140" "21160"
   "21170" "21190" "21310"])


(def s008-xf (comp (map (partial u/row-fields s008-fields))
                   (map (juxt :account :name))))

(def s215-xf (comp (map (partial u/row-fields s215-fields))
                   (filter (comp (partial = 3) :owner-id))))




(defn get-output-path [^File root fmt]
  (->> (format fmt (.getName root))
       (io/file root)))



(defn gen-row-markup [m]
  [:p
   [:span (:uid m)]
   [:span (:name m)]
   [:span (:unit m)]
   [:span (:amount m)]
   [:span (:value m)]])




(defn gen-account-markup [acc acc-name]
  [:p {:style (str "margin-bottom: 2em;")}
   (str acc " " acc-name)])



(defn gen-markup [^File month-dir]
  (let [s008 (->> (io/file month-dir s008-path)
                  (u/dbf-rows)
                  (into {} s008-xf))
        
        s215 (->> (io/file month-dir s215-path)
                  (u/dbf-rows)
                  (into [] s215-xf)
                  (group-by :account))]

    (->> accounts-to-gen
         (mapcat
          (fn [acc]
            (->> (get s215 acc)
                 (sort-by :name)
                 (map gen-row-markup)
                 (into [(gen-account-markup acc (s008 acc))])))))))




(defn gen [^File month-dir & [out]]
  (let [out (or out (get-output-path month-dir))]
    (io/make-parents out)
    (hp/->pdf
     (html5 {:encoding "UTF-8"}
            (into [:body {:style (str "font-family: consola;")}]
                  (gen-markup month-dir)))
     out
     {:page {:size :a4
             :margin "0.7in"
             :margin-box {:bottom-right-corner {:paging [:page]}}}
      :styles
      {:fonts [{:font-family "consola"
                :src "consola.ttf"}]}})))







(comment

  (->> "/home/dimovich/Desktop/materiale.pdf"
       (gen (io/file "data/server/Bux2015/2022_07")))

  (gen-markup (io/file "data/server/Bux2015/2022_07"))




  (def s008
    (->> (io/file "data/server/Bux2015/2022_07" s008-path)
         (u/dbf-rows)
         (into {} s008-xf)))


  (def s215
    (->> (io/file "data/server/Bux2015/2022_07" s215-path)
         (u/dbf-rows)
         (into [] s215-xf)
         (group-by :account)))


  (->> (get s215 "21110")
       (map #(select-keys % [:uid :name :unit :amount :value]))
       (sort-by :name))



  )



;; S008.DBF (category id -> category name)
;; S215.DBF materiale


;; TODO
;; format data
