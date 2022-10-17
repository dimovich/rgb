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
   :amount ["KSHTUK" u/get-double]
   :value ["KSUMAD" u/get-double]})


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




(defn get-output-file
  ([^File root] (get-output-file root output-path-fmt))
  ([^File root fmt] (->> (format fmt (.getName root))
                         (io/file root))))



(defn gen-cell [style text]
  [:span
   {:style (str style
                "display: inline-block;"
                "vertical-align: top;"
                ;;"border: 1px solid #ff4846;"
                )}
   text])

(defn gen-row-markup [m]
  [:div {:style (str "border-bottom: 1px dotted gray;"
                     "padding: 0;"
                     "margin: 0;"
                     "width: 38em;")}
   (gen-cell "width: 4em;" (:uid m))
   (gen-cell "width: 18em;" (:name m))
   (gen-cell (str "width: 3em;"
                  "text-align: center;") (:unit m))
   (gen-cell (str "width: 5em;"
                  "text-align: right;")
             (let [amount (:amount m)]
               (if (and (number? amount)
                        (= (quot amount 1) amount))
                 (int amount)
                 amount)))
   (gen-cell (str "width: 8em;"
                  "text-align: right;")
             (if (number? (:value m))
               (let [[w _ d] (u/whole-sep-decimal (:value m))]
                 [:span w "."
                  [:span {:style "font-size: 12px; color: gray"}
                   d]])
               (:value m)))])





(defn gen-account-markup [acc acc-name]
  [:p {:style (str "margin-bottom: 1em;"
                   "padding-top: 1em;")}
   [:span {:style "width: 4em; display: inline-block;"} acc]
   [:b acc-name]])



(defn gen-owner-markup [row]
  [:p {:style (str "margin-top: -1em;"
                     "font-size: 1.2em;")}
   [:span "Materiale " (:owner-name row)]
   [:span (str "  ("(:owner-id row)")")]])



(defn gen-markup [^File month-dir]
  (let [s008 (->> (io/file month-dir s008-path)
                  (u/dbf-rows)
                  (into {} s008-xf))
        
        s215 (->> (io/file month-dir s215-path)
                  (u/dbf-rows)
                  (into [] s215-xf)
                  (group-by :account))]

    (concat
     ;; header
     [(u/gen-month-markup month-dir)
      (gen-owner-markup (first (val (first s215))))
      (gen-row-markup {:uid "Cod"
                       :name "Denumirea"
                       :unit "Unit."
                       :amount "Cant."
                       :value "Suma (lei)"})]
     ;; data       
     (->> accounts-to-gen
          (mapcat
           (fn [acc]
             (->> (get s215 acc)
                  (sort-by :name)
                  (map gen-row-markup)
                  (into [(gen-account-markup acc (s008 acc))]))))))))




(defn gen [^File month-dir & [out]]
  (let [out (or out (get-output-file month-dir))]
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
;; input box 3 10 124 42
;; total
