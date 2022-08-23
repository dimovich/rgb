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
   :unit ["NME1" u/get-string]
   :amount ["NSHTUK" u/get-double]
   :value ["NSUMAD" u/get-double]})


(def s008-fields
  {:account ["SSU" u/get-string]
   :name ["NAME" u/get-string]})


(def accounts-to-gen
  ["21110" "21130" "21140" "21160"
   "21170" "21190" "21310"])


(defn gen [^File month-dir & [out]])





(comment

  

  )



;; S008.DBF (category id -> category name)
;; S215.DBF materiale


