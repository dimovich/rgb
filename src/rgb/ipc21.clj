(ns rgb.ipc21
  (:require [clojure.java.io :as io]
            [hiccup.core :refer [html]]
            [clojure.string :as s]
            [clj-time.core :as ct]
            [clj-time.coerce :as ctc]
            [clj-time.format :as ctf]
            [rgb.util :as u])
  (:import [com.linuxense.javadbf DBFReader DBFRow]
           [java.io File])

  (:gen-class))


(set! *warn-on-reflection* true)


(def cota-0 #{15311 15312 15321
              15322 15332 15342
              155 159})

(def s795-path "ARM_02/S795.DBF")
(def s010-path "ARM_02/S010.DBF")
(def s900-path "S900.DBF")

(def prereqs [s795-path s010-path s900-path])


(def default-output-path "ARM_02/IPC21/IPC21.xml")



(def get-string #(.getString ^DBFRow %1 ^String %2))
(def get-double #(.getDouble ^DBFRow %1 ^String %2))
(def get-int #(.getInt ^DBFRow %1 ^String %2))
(def get-date
  #(-> (.getDate ^DBFRow %1 ^String %2)
       ctc/from-date
       (ct/from-time-zone (ct/time-zone-for-offset -3))))



(def out-fmt (ctf/formatter "dd.MM.yyyy"))
(def format-date #(ctf/unparse out-fmt %))


(def s900-fields
  {:account ["SSUK" get-string]
   :amount ["SUMAT" get-double]
   :uid ["INPZAK" get-int]})

(def account-keys
  {"24110" :salariu
   "53320" :asigurare-medicala
   "53420" :impozit-venit})



(def s010-fields
  {:division ["BRIG" get-int]
   :uid ["TBN" get-int]})

(def division-names
  {1 "5501" ;;ialoveni
   4 "5501"
   2 "2316" ;;cigirleni
   })

(def division-codes ["5501" "2316"])



(def s795-fields
  {:uid ["TBN" get-int]
   :firstname ["FIRSTNAME" (comp s/upper-case get-string)]
   :lastname ["SURNAME" (comp s/upper-case get-string)]
   :idnp ["IDNP" get-string]
   :payed ["PAYED" get-double]
   :cpas ["CPAS" get-string]
   :prof ["ID_PROF" get-string]
   :categ ["ID_KATEG" get-int]
   :fs-payed ["CALC_SF" get-double]
   :med-payed ["MED_LIST" get-double]
   :date-start ["DATA_BEG" get-date]
   :date-end ["DATA_END" get-date]})


(def pay-categ
  {161 :payed
   160 :payed
   15311 :med-payed})




(defn read-s795 [fname]
  (with-open [rdr (DBFReader. (io/input-stream fname))]
    (loop [row (.nextRow rdr)
           res {}]
      (if row
        (let [{:keys [uid firstname lastname] :as row-data}
              (u/row-fields row s795-fields)]
          (recur
           (.nextRow rdr)
           (->> (str lastname " " firstname)
                (assoc row-data :fullname)
                (update-in res [uid :payments] conj))))

        res))))




(defn read-s900
  "Payments (salariu, impozit, asigurare)."
  [fname]
  (with-open [rdr (DBFReader. (io/input-stream fname))]
    (loop [row (.nextRow rdr)
           res {}]
      (if row
        (let [{:keys [uid account amount] :as row-data}
              (u/row-fields row s900-fields)]
          (recur
           (.nextRow rdr)
           (if-let [akey (get account-keys account)]
             (assoc-in res [uid akey] amount)
             res)))

        res))))



(defn read-s010
  "Employee information (division...)."
  [fname]
  (with-open [rdr (DBFReader. (io/input-stream fname))]
    (loop [row (.nextRow rdr)
           res {}]
      (if row
        (let [{:keys [uid division] :as row-data} (u/row-fields row s010-fields)]
          (recur
           (.nextRow rdr)
           (assoc res uid {:division (division-names division)
                           :uid uid})))

        res))))




(defn sum-payments [ents k]
  (reduce-kv
   (fn [res _ v]
     (->> (:payments v)
          (map k)
          (reduce +)
          (+ res)))
   0 ents))


(defn sum-division-impozit [ents]
  (->> (vals ents)
       (filter :impozit-venit)
       (group-by :division)
       (reduce-kv
        (fn [res k v]
          (->> (map :impozit-venit v)
               (reduce +)
               u/round-double
               (assoc res k)))
        {})))


(defn gen-division-markup
  [{:keys [idx code impozit]}]
  [:row {:line (str idx)}
   [:c1 idx]
   [:c2 code]
   [:c3 impozit]
   [:c4 impozit]
   [:c5 0]])



(defn gen-payments-markup [payments]
  (loop [[ent & rst] payments
         line-idx 1
         ent-idx 1
         markup []]
    (if-not ent
      markup
      (->>
       [:row {:line (str line-idx)}
        [:c1 ent-idx]
        [:c2 (:fullname ent)]
        [:c3 (:idnp ent)]
        [:c4 (:cpas ent)]
        [:c5 (format-date (:date-start ent))]
        [:c6 (format-date (:date-end ent))]
        [:c7 (:categ ent)]
        [:c71
         (if (cota-0 (:categ ent))
           "0%" "24%")]
        [:c8 (:prof ent)]
        [:c9 (:payed ent)]
        (if (some-> (:med-payed ent) pos?)
          [:c10 (:med-payed ent)]
          "<c10/>")
        [:c11 (:fs-payed ent)]]

       (conj markup)

       (recur rst (inc line-idx)
              (if (= (:idnp ent)
                     (:idnp (first rst)))
                ent-idx (inc ent-idx)))))))





(defn gen-markup [ents config]
  (let [payments (->> ents
                      (reduce-kv
                       (fn [payments k v]
                         (into payments (:payments v)))
                       [])
                      (sort-by :fullname))

        sum-payed (u/round-double (sum-payments ents :payed))
        sum-med (u/round-double (sum-payments ents :med-payed))
        sum-fs (u/round-double (sum-payments ents :fs-payed))
        sum-am (u/round-double (* 0.09 sum-payed))

        division-impozit (sum-division-impozit ents)
        sum-impozit (u/round-double (reduce + (vals division-impozit)))]
    
    [:dec {"TypeName" "IPC21"}
     [:fiscCod
      [:fiscal (:fiscalcode config)]
      [:name (:name config)]
      [:cuatm (:cuatm config)]
      [:fisc (:fisc config)]
      [:caem (:caem config)]
      [:cnas (:cnas config)]
      [:email (:email config)]
      [:telefon (:phone config)]]
     [:peroidnalog
      [:datefisc (:period config)]]
     [:director
      [:director (:director config)]
      [:contabil (:contabil config)]]

     
     [:table1
      [:ds
       [:flag1 "1"]]
      [:row
       [:r11c4 sum-payed]
       [:r11c5 sum-impozit]
       [:r11c6 sum-am]
       "<r12c4/>" "<r12c5/>" "<r12c6/>"
       "<r21c4/>" "<r21c5/>"
       "<r22c4/>" "<r22c5/>"
       "<r31c4/>" "<r31c5/>"
       "<r32c4/>"
       "<r41c4/>" "<r41c5/>"
       "<r42c4/>" "<r42c5/>"
       "<r43c4/>" "<r43c5/>"
       "<r44c4/>" "<r44c5/>"
       "<r45c4/>" "<r45c5/>"
       "<r46c4/>" "<r46c5/>"
       "<r47c4/>" "<r47c5/>"
       "<r48c4/>" "<r48c5/>"
       "<r49c4/>" "<r49c5/>"
       "<r491c4/>" "<r491c5/>"
       "<r51c4/>" "<r51c5/>"
       "<r52c4/>" "<r52c5/>"
       "<r53c4/>" "<r53c5/>"
       "<r54c4/>" "<r54c5/>"
       "<r55c4/>" "<r55c5/>"
       "<r56c4/>" "<r56c5/>"
       [:r61c4 sum-payed]
       [:r61c5 sum-impozit]
       [:r61c6 sum-am]]]

     [:summContr sum-impozit]
     

     ;; Divisions
     ;;
     
     (into
      [:dinamicTable]
      (concat
       (map-indexed
        (fn [idx code]
          (gen-division-markup
           {:impozit (get division-impozit code)
            :code code
            :idx (inc idx)}))
        division-codes)
       
       [[:total
         [:tot1c3 sum-impozit]
         [:tot1c4 sum-impozit]
         [:tot1c5 0]]]))


     ;; Employees
     ;;
     
     (into
      [:dinamicTable2]
      (conj
       (gen-payments-markup payments)
       [:total
        [:tot2c9 sum-payed]
        [:tot2c10 sum-med]
        [:tot2c11 sum-fs]]))
     
     
     [:table2
      [:row
       [:r11ac9 0]
       [:r11ac11 0]
       [:r11bc9 sum-payed]
       [:r11bc10 sum-med]
       [:r11bc11 sum-fs]
       [:r12ac9 0]
       [:r12ac11 0]
       [:r12bc9 0]
       [:r12bc11 0]
       "<r13c9/>"
       "<r13c11/>"
       "<r14ac9/>"
       "<r14ac11/>"
       "<r14bc11/>"
       [:r15c11 0]
       [:r16c11 0]
       [:r2c10 0]
       [:r21c10 0]
       [:r22c10 0]
       [:r23c10 0]
       [:r24c10 0]
       "<r3c10/>"
       "<r31c10/>"
       "<r32c10/>"
       "<r33c10/>"
       "<r34c10/>"]]]))




(defn path-period [path-str]
  (let [date-str (last (re-seq #"\d{4}_\d{2}" path-str))
        in-fmt (ctf/formatter "yyyy_MM")
        period-fmt (ctf/formatter "MM/yyyy")
        date (ctf/parse in-fmt date-str)]
    
    (str "L/" (ctf/unparse period-fmt date))))




(def s092-fields
  {:name ["NAME" get-string]
   :data ["DATA" get-string]})

(def config-keys
  {"FISKALKOD" :fiscalcode
   "MYCPAS" :cnas
   "DIREKTOR" :director
   "GLBUX" :contabil
   "FISCLOC" :cuatm
   "KODCAEM" :caem
   "KODIFS" :fisc
   "MYEMAIL" :email
   "MYTELEFON" :phone
   "NFIRMA" :name})

(def config-set (set (keys config-keys)))


(defn read-company-config [month-path]
  (let [file (-> (.getParent (io/file month-path))
                 (io/file "S092.DBF"))]
    (with-open [rdr (DBFReader. (io/input-stream file))]
      (loop [row (.nextRow rdr)
             res {}]
        (if row
          (let [row-data (u/row-fields row s092-fields)]
            (recur
             (.nextRow rdr)
             (cond-> res
               (config-set (:name row-data))
               (assoc (config-keys (:name row-data))
                      (:data row-data)))))

          res)))))




(defn gen [month-dir & [out]]
  (let [s795 (read-s795 (io/file month-dir s795-path))
        s900 (read-s900 (io/file month-dir s900-path))
        s010 (read-s010 (io/file month-dir s010-path))
        
        ents (->> (merge-with merge s900 s010)
                  (u/merge-existing s795))
        
        config (-> (read-company-config month-dir)
                   (assoc :period (path-period
                                   (.getName ^File month-dir))))

        output-file (or out (io/file month-dir
                                     default-output-path))]

    (io/make-parents output-file)
    
    (println "writing " (str output-file))
    (->> (gen-markup ents config)
         html
         (spit output-file))))





(defn -main [month-path & [out]]
  (if (u/some-paths [month-path] prereqs)
    (gen (io/file month-path))
    (throw (Exception. (str "missing files in " month-path)))))


;;(-main "data/SAL2020/2021_09")





(comment

  {:c2 ["SURNAME" "FIRSTNAME"]
   :c3 ["IDNP"]
   :c4 ["CPAS"]
   :c5 :start-date
   :c6 :end-date
   :c7 161
   :c71 "24%"
   :c8 ["ID_PROF"]
   :c9 "PAYED"
   :c11 "CALC_SF"}



  '("TBN" "LINE_NO" "YEAR_NO" "MONTH_NO" "GODDOC" "MESDOC" "DATA_BEG" "DATA_END" "DAY_BL" "RABWEEK" "ID_KATEG" "ID_PROF" "PAYED" "MED_LISvT" "PF_TARIF" "CALC_PF" "SF_TARIF" "CALC_SF" "SURNAME" "FIRSTNAME" "FATHERNAME" "DATAROJD" "CPAS" "IDNP" "ID_VDOC" "PAS_NAME" "PAS_SERIA" "PAS_NOMER" "PAS_GIVED" "PAS_DATE" "NOMINPAK" "PAKET" "SBROS" "TKCC")


  )




;; clojure -X:uberjar :jar ipc21.jar :main-class rgb.ipc21

;; java -jar ipc21.jar data/SAL2020/2021_05
