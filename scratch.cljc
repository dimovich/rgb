



(import '[com.linuxense.javadbf DBFReader])
(require '[clojure.java.io :as io]
         '[hiccup.core :refer [html]]
         '[hiccup.util :as hu]
         '[clojure.string :as s]
         '[clj-time.core :as ct]
         '[clj-time.coerce :as ctc]
         '[clj-time.format :as ctf]
         '[clojure.pprint :refer [cl-format]])




(def rdr (DBFReader. (io/input-stream "data/S795.DBF")))

(->> (range (.getFieldCount rdr))
     (map #(->> (.getField rdr %)
                (.getName))))

(.nextRecord rdr)

(def nxt (.nextRecord rdr))

(seq nxt)

(.close rdr)



(def row (.nextRow rdr))

(.getString row "DATA_BEG")








(let [in (str in "/ARM_02/S795.DBF")
      out (or out "IPC21.xml")]
  (if (.exists (io/file in))
    (generate in out)
    (throw (Exception. (str "missing " in)))))










;; compile / GraalVM / babashka


;; run from Grosbux


;; strings -f * | grep 4028
;; API?


strings -f * | grep "TVA12\.xml"







;; Raport Fertilizanti
;;


(def paths ["data/fert/2020_12/ARM_10/JS521100.DBF"
            "data/fert/2021_04/ARM_10/JS521100.DBF"
            "data/fert/2021_03/ARM_10/JS521100.DBF"
            "data/fert/2021_06/ARM_10/JS521100.DBF"
            "data/fert/2021_05/ARM_10/JS521100.DBF"
            "data/fert/2021_01/ARM_10/JS521100.DBF"
            "data/fert/2021_02/ARM_10/JS521100.DBF"])

(def outfmt (ctf/formatter "dd.MM.yy"))


(defn read-entities [fname]
  (with-open [rdr (DBFReader. (io/input-stream fname))]
    (loop [row (.nextRow rdr)
           res {}]
      (if row
        (let [uid (.getString row "NDOC")
              company (.getString row "NAMZAK")
              date (some-> (.getDate row "DATA")
                           ctc/from-date
                           (ct/from-time-zone (ct/time-zone-for-offset -3)))]
          (recur
           (.nextRow rdr)
           (if (and (not-empty uid) (not-empty company))
             (assoc res uid {:uid uid
                             :date date
                             :company company})
             res)))

        res))))



(def uids-company
  (reduce
   (fn [m path]
     (merge m (read-entities path)))
   {}
   paths))



(def uids
  [])


(->> (distinct uids)
     (reduce
      (fn [s uid]
        (let [item (get uids-company uid)]
          (-> (str s uid ",")
              (str (:company item) ",")
              (str (ctf/unparse outfmt (:date item)) "\n"))))
      "")
     (spit "uids-company.csv"))







;; strings -f * | grep 


;; LANG=ru_RU.UTF-8 wine program.exe

;; XL521100.DBF
;; JS521100.DBF

;; find . -name "*.DBF" -exec grep -l 1311 '{}' \;

;; 

find . -name "*.DBF" -exec grep -l TVA12 '{}' \;



(require '[rgb.ipc21 :as ipc21]
         '[rgb.util :as u])

(def employee-file "data/SAL2020/2021_09/ARM_02/S010.DBF")
(def payments-file "data/SAL2020/2021_09/S900.DBF")
(def s795-file "data/SAL2020/2021_09//ARM_02/S795.DBF")

(def s900 (ipc21/read-s900 payments-file))
(def s010 (ipc21/read-s010 employee-file))
(def s795 (ipc21/read-s795 s795-file))



(def ents
  (->> (merge-with merge s900 s010)
       (u/merge-existing s795)))



(ipc21/gen-markup ents (ipc21/path-period
                        "data/SAL2020/2021_09"))


(ipc21/-main "data/SAL2020/2021_09")


(map-indexed
 (fn [idx code]
   (gen-division-markup {:impozit (get division-impozit code)
                         :code code
                         :idx (inc idx)}))
 division-codes)








(ipc21/read-company-config "data/SAL2020/2021_09")











;; UI
;;



(require '[cljfx.api :as fx])

(def *state (atom {:title "RGB"}))



(defn title-input [{:keys [title]}]
  {:fx/type :text-field
   :on-text-changed #(swap! *state assoc :title %)
   :text title})



(defn root [{:keys [title]}]
  {:fx/type :stage
   :showing true
   :title title
   :width 400
   :height 280
   :scene {:fx/type :scene
           :root {:fx/type :v-box
                  :children [{:fx/type :label
                              :text "Title"}
                             {:fx/type title-input
                              :title title}]}}})




(def renderer (fx/create-renderer
               :middleware (fx/wrap-map-desc assoc :fx/type root )))

(fx/mount-renderer *state renderer)









(def *todo-state (atom {:by-id {1 {:text "RGB UI" :done false}
                                2 {:text "Eat" :done true}
                                3 {:text "Roll" :done true}}}))

(swap! *todo-state assoc-in [:by-id 4] {:text "Roll2" :done false})

(defn todo-view [{:keys [id text done]}]
  {:fx/type :h-box
   :spacing 5
   :padding 5
   :children [{:fx/type :check-box
               :selected done
               :on-selected-changed {:event/type ::set-done :id id}}
              {:fx/type :label
               :style {:-fx-text-fill (if done :grey :black)}
               :text text}]})


(defn todo-app [{:keys [by-id]}]
  {:fx/type :stage
   :showing true
   :title "RGB"
   :width 400
   :height 280
   :scene {:fx/type :scene
           :root {:fx/type :v-box
                  :children (->> by-id
                                 (mapv #(assoc (val %) :id (key %) :fx/type todo-view)))}}})



(defn map-event-handler [event]
  (case (:event/type event)
    ::set-done (swap! *todo-state assoc-in [:by-id (:id event) :done] (:fx/event event))))


(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc assoc :fx/type todo-app)))

(fx/mount-renderer
 *todo-state
 (fx/create-renderer
  :middleware (fx/wrap-map-desc assoc :fx/type todo-app)
  :opts {:fx.opt/map-event-handler map-event-handler}))





;;;;

;;(require '[clojure.core.cache :as cache])

(def *state
  (atom (fx/create-context {:title "RGB"})))

(defn root [{:keys [fx/context]}]
  {:fx/type :stage
   :showing true
   :scene {:fx/type :scene
           :root {:fx/type :h-box
                  :children [{:fx/type :label
                              :text (fx/sub-val context :title)}]}}})


(def renderer
  (fx/create-renderer
   :middleware (comp
                fx/wrap-context-desc
                (fx/wrap-map-desc (fn [_] {:fx/type root})))
   :opts {:fx.opt/type->lifecycle #(or (fx/keyword->lifecycle %)
                                       (fx/fn->lifecycle-with-context %))}))

(fx/mount-renderer *state renderer)


(fx/swap-context @*state assoc :title "RGB-UI")








(fx/on-fx-thread
  (fx/create-component
    {:fx/type :stage
     :showing true
     :scene {:fx/type :scene
             :root {:fx/type :stack-pane
                    :children [{:fx/type :rectangle
                                :width 200
                                :height 200
                                :fill :lightgray}
                               {:fx/type :label
                                :stack-pane/alignment :bottom-left
                                :stack-pane/margin 5
                                :text "bottom-left"}
                               {:fx/type :label
                                :stack-pane/alignment :top-right
                                :stack-pane/margin 5
                                :text "top-right"}]}}}))




(def *state (atom {:todos [{:text "UI" :done false}
                           {:text "Music" :done false}]}))


(defn handler [event]
  (let [{:keys [event/type text state]} event]
    (case type
      ::add-todo {:state (update state :todos conj {:text text :done false})})))


(def actual-handler
  (-> handler
      (fx/wrap-co-effects {:state #(deref *state)})
      (fx/wrap-effects {:state (fn [state _] (reset! *state state))})))


(actual-handler {:event/type ::add-todo
                 :text "Sleep"})


;; (.getText (.getTarget (:fx/event event)))






(set! *warn-on-reflection* false)


(require [clojure.tools.build.api :as b])

(b/compile-clj {:basis basis :src-dirs ["../gits/cljfx/src"] :class-dir class-dir})













;; Inventarierea


(import '[com.linuxense.javadbf DBFWriter DBFReader])
(require '[clojure.java.io :as io]
         '[clojure.string :as s]
         '[clj-time.core :as ct]
         '[clj-time.coerce :as ctc]
         '[clj-time.format :as ctf]
         '[clojure.pprint :refer [cl-format]])


(defn round-double9 [^Double x]
  (Double. ^String (cl-format nil "~,9f" x)))


(defn timestamp2 [dt]
  (str (+ (* 10000 (- (ct/year dt) 1990))
          (* 100 (ct/month dt))
          (ct/day dt))
       "."
       (-> (+ (/ (ct/hour dt) 24)
              (/ (ct/minute dt) (* 24 60))
              (/ (ct/second dt) (* 24 60 60)))
           double
           str
           (subs 2 11))))


(defn timestamp [dt]
  (round-double9
   (double
    (+ (+ (* 10000 (- (ct/year dt) 1990))
          (* 100 (ct/month dt))
          (ct/day dt))
       (+ (/ (ct/hour dt) 24)
          (/ (ct/minute dt) (* 24 60))
          (/ (ct/second dt) (* 24 60 60)))))))




;; 04/10/2025 350410.347513003


(defn timestamp-fn [start-dt]
  (let [dt (atom start-dt)]
    (fn []
      (-> (swap! dt #(ct/plus % (ct/seconds 2)))
          (timestamp)))))


(def csv-data (slurp "dump/inv/inv.csv"))

(def units {1 "bucati."
            16 "pachet"
            7 "metri"
            4 "kg    ."
            11 "perechi."})


(def data (->> (s/split-lines csv-data)
               (map #(s/split %1 #","))
               (map-indexed #(into [(+ 1870 %1)] %2))
               (map #(update %1 2 Integer/parseInt))
               (map #(update %1 3 Double/parseDouble))
               (map #(update %1 4 Double/parseDouble))))



(first data)
[1864 "Clame aparat legat" 1 "70" "1212.078"]




(defn get-fields [path]
  (with-open [rdr (DBFReader. (io/input-stream path))]
    (into-array com.linuxense.javadbf.DBFField
                (for [i (range (.getFieldCount rdr))]
                  (.getField rdr i)))))


(def s005-fields ["KOD" "NAME" "NAMER" "NAME_D" "ARTICUL2" "STRIHKOD" "GRMAT" "KD1" "NME1" "KD2" "NME2" "KFED" "VES1KG" "NORMA" "C_INP" "C_OUT" "C_OUT2" "C_OUT3" "C_OUT4" "DGODN" "SROKGODN" "SPZ" "SPP" "ACCIZ" "NDS" "MAXPRC" "TOVAR" "SBROS" "SERVICE" "WORKED" "KODCROS" "DUF"])

(def s005-path "dump/inv/S005.DBF")


(def path "dump/inv/S005-new.DBF")
(io/copy (io/file s005-path) (io/file path))

(def wtr (DBFWriter. (io/file path)))
;;(.setFields wtr (get-fields s005-path))

;; old data
#_(with-open [rdr (DBFReader. (io/input-stream s005-path))]
    (loop []
      (when-let [row (.nextRecord rdr)]
        (.addRecord wtr row)
        (recur))))


;; new data
(loop [data data]
  (when data
    (let [row (first data)
          m {"KOD" (get row 0)
             "NAME" (get row 1)
             "GRMAT" 1
             "KD1" (get row 2)
             "NME1" (units (get row 2))
             "KD2" 0
             "KFED" 0
             "C_INP" 0
             "C_OUT" 0
             "SROKGODN" 0
             "SPZ" 0
             "DUF" (ctc/to-sql-date (ct/now))}]
      (->> (into-array Object (map #(m %) s005-fields))
           (.addRecord wtr))
      (recur (next data)))))


(.close wtr)




(def s215-path "dump/inv/S215.DBF")
(def s215-fields ["SSU" "ACTIV" "KODZAK" "SPP" "KODVAL" "KODMAT" "NAME_D" "NAME" "NAMZAK" "KODFSK" "NAMSPP" "NAMSSU" "NME1" "NME2" "KFC1" "NSUMAD" "NSUMAK" "NSHTUK" "NSHTU2" "KSUMAD" "KSUMAK" "KSHTUK" "KSHTU2" "RUB_DBT" "RUB_KDT" "SHT_DBT" "SHT_KDT" "SH2_DBT" "SH2_KDT" "NSUMAV" "ODSUMAV" "OKSUMAV" "KSUMAV" "KURS" "INPSUM" "INPKOL" "CEKSUM" "CEKKOL" "TEKKOL" "DIFKOL" "CENAM" "NDOCN" "TEXTN" "TEXTLAST" "NDOC" "SBROS" "DATADOCN" "DUF_"])


(def path "dump/inv/S215-new.DBF")
(io/copy (io/file s215-path) (io/file path))

(def wtr (DBFWriter. (io/file path)))
;;(.setFields wtr (get-fields s215-path))

;; old
#_(with-open [rdr (DBFReader. (io/input-stream s215-path))]
    (loop []
      (when-let [row (.nextRecord rdr)]
        (.addRecord wtr row)
        (recur))))

;; new
(loop [data data]
  (when data
    (let [row (first data)
          m {"SSU" "21710"
             "KODZAK" 1311
             "KODMAT" (get row 0)
             "NAME" (get row 1)
             "NME1" (units (get row 2))
             "KFC1" 0
             "KSUMAD" (get row 4)
             "KSUMAK" 0
             "KSHTUK" (get row 3)
             "RUB_DBT" (get row 4)
             "RUB_KDT" 0
             "SHT_DBT" (get row 3)
             "SHT_KDT" 0
             "DUF_" (ctc/to-sql-date (ct/now))}]
      (->> (into-array Object (map #(m %) s215-fields))
           (.addRecord wtr))
      (recur (next data)))))

(.close wtr)






(def s900-path "dump/inv/S900.DBF")
(def s900-fields ["NDOC" "SSUD" "SSUK" "NAME" "NOMDOG" "SIF" "SPZ" "SPP" "SPPKR" "TYP_DOC" "DATA_BAN" "DATA" "SHTU2" "INPZAK" "OUTZAK" "INVN" "KODMAT" "KODVAL" "PROVD" "SHTUK" "SUMAT" "SSUD1" "SSUK1" "ZAKD1" "ZAKK1" "SUMA1" "SSUD2" "SSUK2" "ZAKD2" "ZAKK2" "SUMA2" "SSUD3" "SSUK3" "ZAKD3" "ZAKK3" "SUMA3" "SSUD4" "SSUK4" "ZAKD4" "ZAKK4" "SUMA4" "ARM" "DOPK" "DOPKQ" "DOPKU" "MANAGER" "OPERATOR" "DOC_REG" "DOC_ZAR" "DUF" "IDN" "DMOD" "DOCID"])



(def ts (timestamp-fn (ct/now)))

(def path "dump/inv/S900-new.DBF")
(io/copy (io/file s900-path) (io/file path))

(def wtr (DBFWriter. (io/file path)))
;;(.setFields wtr (get-fields s900-path))

;; old
#_(with-open [rdr (DBFReader. (io/input-stream s900-path))]
    (loop []
      (when-let [row (.nextRecord rdr)]
        (.addRecord wtr row)
        (recur))))


(def idn-list (atom []))

;; new
(loop [data data]
  (when data
    (let [row (first data)
          idn (ts)
          m {"NDOC" "transmitere"
             "SSUD" "21710"
             "SSUK" "21720"
             "NAME" "universala"
             "SPZ" 0
             "SPP" 0
             "TYP_DOC" 0
             "DATA_BAN" (ctc/to-sql-date (ct/now))
             "DATA" (ctc/to-sql-date (ct/now))
             "SHTU2" 0
             "INPZAK" 1311
             "OUTZAK" 0
             "INVN" 0
             "KODMAT" (get row 0)
             "KODVAL" 0
             "PROVD" 1
             "SHTUK" (get row 3)
             "SUMAT" (get row 4)
             "SSUD1" "21710"
             "SSUK1" "21720"
             "ZAKD1" 1311
             "ZAKK1" 0
             "SUMA1" (get row 4)
             "SSUD2" "00000"
             "SSUK2" "00000"
             "ZAKD2" 0
             "ZAKK2" 0
             "SUMA2" 0
             "SSUD3" "00000"
             "SSUK3" "00000"
             "ZAKD3" 0
             "ZAKK3" 0
             "SUMA3" 0
             "SSUD4" "00000"
             "SSUK4" "00000"
             "ZAKD4" 0
             "ZAKK4" 0
             "SUMA4" 0
             "ARM" 10
             "DOPK" 0
             "DOPKQ" 0
             "DOPKU" 0
             "MANAGER" 0
             "DUF" (ctc/to-sql-date (ct/now))
             "IDN" idn
             "DMOD" idn}]
      (swap! idn-list conj idn)
      (->> (into-array Object (map #(m %) s900-fields))
           (.addRecord wtr))
      (recur (next data)))))


(.close wtr)






(def s902-path "dump/inv/S902.DBF")
(def s902-fields ["IDN" "DMOD"])

(def path "dump/inv/S902-new.DBF")
(io/copy (io/file s902-path) (io/file path))

(with-open [wtr (DBFWriter. (io/file path))]
 (loop [[idn & rst] @idn-list]
   (when idn
     (let [m {"IDN" idn
              "DMOD" idn}]
       (->> (into-array Object (map #(m %) s902-fields))
            (.addRecord wtr))
       (recur rst)))))



















;; KOD,N,6,0	NAME,C,50	NAMER,C,50	NAME_D,C,20	ARTICUL2,C,20	STRIHKOD,C,20	GRMAT,N,6,0	KD1,N,3,0	NME1,C,10	KD2,N,3,0	NME2,C,10	KFED,N,14,6	VES1KG,N,10,4	NORMA,N,16,8	C_INP,N,19,4	C_OUT,N,19,4	C_OUT2,N,19,4	C_OUT3,N,19,4	C_OUT4,N,19,4	DGODN,D	SROKGODN,N,8,0	SPZ,N,6,0	SPP,N,5,0	ACCIZ,N,10,4	NDS,N,2,0	MAXPRC,N,6,2	TOVAR,L	SBROS,L	SERVICE,L	WORKED,L	KODCROS,N,6,0	DUF,D




(comment
  (def encoding "CP866")
  (def S005 (DBFWriter. (io/file S005-path) encoding))

  (import '[com.linuxense.javadbf DBFUtils]
          '[java.nio.charset StandardCharsets])

  (->> (DBFUtils/doubleFormating (ts) StandardCharsets/ISO_8859_1 16 9)
       (map char)))
















;; Font Path bug
;; - run on windows (try leiningen on XP)





;; AOT
;; :jvm-opts ["-Dcljfx.skip-javafx-initialization=true"]


;; https://docs.gluonhq.com/#_gluonfx_plugin_for_maven


;; babashka


;; try compiling on a VPS
;; clj-easy, how to use the :native alias (read until the end)



;; Tcl/Tk
;; https://freewrap.dengensys.com/
;; wkhtmltopdf.org
;; wxwidgets
;; https://glade.gnome.org/



;; https://old.reddit.com/r/Clojure/comments/fw93gc/new_clojurians_ask_anything/fmvnq4q/
;; https://vlaaad.github.io/year-of-clojure-on-the-desktop







;; find . -name "*.DBF" -exec grep -l Botryx '{}' \;






