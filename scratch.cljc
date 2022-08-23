



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






(def s008-xf (comp (map (partial u/row-fields s008-fields))
                   (map (juxt :account :name))))

(def s215-xf (comp (map (partial u/row-fields s215-fields))
                   (filter (comp (partial = 3) :owner-id))))

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
     (map #(select-keys % [:uid :name :amount :value]))
     (sort-by :name))
