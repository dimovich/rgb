(ns rgb.ui
  (:require [cljfx.api :as fx]
            [cljfx.ext.list-view :as fx.ext.list-view]
            [clojure.java.io :as io]
            [rgb.util :as u]
            [rgb.ipc21 :as ipc21]
            [rgb.creditori :as rc])

  (:gen-class))



(defonce *state (atom nil))



(defn alert [text]
  (fx/on-fx-thread
   (fx/create-component
    {:fx/type :dialog
     :showing true
     :dialog-pane {:fx/type :dialog-pane
                   :content-text text
                   :style {:-fx-font-size "16px"}
                   :button-types [:ok]}})))




(defn ipc21-screen-items [month-dir]
  (cond-> []
    (u/files-exist? month-dir ipc21/prereqs)
    (conj {:text "Creare"
           :fn (fn []
                 (ipc21/gen month-dir)
                 (alert "Documentul a fost creat."))})
    

    (u/files-exist? month-dir [ipc21/default-output-path])
    (conj {:text "Deschidere"
           :fn #(let [path (->> ipc21/default-output-path
                                (io/file month-dir)
                                (.getParent))]
                  (u/open-explorer path))})))




(defn creditori-screen-items [month-dir]
  (let [output-file (rc/get-output-file month-dir)]
    (cond-> []
      (u/files-exist? month-dir rc/prereqs)
      (conj {:text "Creare"
             :fn (fn []
                   (rc/gen month-dir)
                   (alert "Documentul a fost creat."))})

      (.exists output-file)
      (conj {:text "Deschidere"
             :fn (fn []
                   (u/open-explorer output-file))}))))



(defn default-screen-items [month-dir]
  [{:text "IPC21"
    :children-fn #(ipc21-screen-items month-dir)}
   {:text "Creditori"
    :children-fn #(creditori-screen-items month-dir)}])




(defn home-screen-items [paths]
  (->> (mapcat u/list-dirs paths)
       (filter #(re-find #"^\d+_\d+$" (.getName %)))
       (sort-by #(.getName %))
       (reverse)
       (mapv (fn [dir]
               {:children-fn #(default-screen-items dir)
                :text (.getName dir)}))))





(defn handler [event]
  (let [{:keys [event/type]} event]
    (case type
      ::list-click
      (when (< 1 (.getClickCount (:fx/event event)))
        (let [idx (.. (:fx/event event)
                      getSource getSelectionModel getSelectedIndex)]
          
          (swap! *state update :screen-stack
                 (fn [stack]
                   (let [current (peek stack)
                         item (get (:children current) idx)]
                     ;; run action
                     (if (:fn item)
                       (do
                         ((:fn item))
                         ;; refresh menu
                         (conj (pop stack)
                               (assoc current :children
                                      ((:children-fn current)))))
                       ;; switch menu
                       (if-let [cf (:children-fn item)]
                         (conj stack (assoc item :children (cf)))
                         stack)))))))
      

      ::screen-stack-click
      (swap! *state update :screen-stack subvec 0 (inc (:idx event))))))




#_(def actual-handler
    (-> handler
        (fx/wrap-co-effects {:state #(deref *state)})
        (fx/wrap-effects {:state (fn [state _] (reset! *state state))})))





(defn list-view [{:keys [items selection]}]
  {:fx/type fx.ext.list-view/with-selection-props
   :props {:selection-mode :single}
   :desc {:fx/type :list-view
          :cell-factory {:fx/cell-type :list-cell
                         :describe (fn [path] {:text path})}
          :on-mouse-clicked {:event/type ::list-click}
          :style {:-fx-font-size "16px"}
          :items items}})




(defn screen-stack-view [{:keys [coll]}]
  {:fx/type :h-box
   :spacing 5
   :children
   (->> (map-indexed
         (fn [idx item]
           {:fx/type :label
            :on-mouse-clicked {:event/type ::screen-stack-click
                               :idx idx}
            :style {:-fx-font-weight "bold"
                    :-fx-font-size "16px"}
            :text item})
         coll)
        (interpose {:fx/type :label
                    :text "/"
                    :style {:-fx-font-size "16px"}}))})





(defn rgb-app [{:keys [screen-stack] :as state}]
  {:fx/type :stage
   :showing true
   :title "Rapoarte Grossbux"
   :width 320
   :height 480
   :on-close-request (fn [_] (System/exit 0))
   :scene {:fx/type :scene
           :root {:fx/type :v-box
                  :spacing 15
                  :padding 5
                  :children
                  [{:fx/type screen-stack-view
                    :coll (map :text screen-stack)}
                             
                   {:fx/type list-view
                    :items (->> (:children (peek screen-stack))
                                (map :text))}]}}})



(defn initial-state [{:keys [paths]}]
  {:screen-stack [{:text "Perioade"
                   :children (home-screen-items paths)}]})



(defn start [config]
  (reset! *state (initial-state config))
  
  (fx/mount-renderer
   *state
   (fx/create-renderer
    :middleware (fx/wrap-map-desc assoc :fx/type rgb-app)
    :opts {:fx.opt/map-event-handler handler})))





(defn -main []
  (->> (clojure.edn/read-string (slurp "config.edn"))
       (start)))





(comment
  
  (->> (clojure.edn/read-string (slurp "config.edn"))
       (start))


  (.showAndWait
   (Alert. Alert$AlertType/INFORMATION))


  (fx/on-fx-thread
   (fx/create-component
    {:fx/type :dialog
     :showing true
     :dialog-pane {:fx/type :dialog-pane
                   :content-text "Documentul a fost creat."
                   :style {:-fx-font-size "16px"}
                   :button-types [:ok]}}))


  
  )



;; 
;; config
;; merging 2021_12 from server and contabil
;; 

;; on build completion
;; (javafx.application.Platform/exit)

;; (javafx.application.Platform/setImplicitExit true)


;; clj -T:build uber

