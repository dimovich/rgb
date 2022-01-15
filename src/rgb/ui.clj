(ns rgb.ui
  (:require [cljfx.api :as fx]
            [cljfx.ext.list-view :as fx.ext.list-view]
            [clojure.java.io :as io]
            [rgb.util :as u]
            [rgb.ipc21 :as ipc21]
            [rgb.creditori :as rc])

  (:import [java.io File]
           [javafx.scene.control ListView]
           [javafx.scene.input MouseEvent KeyEvent KeyCode])

  (:gen-class))

(set! *warn-on-reflection* true)


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




(defn ipc21-screen-items [dir-paths]
  (let [month-dir (u/some-paths dir-paths ipc21/prereqs)
        output-file (io/file month-dir ipc21/default-output-path)]
    (cond-> []
      month-dir
      (conj {:text "Creare"
             :fn (fn []
                   (ipc21/gen month-dir)
                   (alert "Documentul a fost creat."))})
    

      (.exists ^File output-file)
      (conj {:text "Deschidere"
             :fn #(u/open-explorer (.getParent output-file))}))))




(defn creditori-screen-items [dir-paths]
  (when-let [month-dir (u/some-paths dir-paths rc/prereqs)]
    (let [output-file (rc/get-output-file month-dir)]
      (cond-> []
        month-dir
        (conj {:text "Creare"
               :fn (fn []
                     (rc/gen month-dir)
                     (alert "Documentul a fost creat."))})

        (.exists ^File output-file)
        (conj {:text "Deschidere"
               :fn (fn []
                     (u/open-explorer output-file))})))))




(defn default-screen-items [dir-paths]
  [{:text "IPC21"
    :children-fn #(ipc21-screen-items dir-paths)}
   {:text "Creditori"
    :children-fn #(creditori-screen-items dir-paths)}])




(defn home-screen-items [paths]
  (->> (mapcat u/list-dirs paths)
       (filter #(re-find #"^\d+_\d+$" (.getName ^File %)))
       (group-by #(.getName ^File %))
       (map (fn [[name dir-paths]]
              {:children-fn #(default-screen-items dir-paths)
               :text name}))
       (sort-by :text)
       (reverse)
       vec))



(defn handle-forward [idx]
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
                 (let [children (cf)]
                   (conj stack (assoc item :children
                                      (->> children
                                           (remove #(when-let [cf2 (:children-fn %)]
                                                      (empty? (cf2))))
                                           vec))))
                 stack))))))



(defn handle-back [idx]
  (swap! *state update :screen-stack
         (fn [stack]
           (if (neg? idx)
             (if (< 1 (count stack))
               (pop stack)
               stack)
             (subvec stack 0 idx)))))



(defn handler [ev]
  (let [{:keys [event/type]} ev]
    (case type
      ::list-key
      (let [event ^KeyEvent (:fx/event ev)
            code (.getCode event)]
        (cond
          (= code KeyCode/ENTER)
          (let [target ^ListView (.getSource event)
                idx (.. target getSelectionModel getSelectedIndex)]
            (handle-forward idx))

          (#{KeyCode/LEFT KeyCode/BACK_SPACE} code)
          (handle-back -1)))
      
      ::list-click
      (let [event ^MouseEvent (:fx/event ev)]
        (when (< 1 (.getClickCount event))
          (let [target ^ListView (.getSource event)
                idx (.. target getSelectionModel getSelectedIndex)]
            (handle-forward idx))))
      

      ::screen-stack-click
      (handle-back (inc (:idx ev))))))




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
          :on-key-pressed {:event/type ::list-key}
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





(defn rgb-app [{{:keys [screen-stack]} :state}]
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
  
  (let [renderer
        (fx/create-renderer
         :middleware (fx/wrap-map-desc
                      (fn [state] {:fx/type rgb-app :state state}))
         :opts {:fx.opt/map-event-handler handler})]
    (fx/mount-renderer *state renderer)
    renderer))





(defn -main []
  (->> (clojure.edn/read-string (slurp "config.edn"))
       (start)))





(comment
  
  (def renderer
    (->> (clojure.edn/read-string (slurp "config.edn"))
         (start)))

  (renderer)
  

  )






;; 
;; config
;; merging 2021_12 from server and contabil
;; 

;; on build completion
;; (javafx.application.Platform/exit)

;; (javafx.application.Platform/setImplicitExit true)


;; clj -T:build uber

