(ns build
  (:require [clojure.tools.build.api :as b]))

;;(def lib 'rgb)
;;(def version (format "1.2.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def uber-file "target/rgb.jar")

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (println "compiling...")
  (b/compile-clj {:basis basis
                  :src-dirs ["src"]
                  :class-dir class-dir})
  (println "uberjaring...")
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis
           :main 'rgb.ui})

  (println "done!")
  ;;(javafx.application.Platform/exit)
  )
