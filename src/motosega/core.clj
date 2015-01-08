;; Motosega tool 
;; (C) MMXV by Matteo Pradella


(ns motosega.core
  (:gen-class))


(defn -main
  [& args]
  (println "Chainsaw 0.1")
  (load-file (first args)))

