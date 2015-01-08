;; Motosega tool 
;; (C) MMXV by Matteo Pradella


(ns motosega.core
  (:gen-class))
(use 'clojure.java.io)

(defn -main
  [& args]
  (println "Motosega 0.1")
  (if (and (not (empty? args))
           (.exists (as-file (first args))))
    (load-file (first args))
    (do
      (print "Error: I need a file to work, not ")
      (println args))))

