(use 'motosega.chains)

(def G1
  (build-grammar 
  '((:E -> ((:E :- :T) (:T :* :F) (:a) (:- :a)))
    (:T -> ((:T :* :F) (:a) (:- :a)))
    (:F -> ((:a) (:- :a))))
  '(:E :T :F)))

(time
 (chains G1
         :E    ; axiom
         2     ; context size
         100)  ; number of steps
 )
