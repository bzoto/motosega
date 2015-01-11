(use 'motosega.chains)

(def G1
  '((E -> ((E - T) (T * F) (a) (- a)))
    (T -> ((T * F) (a) (- a)))
    (F -> ((a) (- a)))))

(time
 (chains (build-grammar G1 '(E T F))
         'E    ; axiom
         2     ; context size
         100)  ; number of steps
 )
