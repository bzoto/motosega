(use 'motosega.chains)

(def G1
'((S -> ((X) (Y)))
  (X -> ((E s X) (E s E)))
  (Y -> ((Y s E) (E s E)))
  (E -> ((e)))))


(chains (build-grammar G1 '(S X Y E)) 
        'S    ; axiom
        3     ; context size 
        100)  ; number of steps
