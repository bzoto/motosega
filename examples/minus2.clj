(use 'motosega.chains)

(def G1
  (build-grammar 
   '((E -> ((E - T) (T * F) (a) (M a)))
     (T -> ((T * F) (a) (M a)))
     (F -> ((a) (M a)))
     (M -> ((-) (M -))))
   '(E T M F)))


(def grammchains (grammatical-chains G1 'E 3 20 4))

(show-chains grammchains)

(time 
;;(show-conflicts (find-conflicts grammchains (chains-as-set (chains G1 'E 3 100)) 3))
(parallel-find-conflicts-4 grammchains (chains-as-set (chains G1 'E 3 100)) 3)
)
