(use 'motosega.chains)

(def G1
  (build-grammar 
  '((E -> ((E - T) (T * F) (a) (- a)))
    (T -> ((T * F) (a) (- a)))
    (F -> ((a) (- a))))
  '(E T F)))



(def grammchains (grammatical-chains G1 'E 2 10))

(show-chains grammchains)

(show-conflicts (find-conflicts grammchains (chains-as-set (chains G1 'E 2 100)) 2))

;;(parallel-find-conflicts-4 grammchains (chains-as-set (chains G1 'E 2 100)) 2)
