(use 'motosega.chains)

(def G1
  (build-grammar 
   '((E -> ((E - T) (T * F) (a) (M a)))
     (T -> ((T * F) (a) (M a)))
     (F -> ((a) (M a)))
     (M -> ((-) (M -))))
   '(E T M F)))


(def grammchains (grammatical-chains G1 'E 3 15))


(display-list-of-strings
 (set-of-sfs->list-of-strings
  grammchains
  ))

(show-conflicts
 (find-conflicts grammchains
                 (chains-as-set (chains G1 'E 3 100))
                 3))
