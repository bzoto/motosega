(use 'motosega.chains)

(def G1
  (build-grammar 
   '((E -> ((E - a * T)(E - - a * T)(E - a)(E - - a)(a * T)(- a * T)(a)(- a)))
     (T -> ((a * T)(- a * T)(a)(- a))))
  '(E T)))



(def grammchains (grammatical-chains G1 'E 2 15))

(show-chains grammchains)


(show-conflicts
 (find-conflicts grammchains
                 (chains-as-set (chains G1 'E 2 100))
                 2))
