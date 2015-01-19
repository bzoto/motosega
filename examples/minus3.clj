(use 'motosega.chains)

(def G1
  (build-grammar 
   '((E -> ((T * a - E)(T * - a - E)(a - E)(- a - E)
            (T * a)(T * - a)(a)(- a)))
     (T -> ((T * a)(T * - a)(a)(- a))))
   '(E T)))



(def grammchains (grammatical-chains G1 'E 1 30))

(show-chains grammchains)


(show-conflicts
 (find-conflicts grammchains
                 (chains-as-set (chains G1 'E 1 100))
                 1))
