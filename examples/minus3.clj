(use 'motosega.chains)

(def G1
  (build-grammar 
   '((E -> ((T * a - E)(T * - a - E)(a - E)(- a - E)
            (T * a)(T * - a)(a)(- a)))
     (T -> ((T * a)(T * - a)(a)(- a))))
   '(E T)))



(def grammchains (grammatical-chains G1 'E 1 24 7))

(show-chains grammchains)

(time  

 ;(show-conflicts (find-conflicts grammchains (chains-as-set (chains G1 'E 1 100)) 1))

 (parallel-find-conflicts-4 grammchains (chains-as-set (chains G1 'E 1 100)) 1)

)


;; tests with 24/7
;; 85s sequential
;; 83s pmap (XXX-2)
;; 35s threads (XXX-4)
