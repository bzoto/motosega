;; ----------------------------
;; A dumb chain computer
;; -- Clojure version --
;; (c) by Matteo Pradella, MMXV
;; ----------------------------

(ns motosega.chains)
(require 'clojure.set)
(require 'clojure.string)
(use 'clojure.test)

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defrecord Nonterm [nonterm])
(defn Nonterm? [x] (instance? Nonterm x))

(set! *warn-on-reflection* true)

;; (defn Nonterm? [x] (and (vector? x)
;;                         (= (first x) :nonterm)))
;; (defn ->Nonterm [x] [:nonterm x])

(defn- drop-nt [lst]
  (filter #(not (Nonterm? %)) lst))


(defn build-grammar 
  "takes a grammar as a list of lists (e.g. ((A -> ((a A) (a))) ...) and returns a hash table of the rules"
  [gr nt]
  (loop [r gr
         G {}]
    (if (empty? r)
      G
      (let [rule (first r)]
        (recur
         (rest r)
         (assoc G
                (->Nonterm (first rule))
                (map (fn [t]
                       (map (fn [u]
                              (if (some #{u} nt)
                                (->Nonterm u)
                                u))
                            t))
                     (nth rule 2))))))))

(defn terminal-sf? 
  "is it a terminal sentential form?"
  [sf]
  (every? #(not (Nonterm? %)) sf))


(defn- before-k [lst i k]
  (for [x (range (- i k) i)]
    (nth lst x)))


(defn- after-k [lst i k]
  (for [x (range (+ i 1) (+ i k 1))]
    (nth lst x)))

(defn sf-contexts
  "returns the nonterminals and their k contexts, if all terminal, in a hash table"
  [sf k]
  (loop [x (first sf)
         L (rest sf)
         i 0
         res (transient {})
         ]
    (if (empty? L)
      (persistent! res)
      (recur (first L)
             (rest  L)
             (inc i)

             (if (Nonterm? x)
               (let [left  (before-k sf i k)
                     right (after-k sf i k)]
                 (if (and
                      (terminal-sf? left)
                      (terminal-sf? right))
                   (assoc! res x
                           (conj
                            (res x)
                            (list left right)))
                   res))
               res)))))

(defn apply-rules 
  "applies all the possible rules in G to the sentential form sf"
  [sf G]
  (loop [out  '()
         left '()
         right sf]
    (if (empty? right)
      out
      (let [[x & xs]  right]
        (recur (if (Nonterm? x)
                 (concat (map #(concat left % xs)
                              (G x))
                         out)
                 out)
               (concat left (list x))
               xs)))))




(defn- union-of-context-hashes [h1 h2]
  (loop [nts (keys h2)
         out (transient h1)]
    (if (empty? nts)
      (persistent! out)
      (let [nt (first nts)]
        (recur
         (rest nts)
         (assoc! out nt (clojure.set/union (out nt)(h2 nt))))))))


(defn border [k]
  (for [x (range k)] :#))


(defn get-contexts 
  "computes all the k-contexts it can find in 'steps' derivations"
  [G axiom k steps]
  (let [bord (border k)]
    (loop [sfs  (list (concat bord (list (->Nonterm axiom)) bord))
           ctxs { (->Nonterm axiom) #{(list bord bord)} }
           cnt  0]
      (if (or (empty? sfs)(== steps cnt))
        ctxs
        (let [x  (filter (fn [t] (not (terminal-sf? t)))
                         (apply-rules (first sfs) G))
              xs (rest sfs)]
          (recur
           (concat xs x)
           (if (empty? x)
             ctxs
             (union-of-context-hashes ctxs
                                      (reduce union-of-context-hashes (map #(sf-contexts % k) x))))
           (inc cnt)))))))



(defn- show-list-as-string [lst]
  (doseq [t lst]
    (print (name t))))

(defn- show-alternatives [lst-of-lst]
  (when-not (empty? lst-of-lst)
    (doseq [l (rest lst-of-lst)]
      (show-list-as-string l)
      (print " | "))
    (show-list-as-string (first lst-of-lst))))

(defn chains 
  "computes all the simple chains it can find with 'steps' derivation depth"
  [G axiom k steps]
  (let [contexts   (get-contexts G axiom k steps)
        nts        (keys contexts)
        bodys      (atom {})
        the-chains (atom {})
        ]
    (doseq [n nts]
      (let [bd (filter #(seq %) ;; idiomatic for not empty
                       (map drop-nt (G n)))]
        (swap! bodys #(assoc % n (set bd)))))

    (doseq [n nts]
      (doseq [c (contexts n)]
        (let [old (@the-chains c #{})]
          (swap! the-chains
                 #(assoc % c (clojure.set/union old (@bodys n)))))))

    (println "Simple chains:")
    (doseq [k @the-chains]
      (show-list-as-string (first (first k)))
      (print "[")
      (show-alternatives (nth k 1))
      (print "]")
      (show-list-as-string (nth (first k) 1))
      (println))

    @the-chains
    ))

(defn chains-as-set [chains-hash]
  (let [out (atom #{})]
    (doseq [[k v] chains-hash]
      (doseq  [body v]
        (swap! out 
               #(conj %
                      (concat 
                       (list (first k))
                       (list body)
                       (list (second k)))))))
    @out))

(declare newstuff+newchains)

(defn grammatical-chains 
  [G axiom k maxlength & bound]
  (let [bord (border k)]
    (loop [sfs  '()
           left '()
           right (doall
                  (concat bord (list (->Nonterm axiom)) bord))
           out   #{}
           ]

      (if (and (empty? sfs)
               (empty? right))
        out
        (if (empty? right)
          (recur (rest sfs)
                 '()
                 (first sfs)
                 out)
          (let [[x & xs]  right]
                (if (Nonterm? x)
                  (let [[newstuff newchains] (newstuff+newchains 
                                              left (G x)
                                              x xs maxlength bound)]
                    (recur (doall (concat sfs newstuff))
                           (doall (concat left (list x)))
                           xs
                           (clojure.set/union out newchains)))
                  (recur sfs
                         (doall (concat left (list x)))
                         xs
                         out))))))))


(defn newstuff+newchains [left right-parts x xs maxlen bound]
  (let  [newchains (transient #{})]
    (loop [ns right-parts
           newstuff '()]

      (if (empty? ns)
        [newstuff (persistent! newchains)]
        (let [newchain  (doall (concat left
                                (list ':<)
                                (first ns)
                                (list ':>) xs))
              nc (drop-nt newchain)
              lnc (count nc)]
          (when (or (empty? bound)
                    (<= (- maxlen (first bound))
                        lnc
                        maxlen))
            (conj! newchains nc))
          (recur (rest ns)
                 (if (<= lnc
                         maxlen)
                   (cons newchain newstuff)
                   newstuff)
                 ))))))



(defn three-factors [lst]
  (let [out (atom '())
        n   (dec (count lst))]
    (loop [i 1]
      (when (< i n)
        (loop [j (inc i)]
          (when (<= j n)
            (let [[a b] (split-at i lst)
                  [c d] (split-at (- j i) b)]
              (swap! out #(cons (list a c d) %))
              (recur (+ j 1)))))
        (recur (+ i 1))))
    @out))

(defn drop-brackets [lst]
  (filter #(not (some #{%} '(:< :>))) lst))


(defn h+ [lst k]
  (let [l1 (drop-brackets lst)]
    (if (< (count l1) k)
      '()
      (take-last k l1))))

(defn h- [lst k]
  (let [l1 (drop-brackets lst)]
    (if (< (count l1) k)
      '()
      (take k l1))))

(defn is-bracketed? [lst y]
  (loop [cur lst
         the-y y
         state -1]
    (cond
      (empty? cur)  (= state 2)
      (and (= :< (first cur))
           (== -1 state))
      (recur (rest cur) the-y 0)
      (and (= :< (first cur))
           (== 0 state))
      (recur (rest cur) the-y 0)
      (and (empty? the-y) 
           (= :> (first cur))
           (or (== 1 state)(== 2 state)))
      (recur (rest cur) the-y 2)
      (empty? the-y) false
      (and (= (first the-y)(first cur))
           (or (== 0 state)(== 1 state)))
      (recur (rest cur) (rest the-y) 1)
      :else false)))


(defn conflictual 
  "c is a chain, x[y]z is a simple chain"
  [c x y z h]
  (let [cc (three-factors (doall c))]
    (filter 
     (fn [fac]
       (let [[X Y Z] fac
             r
             (and
              (= x (h+ X h))
              (= z (h- Z h))
              (= (drop-brackets Y) y)
              (not (is-bracketed? Y y))
              (not (some #{(last X)}   '(:< :>)))
              (not (some #{(first Z)}  '(:< :>)))
              )]
         r))
     cc)))

(defn chain->string [ch]
  (clojure.string/join
   (map #(cond
           (= % :<) \[
           (= % :>) \]
           :else (name %))
        ch)))

(defn show-chains [cs]
  (doseq [c cs]
    (println
     (chain->string c))))



(defn show-conflicts [cf]
  (println "Conflicts:")
  (doseq [c cf]
    (let [[[l cc r] ch] 
          c]
      (show-list-as-string l)
      (print "[")
      (show-list-as-string cc)
      (print "]")
      (show-list-as-string r)
      (print " VS ")
      (println (chain->string ch))
      )))


(defn find-conflicts [the-chains simple-chains h]
  (for [c the-chains
        s simple-chains
        :when
        (let [[x y z] s
              confl (conflictual c x y z h)]
          (seq confl))
        ]
    (list s c)))

(defn set-partition
  "partitions a set in n subsets"
  [the-set n]
  (let [m (quot (count the-set) n)
        v (transient (vec  (repeat n nil)))]
    (loop [part 0
           curr 0
           s the-set]
      (when s
        (assoc! v part (conj (v part) (first s)))
        (if (and
             (>= (inc curr) m)
             (< part (dec n)))
          (recur (inc part) 0 (next s))
          (recur part (inc curr) (next s)))))
    (persistent! v)))

(defn parallel-find-conflicts-1 [the-chains simple-chains h]
  (let [num-proc (.availableProcessors (Runtime/getRuntime))
        schains (set-partition the-chains num-proc)
        agts    (doall (map #(agent %) schains))]
  
  (doseq [a agts]
    (send a #(find-conflicts % simple-chains h)))

  (doseq [a agts]
    (show-conflicts @a))))

(defn parallel-find-conflicts-2 [the-chains simple-chains h]
  (let [num-proc (.availableProcessors (Runtime/getRuntime))
        schains (set-partition the-chains num-proc)
        conf  (doall (pmap #(find-conflicts % simple-chains h) schains))]
    (doseq [c conf]
      (show-conflicts c))))

(defn parallel-find-conflicts-3 [the-chains simple-chains h]
  (let [conf  (doall (pmap #(find-conflicts the-chains [%] h) simple-chains))]
    (doseq [c conf]
      (show-conflicts c))))
    
(defn parallel-find-conflicts-4 [the-chains simple-chains h]
  (let [num-proc (.availableProcessors (Runtime/getRuntime))
        schains (set-partition the-chains num-proc)
        thr (map 
             (fn [s] (Thread. (fn []
                                (show-conflicts
                                 (find-conflicts s simple-chains h)))))
             schains)]
    (doseq [t thr]
      (.start t))

    (doseq [t thr]
      (.join t))))
