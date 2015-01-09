;; ----------------------------
;; A dumb chain computer
;; Clojure version
;; (c) by Matteo Pradella, MMXV
;; ----------------------------

(ns motosega.chains)
(require 'clojure.set)



(defrecord Nonterm [symb])

(defn nonterm [s] (Nonterm. s))


(defn build-grammar [gr nt]
  (loop [r gr
         G {}]
    (if (empty? r)
      G
      (let [rule (first r)]
        (recur
          (rest r)
          (assoc G
                 (Nonterm. (first rule))
                 (map (fn [t]
                        (map (fn [u]
                               (if (some #{u} nt)
                                 (Nonterm. u)
                                 u))
                             t))
                      (nth rule 2))))))))

(defn- terminal-sf? [sf]
  (every? #(not (instance? Nonterm %)) sf))


(defn- before-k [lst i k]
  (for [x (range (- i k) i)]
    (nth lst x)))


(defn- after-k [lst i k]
  (for [x (range (+ i 1) (+ i k 1))]
    (nth lst x)))

(defn- sf-contexts
  "returns nonterminal + context, if all terminal"
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

             (if (instance? Nonterm x)
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

(defn- apply-rules [sf G]
  (loop [out  '()
         left '()
         right sf]
    (if (empty? right)
      out
      (let [[x & xs]  right]
        (recur (if (instance? Nonterm x)
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



(defn- get-contexts [G axiom k steps]
  (let [bord (for [x (range k)] '$)]
    (loop [sfs  (list (concat bord (list (nonterm axiom)) bord))
           ctxs { (nonterm axiom) #{(list bord bord)}}
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


(defn- drop-nt [lst]
  (filter #(not (instance? Nonterm %)) lst))

(defn- show-list-as-string [lst]
  (doseq [t lst]
    (print t)))

(defn- show-alternatives [lst-of-lst]
  (when-not (empty? lst-of-lst)
    (doseq [l (rest lst-of-lst)]
      (show-list-as-string l)
      (print " | "))
    (show-list-as-string (first lst-of-lst))))

(defn chains [G axiom k steps]
  (let [contexts   (get-contexts G axiom k steps)
        nts        (keys contexts)
        bodys      (atom {})
        the-chains (atom {})
        ]
    (doseq [n nts]
      (let [bd (filter #(not (empty? %))
                       (map drop-nt (G n)))]
        (swap! bodys #(assoc % n (set bd)))))

    (doseq [n nts]
      (doseq [c (contexts n)]
        (let [old (@the-chains c)
              old (or old #{})]
          (swap! the-chains
                 #(assoc % c (clojure.set/union old (@bodys n)))))))

    (doseq [k @the-chains]
      (show-list-as-string (first (first k)))
      (print "[")
      (show-alternatives (nth k 1))
      (print "]")
      (show-list-as-string (nth (first k) 1))
      (println))

    @the-chains
    ))


