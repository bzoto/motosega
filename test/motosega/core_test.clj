(ns motosega.core-test
  (:require [clojure.test :refer :all]
            [motosega.core :refer :all]
            [motosega.chains :refer :all]))


(def a-grammar (build-grammar 
                '((:E -> ((:E :- :T) (:T :* :F) (:a) (:- :a)))
                  (:T -> ((:T :* :F) (:a) (:- :a)))
                  (:F -> ((:a) (:- :a))))
                '(:E :T :F)))

(def a-sf `(:$ :$ :a ~(->Nonterm :C) :c :d ~(->Nonterm :D) :$ :$))


(deftest test-chains
  (testing "sf-contexts"
    (is (=
         {(->Nonterm :C) '(((:a)(:c)))
          (->Nonterm :D) '(((:d)(:$)))}
         (sf-contexts a-sf 1) 
         ))
    (is (=
         {(->Nonterm :C) '(((:$ :a)(:c :d))),
          (->Nonterm :D) '(((:c :d)(:$ :$)))}
         (sf-contexts a-sf 2)
         ))
    (is (=
         {}
         (sf-contexts a-sf 3)
         )))
  (testing "apply-rules"
    (is (=
         (apply-rules (list :a :b (->Nonterm :E) :c) a-grammar)
         `((:a :b ~(->Nonterm :E) :- ~(->Nonterm :T) :c)
           (:a :b ~(->Nonterm :T) :* ~(->Nonterm :F) :c)
           (:a :b :a :c)
           (:a :b :- :a :c))))
    (is (=
         (apply-rules (list :a :b :c) a-grammar)
         '()))
    )
  )
