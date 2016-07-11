(ns okasaki-clojure.core)

;; //=================
;;    Ex. 2.2
;; //=================

(defn mk-tree [l v r]
  {:left l :val v :right r})

(defn is-member? [tree val]
  (if-let [x (:val tree)]
    (cond
      (x < val) (recur (:left tree) val)
      (x > val) (recur (:right tree) val)
      :else tree)
      false))

