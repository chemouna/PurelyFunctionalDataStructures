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

;;
;; Ex 2.2
;;

(defn insert [tree value]
  (if-let [x (:val tree)]
    (cond
      (< x value) (mk-tree (insert (:left tree) value) x (:right tree))
      (> x value) (mk-tree (:left tree) x (insert (:right tree) x))
      :else (throw (Exception. "Element already in tree")))
  (mk-tree nil value nil)))


;; (insert (mk-tree 4 5 7) 6)

;;
;; Ex 2.3
;;



