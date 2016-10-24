module queue

import Data.Vector


abstract
data Queue : Type -> Type where
  mkQueue : LTE bsize fsize
          -> (front : Vect fsize)
          -> (back : Vect bsize)
          -> Queue a

public empty : Queue a
empty = mkQueue LTEZero [] []

reassocLemma : Vect ((l + r) + S k) -> Vect ((l + S r) + k) a
reassocLemma = ?reassocLemmaProof


