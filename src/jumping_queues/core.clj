(ns jumping-queues.core

  "Clojure implementation of the real-time persistent deque of Kaplan
  & Tarjan's \"Purely Functional, Real-Time Deques with Catenation\".

  Here real-time means that all operations have O(1) worst-case time
  complexity. The deque is reversible via clojure.core/rseq.

  Use with clojure.core functions:

    1. first / next operate from the left;

    2. conj / peek / pop operate from the left (as with stacks);

    3. rseq flips the deque."

  {:author "Michał Marczyk"}

  (:refer-clojure :exclude [peek pop])
  (:require (jumping-queues.impl #_[queue   :as q]
                                 [ndeque  :as nd]
                                 #_[nsteque :as ns]
                                 #_[steque  :as s]
                                 #_[deque   :as d]
                                 interfaces))
  (:import (jumping_queues.impl.interfaces Left Right QCat)))

(defn peek
  "Returns the leftmost element of q.

  On supported types, equivalent to clojure.core/peek."
  [q]
  (.jqPeek ^Left q))

(defn pop
  "Removes the leftmost element of q.

  On supported types, equivalent to clojure.core/pop."
  [q]
  (.jqPop ^Left q))

(defn push
  "Adds x to q from the left.

  On supported types, equivalent to clojure.core/conj."
  ([q x]
     (.jqPush ^Left q x))
  ([q x & xs]
     (reduce push (push q x) xs)))

(defn inspect
  "Returns the rightmost element of q."
  [q]
  (.jqInspect ^Right q))

(defn eject
  "Removes the rightmost element of q."
  [q]
  (.jqEject ^Right q))

(defn inject
  "Adds x to q from the right."
  ([q x]
     (.jqInject ^Right q x))
  ([q x & xs]
     (reduce inject (inject q x) xs)))

(defn qcat
  "Concatenates the given qs which must of the same catenable
  queue-like type."
  ([q1 q2]
     (.jqCat ^QCat q1 q2))
  ([q1 q2 & qn]
     (reduce qcat (qcat q1 q2) qn)))

#_
(defn nqueue
  "Constructs a non-catenable persistent queue holding the contents of
  coll."
  [coll]
  (reduce inject q/empty coll))

#_
(defn nsteque
  "Constructs a non-catenable persistent steque holding the contents
  of coll."
  [coll]
  (reduce inject ns/empty coll))

(defn ndeque
  "Constructs a non-catenable persistent deque holding the contents of
  coll."
  [coll]
  (reduce inject nd/empty coll))

#_
(defn steque
  "Constructs a catenable persistent steque holding the contents of
  coll."
  [coll]
  (reduce inject s/empty coll))

#_
(defn deque
  "Constructs a catenable persistent deque holding the contents of
  coll."
  [coll]
  (reduce inject d/empty coll))

#_
(defn minnqueue
  "Constructs a non-catenable persistent minqueue holding the contents
  of coll. comp is the Comparator to be used, clojure.core/compare by
  default."
  ([coll]
     (minnqueue compare coll))
  ([comp coll]
     (reduce inject (q/minempty comp) coll)))

#_
(defn minnsteque
  "Constructs a non-catenable persistent minsteque holding the
  contents of coll. comp is the Comparator to be used,
  clojure.core/compare by default."
  ([coll]
     (minnsteque compare coll))
  ([comp coll]
     (reduce inject (ns/minempty comp) coll)))

#_
(defn minndeque
  "Constructs a non-catenable persistent deque holding the contents of
  coll. comp is the Comparator to be used, clojure.core/compare by
  default."
  ([coll]
     (minndeque compare coll))
  ([comp coll]
     (reduce inject (nd/minempty comp) coll)))

#_
(defn minsteque
  "Constructs a catenable persistent steque holding the contents of
  coll. comp is the Comparator to be used, clojure.core/compare by
  default."
  ([coll]
     (minsteque compare coll))
  ([comp coll]
     (reduce inject (s/minempty comp) coll)))

#_
(defn mindeque
  "Constructs a catenable persistent deque holding the contents of
  coll. comp is the Comparator to be used, clojure.core/compare by
  default."
  ([coll]
     (mindeque compare coll))
  ([comp coll]
     (reduce inject (d/minempty comp) coll)))
