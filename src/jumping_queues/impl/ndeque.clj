(ns jumping-queues.impl.ndeque
  (:refer-clojure :exclude [empty])
  (:require jumping-queues.impl.interfaces)
  (:import (jumping_queues.impl.interfaces Left Right)
           (clojure.lang Util)))

(deftype Pair [car cdr])

(defmethod print-method Pair [^Pair p ^java.io.Writer w]
  (.write w "#<Pair: (")
  (.write w (pr-str (.-car p)))
  (.write w " . ")
  (.write w (pr-str (.-cdr p)))
  (.write w ")>"))

(defn buffer
  ([x]
     (doto (object-array 1)
       (aset 0 x)))
  ([x y]
     (doto (object-array 2)
       (aset 0 x)
       (aset 1 y))))

(defn bpeek [b]
  (aget ^objects b 0))

(defn bpush [b x]
  (if (nil? b)
    (buffer x)
    (let [len (alength b)]
      (if (< len 5)
        (let [b' (object-array (inc len))]
          (aset b' 0 x)
          (System/arraycopy b 0 b' 1 len)
          b')
        (throw (ex-info "ndeque buffer overflow when pushing" {}))))))

(defn bpop [b]
  (let [len (alength b)]
    (cond
      (== len 1)
      nil

      (> len 1)
      (let [b' (object-array (dec len))]
        (System/arraycopy b 1 b' 0 (dec len))
        b')

      :else
      (throw (ex-info "ndeque buffer underflow when popping" {})))))

(defn binspect [b]
  (aget ^objects b (unchecked-dec-int (alength b))))

(defn binject [b x]
  (if (nil? b)
    (buffer x)
    (let [len (alength b)]
      (if (< len 5)
        (let [b' (object-array (inc len))]
          (System/arraycopy b 0 b' 0 len)
          (aset b' len x)
          b')
        (throw (ex-info "ndeque buffer overflow when injecting" {}))))))

(defn beject [b]
  (let [len (alength b)]
    (cond
      (== len 1)
      nil

      (> len 1)
      (let [b' (object-array (dec len))]
        (System/arraycopy b 0 b' 0 (dec len))
        b')

      :else
      (throw (ex-info "ndeque buffer underflow when ejecting")))))

(defn blength [b]
  (if b (alength b) 0))

(defn bpop2 [b]
  (let [len     (alength b)
        new-len (- len 2)
        b'      (object-array new-len)]
    (System/arraycopy b 2 b' 0 new-len)
    b'))

(defn beject2 [b]
  (let [len     (alength b)
        new-len (- len 2)
        b'      (object-array new-len)]
    (System/arraycopy b 0 b' 0 new-len)
    b'))

(defn bpeek2 [b]
  (->Pair (aget ^objects b 0)
          (aget ^objects b 1)))

(defn binspect2 [b]
  (let [len (alength b)]
    (->Pair (aget ^objects b (- len 2))
            (aget ^objects b (dec len)))))

(defn bpush2 [b ^Pair p]
  (if (nil? b)
    (buffer (.-car p) (.-cdr p))
    (let [len     (alength b)
          new-len (+ len 2)
          b'      (object-array new-len)]
      (System/arraycopy b 0 b' 2 len)
      (aset ^objects b' 0 (.-car p))
      (aset ^objects b' 1 (.-cdr p))
      b')))

(defn binject2 [b ^Pair p]
  (if (nil? b)
    (buffer (.-car p) (.-cdr p))
    (let [len     (alength b)
          new-len (+ len 2)
          b'      (object-array new-len)]
      (System/arraycopy b 0 b' 0 len)
      (aset ^objects b' len (.-car p))
      (aset ^objects b' (inc len) (.-cdr p))
      b')))

(deftype Node [color prefix left right suffix])

(defn color [n]
  (case n
    (0 5) :red
    (1 4) :yellow
    (2 3) :green))

(defn next-color [n m above-bottom?]
  (cond
       (zero? n) (if above-bottom? :red (color m))
       (zero? m) (if above-bottom? :red (color n))
       :else
       (let [n (if (> n 2) (- 5 n) n)
             m (if (> m 2) (- 5 m) m)]
         (color (min n m)))))

(defn ^Node node [prefix left right suffix]
  (assert (or (nil? right) (#{:green :red} (.-color ^Node right))))
  (cond
    (and prefix suffix)
    (let [pcol (color (blength prefix))
          scol (color (blength suffix))
          col  (cond
                 (or (identical? :red pcol)
                     (identical? :red scol))
                 :red

                 (or (identical? :yellow pcol)
                     (identical? :yellow scol))
                 :yellow

                 :else
                 :green)]
      (->Node col prefix left right suffix))

    (and (not left) (not right))
    (if prefix
      (->Node (color (blength prefix)) prefix nil nil nil)
      (->Node (color (blength suffix)) nil nil nil suffix))

    :else
    (->Node :red prefix left right suffix)))

(defn red? [^Node stack]
  (identical? :red (.-color stack)))

(defn yellow? [^Node stack]
  (identical? :yellow (.-color stack)))

(defn green? [^Node stack]
  (identical? :green (.-color stack)))

(defn regular? [^Node stack]
  (not (or (red? stack)
           (and (yellow? stack)
                (.-right stack)
                (red? (.-right stack))))))

(defn restore [^Node stack]
  (if (red? stack)
    (let [left? (.-left stack)
          next-stack (if left?
                       (.-left stack)
                       (.-right stack))
          p  (.-prefix stack)
          s  (.-suffix stack)
          pn (if next-stack (.-prefix next-stack))
          sn (if next-stack (.-suffix next-stack))
          plen  (blength p)
          slen  (blength s)
          pnlen (blength pn)
          snlen (blength sn)]
      (cond
        (>= (+ pnlen snlen) 2)
        (let [empty-pn? (zero? pnlen)
              empty-sn? (zero? snlen)
              pn (if empty-pn?
                   (buffer (bpeek sn))
                   pn)
              sn (cond
                   empty-sn? (buffer (binspect pn))
                   empty-pn? (bpop sn)
                   :else     sn)
              pn (if empty-sn?
                   (beject pn)
                   pn)
              large-p? (>= plen 4)
              large-s? (>= slen 4)
              small-p? (<= plen 1)
              small-s? (<= slen 1)
              pn (if large-p?
                   (bpush pn (binspect2 p))
                   pn)
              p  (if large-p?
                   (beject2 p)
                   p)
              p  (if small-p?
                   (binject2 p (bpeek pn))
                   p)
              pn (if small-p?
                   (bpop pn)
                   pn)
              sn (if large-s?
                   (binject sn (bpeek2 s))
                   sn)
              s  (if large-s?
                   (bpop2 s)
                   s)
              s  (if small-s?
                   (bpush2 s (binspect sn))
                   s)
              sn (if small-s?
                   (beject sn)
                   sn)
              nnext-stack (if (.-left next-stack)
                            (.-left next-stack)
                            (.-right (if left? stack next-stack)))
              bottom? (nil? nnext-stack)
              yellow? (and (not bottom?)
                           (identical? :yellow (.-color nnext-stack)))
              nc (next-color (blength pn)
                             (blength sn)
                             (not bottom?))]
          (cond
            (and (zero? (blength pn))
                 (zero? (blength sn))
                 bottom?)
            (node p nil nil s)

            (and left?
                 (identical? :yellow nc))
            (node p (node pn (.-left next-stack) nil sn) (.-right stack) s)

            left?
            (node p nil (node pn (.-left next-stack) (.-right stack) sn) s)

            (identical? :yellow nc)
            (node p
                  (node pn (.-left (.-right stack)) nil sn)
                  (.-right (.-right stack))
                  s)

            :else
            (node p
                  nil
                  (node pn (.-left next-stack) (.-right next-stack) sn)
                  s)

            #_
            (let [nn (node pn (.-left next-stack) (.-right next-stack) sn)]
              (if (identical? :yellow (.-color nn))
                (node p nn (.-right stack) s)
                (node p nil nn s)))
            #_
            (if left?
              (node p
                    (node pn (.-left next-stack) (.-right next-stack) sn)
                    (.-right stack)
                    s)
              (node p
                    nil
                    (node pn (.-left next-stack) (.-right next-stack) sn)
                    s))))

        (and (<= (+ pnlen snlen) 1) (or (>= plen 2) (>= slen 2)))
        (let [bottom? (and (not left?) (not (.-right stack)))
              pn (cond
                   bottom?      nil
                   (== snlen 1) sn
                   :else        pn)
              sn nil
              large-p? (>= plen 4)
              large-s? (>= slen 4)
              small-p? (<= plen 1)
              small-s? (<= slen 1)
              pn (if large-p?
                   (bpush pn (binspect2 p))
                   pn)
              p  (if large-p?
                   (beject2 p)
                   p)
              pn (if large-s?
                   (binject pn (bpeek2 s))
                   pn)
              s  (if large-s?
                   (bpop2 s)
                   s)
              p  (if small-p?
                   (binject2 p (bpeek pn))
                   p)
              pn (if small-p?
                   (bpop pn)
                   pn)
              s  (if small-s?
                   (bpush2 s (binspect pn))
                   s)
              pn (if small-s?
                   (beject pn)
                   pn)]
          (cond
            (zero? (blength pn))
            (node p nil nil s)

            (identical? :yellow (color (blength pn)))
            (node p (node pn nil nil nil) nil s)

            :else
            (node p nil (node pn nil nil nil) s)))

        :else
        (let [three? (or (pos? plen) (pos? slen))
              b (if three?
                  (if (pos? plen) p s))
              c (if (pos? pnlen) pn sn)
              p (bpeek c)
              b (if three?
                  (if (pos? plen)
                    (binject2 b p)
                    (bpush2 b p))
                  (buffer (.-car p) (.-cdr p)))]
          (node b nil nil nil))))
    (node (.-prefix stack)
          (.-left stack)
          (restore (.-right stack))
          (.-suffix stack))))

(declare ->EmptyNoncatenableDeque)

(definterface NDLeft
  (jqNDPeek [])
  (jqNDPop  [])
  (jqNDPush [x]))

(definterface NDRight
  (jqNDInspect [])
  (jqNDEject   [])
  (jqNDInject  [x]))

(deftype NoncatenableDeque [^Node stack
                            ^int cnt
                            ^boolean flip
                            ^clojure.lang.IPersistentMap _meta
                            ^:unsynchronized-mutable ^int _hash
                            ^:unsynchronized-mutable ^int _hasheq]
  Left
  (jqPeek [this]
    (if flip
      (.jqNDInspect this)
      (.jqNDPeek this)))

  (jqPop [this]
    (if flip
      (.jqNDEject this)
      (.jqNDPop this)))

  (jqPush [this x]
    (if flip
      (.jqNDInject this x)
      (.jqNDPush this x)))

  Right
  (jqInspect [this]
    (if flip
      (.jqNDPeek this)
      (.jqNDInspect this)))

  (jqEject [this]
    (if flip
      (.jqNDPop this)
      (.jqNDEject this)))

  (jqInject [this x]
    (if flip
      (.jqNDPush this x)
      (.jqNDInject this x)))

  NDLeft
  (jqNDPeek [this]
    (if (.-prefix stack)
      (bpeek (.-prefix stack))
      (bpeek (.-suffix stack))))

  (jqNDPop [this]
    (if (== cnt 1)
      (->EmptyNoncatenableDeque _meta)
      (if (.-prefix stack)
        (let [ret (node (bpop (.-prefix stack))
                        (.-left stack)
                        (.-right stack)
                        (.-suffix stack))]
          (NoncatenableDeque. (if (regular? ret)
                                ret
                                (restore ret))
                              (unchecked-dec-int cnt)
                              flip
                              _meta
                              -1
                              -1))
        (NoncatenableDeque. (node nil nil nil (bpop (.-suffix stack)))
                            (unchecked-dec-int cnt)
                            flip
                            _meta
                            -1
                            -1))))

  (jqNDPush [this x]
    (let [ret (if (.-prefix stack)
                (node (bpush (.-prefix stack) x)
                      (.-left stack)
                      (.-right stack)
                      (.-suffix stack))
                (node nil nil nil (bpush (.-suffix stack) x)))]
      (NoncatenableDeque. (if (regular? ret)
                            ret
                            (restore ret))
                          (unchecked-inc-int cnt)
                          flip
                          _meta
                          -1
                          -1)))

  NDRight
  (jqNDInspect [this]
    (if stack
      (if (.-suffix stack)
        (binspect (.-suffix stack))
        (binspect (.-prefix stack)))))

  (jqNDEject [this]
    (if (== cnt 1)
      (->EmptyNoncatenableDeque _meta)
      (if (.-suffix stack)
        (let [ret (node (.-prefix stack)
                        (.-left stack)
                        (.-right stack)
                        (beject (.-suffix stack)))]
          (NoncatenableDeque. (if (regular? ret)
                                ret
                                (restore ret))
                              (unchecked-dec-int cnt)
                              flip
                              _meta
                              -1
                              -1))
        (NoncatenableDeque. (node (beject (.-prefix stack)) nil nil nil)
                            (unchecked-dec-int cnt)
                            flip
                            _meta
                            -1
                            -1))))

  (jqNDInject [this x]
    (let [ret (if (.-suffix stack)
                (node (.-prefix stack)
                      (.-left stack)
                      (.-right stack)
                      (binject (.-suffix stack) x))
                (node (binject (.-prefix stack) x) nil nil nil))]
      (NoncatenableDeque. (if (regular? ret)
                            ret
                            (restore ret))
                          (unchecked-inc-int cnt)
                          flip
                          _meta
                          -1
                          -1)))

  Object
  (hashCode [this]
    (if (== _hash -1)
      (loop [h (int 1) xs this]
        (if xs
          (recur (unchecked-add-int (unchecked-multiply-int (int 31) h)
                                    (let [x (first xs)]
                                      (if (nil? x) (int 0) (.hashCode x))))
                 (next xs))
          (do (set! _hash (int h))
              h)))
      _hash))

  clojure.lang.IHashEq
  (hasheq [this]
    (if (== _hasheq -1)
      (loop [h (int 1) xs this]
        (if xs
          (recur (unchecked-add-int (unchecked-multiply-int (int 31) h)
                                    (Util/hasheq (first xs)))
                 (next xs))
          (do (set! _hasheq (int h))
              h)))
      _hasheq))

  clojure.lang.Counted
  (count [this]
    cnt)

  clojure.lang.IPersistentCollection
  (cons [this x]
    (.jqPush this x))

  (empty [this]
    (->EmptyNoncatenableDeque _meta))

  (equiv [this that]
    (if-not (instance? clojure.lang.Sequential that)
      false
      (loop [xs this
             ys (seq that)]
        (if xs
          (if (or (nil? ys) (not (Util/equiv (first xs) (first ys))))
            false
            (recur (next xs) (next ys)))
          (nil? ys)))))

  clojure.lang.Seqable
  (seq [this]
    this)

  clojure.lang.Sequential
  clojure.lang.ISeq
  (first [this]
    (.jqPeek this))

  (next [this]
    (if (== cnt 1)
      nil
      (.jqPop this)))

  (more [this]
    (.jqPop this))

  clojure.lang.Reversible
  (rseq [this]
    (NoncatenableDeque. stack cnt (not flip) _meta -1 -1))

  clojure.lang.IPersistentList

  clojure.lang.IPersistentStack
  (peek [this]
    (.jqPeek this))

  (pop [this]
    (.jqPop this))

  clojure.lang.IMeta
  (meta [this]
    _meta)

  clojure.lang.IObj
  (withMeta [this meta]
    (NoncatenableDeque. stack cnt flip meta _hash _hasheq))

  java.util.Collection
  (contains [this x]
    (boolean (some #(= % x) this)))

  (containsAll [this c]
    (every? #(.contains this %) c))

  (isEmpty [this]
    false)

  (toArray [this]
    (into-array Object this))

  (toArray [this arr]
    (if (>= (count arr) cnt)
      (loop [i 0 xs this]
        (if (< i cnt)
          (do (aset arr i (first xs))
              (recur (inc i) (next xs)))
          arr))
      (into-array Object this)))

  (size [this]
    cnt)

  (add [this x] (throw (UnsupportedOperationException.)))
  (addAll [this c] (throw (UnsupportedOperationException.)))
  (clear [this] (throw (UnsupportedOperationException.)))
  (^boolean remove [this x] (throw (UnsupportedOperationException.)))
  (removeAll [this c] (throw (UnsupportedOperationException.)))
  (retainAll [this c] (throw (UnsupportedOperationException.)))

  java.io.Serializable)

(deftype EmptyNoncatenableDeque [^clojure.lang.IPersistentMap _meta]
  Left
  (jqPeek [this]
    nil)

  (jqPop [this]
    (throw (ex-info "pop from empty noncatenable deque" {})))

  (jqPush [this x]
    (NoncatenableDeque. (Node. :yellow (buffer x) nil nil nil) 1 false _meta -1 -1))

  Right
  (jqInspect [this]
    nil)

  (jqEject [this]
    (throw (ex-info "eject from empty noncatenable deque" {})))

  (jqInject [this x]
    (NoncatenableDeque. (Node. :yellow nil nil nil (buffer x)) 1 false _meta -1 -1))

  Object
  (hashCode [this]
    1)

  clojure.lang.IHashEq
  (hasheq [this]
    1)

  clojure.lang.Counted
  (count [this]
    (int 0))

  clojure.lang.IPersistentCollection
  (cons [this x]
    (.jqPush this x))

  (empty [this]
    this)

  (equiv [this that]
    (and (instance? clojure.lang.Sequential that)
         (nil? (seq that))))

  clojure.lang.Seqable
  (seq [this]
    nil)

  clojure.lang.Sequential
  clojure.lang.ISeq
  (first [this]
    nil)

  (next [this]
    nil)

  (more [this]
    ())

  clojure.lang.Reversible
  (rseq [this]
    nil)

  clojure.lang.IPersistentList

  clojure.lang.IPersistentStack
  (peek [this]
    (.jqPeek this))

  (pop [this]
    (.jqPop this))

  clojure.lang.IMeta
  (meta [this]
    _meta)

  clojure.lang.IObj
  (withMeta [this meta]
    (EmptyNoncatenableDeque. meta))

  java.util.Collection
  (contains [this x]
    false)

  (containsAll [this c]
    false)

  (isEmpty [this]
    true)

  (toArray [this]
    (object-array 0))

  (toArray [this arr]
    arr)

  (size [this]
    (int 0))

  (add [this x] (throw (UnsupportedOperationException.)))
  (addAll [this c] (throw (UnsupportedOperationException.)))
  (clear [this] (throw (UnsupportedOperationException.)))
  (^boolean remove [this x] (throw (UnsupportedOperationException.)))
  (removeAll [this c] (throw (UnsupportedOperationException.)))
  (retainAll [this c] (throw (UnsupportedOperationException.)))

  java.io.Serializable)

(def empty (->EmptyNoncatenableDeque nil))

(comment

  (defn debug [ndeque-or-stack]
    (let [stack
          (if (.endsWith (.getName (class ndeque-or-stack)) "Deque")
            (.-stack ndeque-or-stack)
            ndeque-or-stack)]
      (println
       (.-color stack)
       (if (.-prefix stack) "(non-nil prefix)" "")
       (if (.-suffix stack) "(non-nil suffix)" "")
       (if (.-left stack)   "(non-nil left)"   "")
       (if (.-right stack)  "(non-nil right)"  ""))
      (println (str "P: " (pr-str (seq (.-prefix stack)))
                    " "
                    "S: " (pr-str (seq (.-suffix stack)))))))

  (defn alternate [n & [right?]]
    (loop [nd (jumping-queues.core/ndeque [])
           n  n
           fs ((if right? next identity)
               (cycle [#(.jqPush %1 %2) #(.jqInject %1 %2)]))]
      (if (zero? n)
        nd
        (recur ((first fs) nd n)
               (dec n)
               (next fs)))))

  (defn dealternate [ndeque]
    (loop [ndeque ndeque
           fs (cycle [#(.jqInspect %) #(.jqPeek %)])
           gs (cycle [#(.jqEject %) #(.jqPop %)])
           out []]
      (if (seq ndeque)
        (recur ((first gs) ndeque)
               (next fs)
               (next gs)
               (conj out ((first fs) ndeque)))
        out)))

  (comment

    ((fn [x] (= (dealternate (alternate x)) (range 1 (inc x)))) 2000)

    )

  (defn debug-stacks [ndeque-or-stack]
    (let [stack (if (.endsWith (.getName (class ndeque-or-stack)) "Deque")
                  (.-stack ndeque-or-stack)
                  ndeque-or-stack)]
      (loop [stack stack]
        (when stack
          (println (.-color stack))
          (println "P: " (seq (.-prefix stack)))
          (println "S: " (seq (.-suffix stack)))
          (println "======")
          (when (.-left stack)
            (loop [stack (.-left stack)]
              (when stack
                (println "  P: " (seq (.-prefix stack)))
                (println "  S: " (seq (.-suffix stack)))
                (println "======")
                (recur (.-left stack)))))
          (recur (.-right stack))))))

  (comment

    ;; provoking the no-buffer case
    (debug-stacks (-> (q/ndeque (range 6))
                      (q/push -1 -2 -3)
                      (q/pop)
                      (q/pop)
                      (q/pop)
                      (q/pop)
                      (q/eject)
                      (q/pop)))

    )

  )
