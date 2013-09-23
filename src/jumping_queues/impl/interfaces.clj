(ns jumping-queues.impl.interfaces)

(definterface Left
  (jqPeek [])
  (jqPop  [])
  (jqPush [x]))

(definterface Right
  (jqInspect [])
  (jqEject   [])
  (jqInject  [x]))

(definterface QCat
  (jqCat [that]))

(definterface QFlip
  (jqFlip []))

(definterface QMin
  (jqMin []))
