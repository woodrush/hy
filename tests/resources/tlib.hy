(import [hyhy.macros [macro]])
(defclass Py2HyReturnException [Exception]
  (defn __init__ [self retvalue]
    (setv self.retvalue retvalue)))
(import [hyhy [HyList HyInteger]])
(with_decorator
  (macro "qplah")
  (defn tmac [&rest tree]
    (HyList (+ (, (HyInteger 8)) tree))))
(with_decorator
  (macro "parald")
  (defn tmac2 [&rest tree]
    (HyList (+ (, (HyInteger 9)) tree))))
