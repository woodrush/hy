(import [hyhy.macros [macro]])
(defclass Py2HyReturnException [Exception] (defn __init__ [self retvalue] (setv self.retvalue retvalue)))
(import [hyhy [HyList HyInteger]])
(with_decorator 
 (macro "qplah") 
 (defn tmac [&rest tree] 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException (HyList (+ (, (HyInteger 8)) tree))))) (except [e Py2HyReturnException] e.retvalue))))
(with_decorator 
 (macro "parald") 
 (defn tmac2 [&rest tree] 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException (HyList (+ (, (HyInteger 9)) tree))))) (except [e Py2HyReturnException] e.retvalue))))
