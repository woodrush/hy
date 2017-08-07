(defclass Py2HyReturnException [Exception] (defn __init__ [self retvalue] (setv self.retvalue retvalue)))
(defn kwtest [&kwargs kwargs &rest args] 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException kwargs))) (except [e Py2HyReturnException] e.retvalue)))
(defn function_with_a_dash [] 
 (do))
