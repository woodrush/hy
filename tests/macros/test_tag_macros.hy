(import [hy])
(defclass Py2HyReturnException [Exception] 
 (defn __init__ [self retvalue] 
 (setv self.retvalue retvalue)))
(import [hyhy.macros [macroexpand]])
(import [hyhy.compiler [HyTypeError HyASTCompiler]])
(import [hyhy.lex [tokenize]])
(defn test_tag_macro_error [] 
 "Check if we get correct error with wrong dispatch character" 
 (try 
 (try 
 (macroexpand (get (tokenize "(dispatch_tag_macro '- '())") 0) (HyASTCompiler __name__)) 
 (except [e Py2HyReturnException] 
 (raise e)) 
 (except [e HyTypeError] 
 (assert (in "with the character `-`" (str e))))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))