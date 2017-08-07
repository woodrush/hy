(import [hy])
(defclass Py2HyReturnException [Exception] (defn __init__ [self retvalue] (setv self.retvalue retvalue)))
(import [hyhy.macros [macro macroexpand]])
(import [hyhy.lex [tokenize]])
(import [hyhy.models [HyString HyList HySymbol HyExpression]])
(import [hyhy.errors [HyMacroExpansionError]])
(import [hyhy.compiler [HyASTCompiler]])
(with_decorator 
 (macro "test") 
 (defn tmac [&rest tree] 
 " Turn an expression into a list " 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException (HyList tree)))) (except [e Py2HyReturnException] e.retvalue))))
(defn test_preprocessor_simple [] 
 " Test basic macro expansion " 
 (do (setv obj (macroexpand (get (tokenize "(test \"one\" \"two\")") 0) (HyASTCompiler __name__)))) 
 (assert (= obj (HyList ["one" "two"]))) 
 (assert (= (type obj) HyList)))
(defn test_preprocessor_expression [] 
 " Test that macro expansion doesn't recurse" 
 (do (setv obj (macroexpand (get (tokenize "(test (test \"one\" \"two\"))") 0) (HyASTCompiler __name__)))) 
 (assert (= (type obj) HyList)) 
 (assert (= (type (get obj 0)) HyExpression)) 
 (assert (= (get obj 0) (HyExpression [(HySymbol "test") (HyString "one") (HyString "two")]))) 
 (do (setv obj (HyList [(HyString "one") (HyString "two")]))) 
 (do (setv obj (get (get (tokenize "(shill [\"one\" \"two\"])") 0) 1))) 
 (assert (= obj (macroexpand obj (HyASTCompiler "")))))
(defn test_preprocessor_exceptions [] 
 " Test that macro expansion raises appropriate exceptions" 
 "Using a hacky implementation of `return`" 
 (try (do (try (do (macroexpand (get (tokenize "(defn)") 0) (HyASTCompiler __name__)) 
 (assert False)) (except [e Py2HyReturnException] (raise e)) (except [e HyMacroExpansionError] (assert (not_in "_hy_anon_fn_" (str e))) (assert (not_in "TypeError" (str e)))))) (except [e Py2HyReturnException] e.retvalue)))
