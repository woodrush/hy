(import [ast])
(defclass Py2HyReturnException [Exception]
  (defn __init__ [self retvalue]
    (setv self.retvalue retvalue)))
(import [hy])
(import [hyhy [compiler]])
(import [hyhy.models [HyExpression HyList HySymbol HyInteger]])
(import [hyhy._compat [PY3]])
(defn test_builds_with_dash []
  (try
    (do
      (assert (callable (compiler.builds "foobar")))
      (assert (callable (compiler.builds "foo_bar")))
      (assert (callable (compiler.builds "-")))
      (try
        (compiler.builds "foobar-with-dash-")
        (except [e Py2HyReturnException]
          (raise e))
        (except [e TypeError]
          (assert (in "*post* translated strings" (str e))))
        (else (assert False))))
    (except [e Py2HyReturnException]
      e.retvalue)))
(defn make_expression [&rest args]
  (setv h (HyExpression args))
  (setv h.start_line 1)
  (setv h.end_line 1)
  (setv h.start_column 1)
  (setv h.end_column 1)
  (h.replace h))
(defn test_compiler_bare_names []
  "
    Check that the compiler doesn't drop bare names from code branches
    "
  (setv e (make_expression (HySymbol "do") (HySymbol "a") (HySymbol "b") (HySymbol "c")))
  (setv ret ((. (compiler.HyASTCompiler "test") compile) e))
  (assert (= (len ret.stmts) 2))
  (for [[stmt symbol] (zip ret.stmts "ab")]
    (assert (isinstance stmt ast.Expr))
    (assert (isinstance stmt.value ast.Name))
    (assert (= stmt.value.id symbol)))
  (assert (isinstance ret.expr ast.Name))
  (assert (= ret.expr.id "c")))
(defn test_compiler_yield_return []
  "
    Check that the compiler correctly generates return statements for
    a generator function. In Python versions prior to 3.3, the return
    statement in a generator can't take a value, so the final expression
    should not generate a return statement. From 3.3 onwards a return
    value should be generated.
    "
  (setv e (make_expression (HySymbol "fn") (HyList) (HyExpression [(HySymbol "yield") (HyInteger 2)]) (HyExpression [(HySymbol "+") (HyInteger 1) (HyInteger 1)])))
  (setv ret ((. (compiler.HyASTCompiler "test") compile_function_def) e))
  (assert (= (len ret.stmts) 1))
  (do
    (setv _py2hy_anon_var_G_1235 ret.stmts)
    (setv stmt (nth _py2hy_anon_var_G_1235 0)))
  (assert (isinstance stmt ast.FunctionDef))
  (setv body stmt.body)
  (assert (= (len body) 2))
  (assert (isinstance (get body 0) ast.Expr))
  (assert (isinstance (. (get body 0) value) ast.Yield))
  (if PY3
    (do
      (assert (isinstance (get body 1) ast.Return))
      (assert (isinstance (. (get body 1) value) ast.BinOp)))
    (do
      (assert (isinstance (get body 1) ast.Expr))
      (assert (isinstance (. (get body 1) value) ast.BinOp)))))