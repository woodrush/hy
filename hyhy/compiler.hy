(import [hy])
(defclass Py2HyReturnException [Exception] 
 (defn __init__ [self retvalue] 
 (setv self.retvalue retvalue)))
(import [hyhy.models [HyObject HyExpression HyKeyword HyInteger HyComplex HyString HyBytes HySymbol HyFloat HyList HySet HyDict HyCons wrap_value]])
(import [hyhy.errors [HyCompileError HyTypeError]])
(import [hyhy.lex.parser [hy_symbol_mangle]])
(import [hyhy.macros])
(import [hyhy._compat [str_type string_types bytes_type long_type PY3 PY34 PY35 raise_empty]])
(import [hyhy.macros])
(import [hyhy.importer])
(import [traceback])
(import [importlib])
(import [codecs])
(import [ast])
(import [sys])
(import [keyword])
(import [collections [defaultdict]])
(if PY3 
 (do 
 (import [builtins])) 
 (do 
 (import [__builtin__ :as builtins])))
(setv _compile_time_ns {})
(defn compile_time_ns [module_name] 
 (try 
 (do 
 (setv ns (_compile_time_ns.get module_name)) 
 (when (is ns None) 
 (setv ns {"hyhy" hyhy "__name__" module_name}) 
 (assoc _compile_time_ns module_name ns)) 
 (raise (Py2HyReturnException ns))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))
(setv _stdlib {})
(defn load_stdlib [] 
 (import [hyhy.core]) 
 (for [module hyhy.core.STDLIB] 
 (setv mod (importlib.import_module module)) 
 (for [e mod.EXPORTS] 
 (when (is_not (getattr mod e) (getattr builtins e "")) 
 (assoc _stdlib e module)))))
(defn _is_hy_builtin [name module_name] 
 (try 
 (do 
 (setv extras ["True" "False" "None"]) 
 (when (or (in name extras) (keyword.iskeyword name)) 
 (raise (Py2HyReturnException True))) 
 (when (not (module_name.startswith "hyhy.")) 
 (raise (Py2HyReturnException (in name _compile_table)))) 
 (raise (Py2HyReturnException False))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))
(setv _compile_table {})
(defn ast_str [foobar] 
 (try 
 (do 
 (when PY3 
 (raise (Py2HyReturnException (str foobar)))) 
 (try 
 [(raise (Py2HyReturnException (str foobar)))] 
 (except [e Py2HyReturnException] 
 (raise e)) 
 (except [UnicodeEncodeError] 
 (do))) 
 (setv enc (codecs.getencoder "punycode")) 
 (do 
 (setv _py2hy_anon_var_G_1235 (enc foobar)) 
 (setv foobar (nth _py2hy_anon_var_G_1235 0))) 
 (raise (Py2HyReturnException (% "hy_%s" ((. (str foobar) replace) "-" "_"))))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))
(defn builds [_type] 
 (try 
 (do 
 (setv unpythonic_chars ["-"]) 
 (setv really_ok ["-"]) 
 (when (any (genexpr (in x unpythonic_chars) [x (str_type _type)])) 
 (when (not_in _type really_ok) 
 (raise (TypeError (% "Dear Hypster: `build' needs to be *post* translated strings... `%s' sucks." _type))))) 
 (defn _dec [fn_py2hy_mangling] 
 (try 
 (do 
 (assoc _compile_table _type fn_py2hy_mangling) 
 (raise (Py2HyReturnException fn_py2hy_mangling))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (raise (Py2HyReturnException _dec))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))
(defn builds_if [_type condition] 
 (try 
 [(if condition (do (raise (Py2HyReturnException (builds _type)))) (do (raise (Py2HyReturnException (fn [fn_py2hy_mangling] fn_py2hy_mangling)))))] 
 (except [e Py2HyReturnException] 
 e.retvalue)))
(defn spoof_positions [obj] 
 (try 
 (do 
 (when (or (not (isinstance obj HyObject)) (isinstance obj HyCons)) 
 (raise (Py2HyReturnException None))) 
 (when (not (hasattr obj "start_column")) 
 (setv obj.start_column 0)) 
 (when (not (hasattr obj "start_line")) 
 (setv obj.start_line 0)) 
 (when (and (hasattr obj "__iter__") (not (isinstance obj (, string_types bytes_type)))) 
 (for [x obj] 
 (spoof_positions x)))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))
(defclass Result [object] 
 "
    Smart representation of the result of a hy->AST compilation

    This object tries to reconcile the hy world, where everything can be used
    as an expression, with the Python world, where statements and expressions
    need to coexist.

    To do so, we represent a compiler result as a list of statements `stmts`,
    terminated by an expression context `expr`. The expression context is used
    when the compiler needs to use the result as an expression.

    Results are chained by addition: adding two results together returns a
    Result representing the succession of the two Results' statements, with
    the second Result's expression context.

    We make sure that a non-empty expression context does not get clobbered by
    adding more results, by checking accesses to the expression context. We
    assume that the context has been used, or deliberately ignored, if it has
    been accessed.

    The Result object is interoperable with python AST objects: when an AST
    object gets added to a Result object, it gets converted on-the-fly.
    " 
 (setv __slots__ (, "imports" "stmts" "temp_variables" "_expr" "__used_expr" "contains_yield")) 
 (defn __init__ [self &kwargs kwargs &rest args] 
 (when args 
 (raise (TypeError "Yo: Hacker: don't pass me real args, dingus"))) 
 (setv self.imports (defaultdict set)) 
 (setv self.stmts []) 
 (setv self.temp_variables []) 
 (setv self._expr None) 
 (setv self.contains_yield False) 
 (setv self.__used_expr False) 
 (for [kwarg kwargs] 
 (when (not_in kwarg ["imports" "contains_yield" "stmts" "expr" "temp_variables"]) 
 (raise (TypeError (% "%s() got an unexpected keyword argument '%s'" (, self.__class__.__name__ kwarg))))) 
 (setattr self kwarg (get kwargs kwarg)))) 
 (with_decorator 
 property 
 (defn expr [self] 
 (try 
 (do 
 (setv self.__used_expr True) 
 (raise (Py2HyReturnException self._expr))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 expr.setter 
 (defn expr [self value] 
 (setv self.__used_expr False) 
 (setv self._expr value))) 
 (defn add_imports [self mod imports] 
 "Autoimport `imports` from `mod`" 
 ((. (get self.imports mod) update) imports)) 
 (defn is_expr [self] 
 "Check whether I am a pure expression" 
 (try 
 [(raise (Py2HyReturnException (and self._expr (not (or self.imports self.stmts)))))] 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (with_decorator 
 property 
 (defn force_expr [self] 
 "Force the expression context of the Result.

        If there is no expression context, we return a \"None\" expression.
        " 
 (try 
 (do 
 (when self.expr 
 (raise (Py2HyReturnException self.expr))) 
 (setv lineno 0) 
 (setv col_offset 0) 
 (when self.stmts 
 (setv lineno (. (get self.stmts (- 1)) lineno)) 
 (setv col_offset (. (get self.stmts (- 1)) col_offset))) 
 (raise (Py2HyReturnException (ast.Name :id (ast_str "None") :arg (ast_str "None") :ctx (ast.Load) :lineno lineno :col_offset col_offset)))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (defn expr_as_stmt [self] 
 "Convert the Result's expression context to a statement

        This is useful when we want to use the stored expression in a
        statement context (for instance in a code branch).

        We drop ast.Names if they are appended to statements, as they
        can't have any side effect. \"Bare\" names still get converted to
        statements.

        If there is no expression context, return an empty result.
        " 
 (try 
 (do 
 (when (and self.expr (not (and (isinstance self.expr ast.Name) self.stmts))) 
 (raise (Py2HyReturnException (+ (Result) (ast.Expr :lineno self.expr.lineno :col_offset self.expr.col_offset :value self.expr))))) 
 (raise (Py2HyReturnException (Result)))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (defn rename [self new_name] 
 "Rename the Result's temporary variables to a `new_name`.

        We know how to handle ast.Names and ast.FunctionDefs.
        " 
 (setv new_name (ast_str new_name)) 
 (for [var self.temp_variables] 
 (if (isinstance var ast.Name) 
 (do 
 (setv var.id new_name) 
 (setv var.arg new_name)) 
 (do 
 (if (isinstance var ast.FunctionDef) 
 (do 
 (setv var.name new_name)) 
 (do 
 (raise (TypeError (% "Don't know how to rename a %s!" var.__class__.__name__)))))))) 
 (setv self.temp_variables [])) 
 (defn __add__ [self other] 
 (try 
 (do 
 (when (isinstance other ast.stmt) 
 (raise (Py2HyReturnException (+ self (Result :stmts [other]))))) 
 (when (isinstance other ast.expr) 
 (raise (Py2HyReturnException (+ self (Result :expr other))))) 
 (when (isinstance other ast.excepthandler) 
 (raise (Py2HyReturnException (+ self (Result :stmts [other]))))) 
 (when (not (isinstance other Result)) 
 (raise (TypeError (% "Can't add %r with non-compiler result %r" (, self other))))) 
 (when (and self.expr (not self.__used_expr)) 
 (traceback.print_stack) 
 (print (% "Bad boy clobbered expr %s with %s" (, (ast.dump self.expr) (ast.dump other.expr))))) 
 (setv result (Result)) 
 (setv result.imports other.imports) 
 (setv result.stmts (+ self.stmts other.stmts)) 
 (setv result.expr other.expr) 
 (setv result.temp_variables other.temp_variables) 
 (setv result.contains_yield False) 
 (when (or self.contains_yield other.contains_yield) 
 (setv result.contains_yield True)) 
 (raise (Py2HyReturnException result))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (defn __str__ [self] 
 (try 
 [(raise (Py2HyReturnException (% "Result(imports=[%s], stmts=[%s], expr=%s, contains_yield=%s)" (, ((. ", " join) (genexpr (ast.dump x) [x self.imports])) ((. ", " join) (genexpr (ast.dump x) [x self.stmts])) (if self.expr (ast.dump self.expr) None) self.contains_yield))))] 
 (except [e Py2HyReturnException] 
 e.retvalue))))
(defn _branch [results] 
 "Make a branch out of a list of Result objects

    This generates a Result from the given sequence of Results, forcing each
    expression context as a statement before the next result is used.

    We keep the expression context of the last argument for the returned Result
    " 
 (try 
 (do 
 (setv results (list results)) 
 (setv ret (Result)) 
 (for [result (get results (slice None (- 1) None))] 
 (setv ret (+ ret result)) 
 (setv ret (+ ret (result.expr_as_stmt)))) 
 (for [result (get results (slice (- 1) None None))] 
 (setv ret (+ ret result))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))
(defn _raise_wrong_args_number [expression error] 
 (raise (HyTypeError expression (% error (, (expression.pop 0) (len expression))))))
(defn checkargs [&optional [exact None] [min None] [max None] [even None] [multiple None]] 
 (try 
 (do 
 (defn _dec [fn_py2hy_mangling] 
 (try 
 (do 
 (defn checker [self expression] 
 (try 
 (do 
 (when (and (is_not exact None) (!= (- (len expression) 1) exact)) 
 (_raise_wrong_args_number expression (% "`%%s' needs %d arguments, got %%d" exact))) 
 (when (and (is_not min None) (< (- (len expression) 1) min)) 
 (_raise_wrong_args_number expression (% "`%%s' needs at least %d arguments, got %%d." min))) 
 (when (and (is_not max None) (> (- (len expression) 1) max)) 
 (_raise_wrong_args_number expression (% "`%%s' needs at most %d arguments, got %%d" max))) 
 (setv is_even (not (% (- (len expression) 1) 2))) 
 (when (and (is_not even None) (!= is_even even)) 
 (setv even_str (if even 
 "even" 
 "odd")) 
 (_raise_wrong_args_number expression (% "`%%s' needs an %s number of arguments, got %%d" even_str))) 
 (when (is_not multiple None) 
 (when (not (in (- (len expression) 1) multiple)) 
 (setv choices ((. ", " join) (list_comp (str val) [val (get multiple (slice None (- 1) None))]))) 
 (setv choices (+ choices (% " or %s" (get multiple (- 1))))) 
 (_raise_wrong_args_number expression (% "`%%s' needs %s arguments, got %%d" choices)))) 
 (raise (Py2HyReturnException (fn_py2hy_mangling self expression)))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (raise (Py2HyReturnException checker))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (raise (Py2HyReturnException _dec))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))
(defn is_unpack [kind x] 
 (try 
 [(raise (Py2HyReturnException (and (isinstance x HyExpression) (> (len x) 0) (isinstance (get x 0) HySymbol) (= (get x 0) (+ "unpack_" kind)))))] 
 (except [e Py2HyReturnException] 
 e.retvalue)))
(defclass HyASTCompiler [object] 
 (defn __init__ [self module_name] 
 (setv self.allow_builtins (module_name.startswith "hyhy.core")) 
 (setv self.anon_fn_count 0) 
 (setv self.anon_var_count 0) 
 (setv self.imports (defaultdict set)) 
 (setv self.module_name module_name) 
 (setv self.temp_if None) 
 (when (not (module_name.startswith "hyhy.core")) 
 (load_stdlib))) 
 (defn get_anon_var [self] 
 (try 
 (do 
 (setv self.anon_var_count (+ self.anon_var_count 1)) 
 (raise (Py2HyReturnException (% "_hy_anon_var_%s" self.anon_var_count)))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (defn get_anon_fn [self] 
 (try 
 (do 
 (setv self.anon_fn_count (+ self.anon_fn_count 1)) 
 (raise (Py2HyReturnException (% "_hy_anon_fn_%d" self.anon_fn_count)))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (defn update_imports [self result] 
 "Retrieve the imports from the result object" 
 (for [mod result.imports] 
 ((. (get self.imports mod) update) (get result.imports mod)))) 
 (defn imports_as_stmts [self expr] 
 "Convert the Result's imports to statements" 
 (try 
 (do 
 (setv ret (Result)) 
 (for [[module names] (self.imports.items)] 
 (when (in None names) 
 (setv e ((. (HyExpression [(HySymbol "import") (HySymbol module)]) replace) expr)) 
 (spoof_positions e) 
 (setv ret (+ ret (self.compile e)))) 
 (setv names (sorted (genexpr name [name names] (and name)))) 
 (when names 
 (setv e ((. (HyExpression [(HySymbol "import") (HyList [(HySymbol module) (HyList (list_comp (HySymbol name) [name names]))])]) replace) expr)) 
 (spoof_positions e) 
 (setv ret (+ ret (self.compile e))))) 
 (setv self.imports (defaultdict set)) 
 (raise (Py2HyReturnException ret.stmts))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (defn compile_atom [self atom_type atom] 
 (try 
 (do 
 (when (in atom_type _compile_table) 
 (setv ret ((get _compile_table atom_type) self atom)) 
 (when (not (isinstance ret Result)) 
 (setv ret (+ (Result) ret))) 
 (raise (Py2HyReturnException ret))) 
 (when (not (isinstance atom HyObject)) 
 (setv atom (wrap_value atom)) 
 (when (isinstance atom HyObject) 
 (spoof_positions atom) 
 (raise (Py2HyReturnException (self.compile_atom (type atom) atom)))))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (defn compile [self tree] 
 (try 
 (do 
 (try 
 (do 
 (setv _type (type tree)) 
 (setv ret (self.compile_atom _type tree)) 
 (when ret 
 (self.update_imports ret) 
 (raise (Py2HyReturnException ret)))) 
 (except [e Py2HyReturnException] 
 (raise e)) 
 (except [HyCompileError] 
 (raise)) 
 (except [e HyTypeError] 
 (raise)) 
 (except [e Exception] 
 (raise_empty HyCompileError e (get (sys.exc_info) 2)))) 
 (raise (HyCompileError (Exception (% "Unknown type: `%s'" _type))))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (defn _compile_collect [self exprs &optional [with_kwargs False] [dict_display False] [oldpy_unpack False]] 
 "Collect the expression contexts from a list of compiled expression.

        This returns a list of the expression contexts, and the sum of the
        Result objects passed as arguments.

        " 
 (try 
 (do 
 (setv compiled_exprs []) 
 (setv ret (Result)) 
 (setv keywords []) 
 (setv oldpy_starargs None) 
 (setv oldpy_kwargs None) 
 (setv exprs_iter (iter exprs)) 
 (for [expr exprs_iter] 
 (if (and (not PY35) oldpy_unpack (is_unpack "iterable" expr)) 
 (do 
 (when oldpy_starargs 
 (raise (HyTypeError expr "Pythons < 3.5 allow only one `unpack-iterable` per call"))) 
 (setv oldpy_starargs (self.compile (get expr 1))) 
 (setv ret (+ ret oldpy_starargs)) 
 (setv oldpy_starargs oldpy_starargs.force_expr)) 
 (do 
 (if (is_unpack "mapping" expr) 
 (do 
 (setv ret (+ ret (self.compile (get expr 1)))) 
 (if PY35 
 (do 
 (if dict_display 
 (do 
 (compiled_exprs.append None) 
 (compiled_exprs.append ret.force_expr)) 
 (do 
 (when with_kwargs 
 (keywords.append (ast.keyword :arg None :value ret.force_expr :lineno expr.start_line :col_offset expr.start_column)))))) 
 (do 
 (when oldpy_unpack 
 (when oldpy_kwargs 
 (raise (HyTypeError expr "Pythons < 3.5 allow only one `unpack-mapping` per call"))) 
 (setv oldpy_kwargs ret.force_expr))))) 
 (do 
 (if (and with_kwargs (isinstance expr HyKeyword)) 
 (do 
 (try 
 [(setv value (next exprs_iter))] 
 (except [e Py2HyReturnException] 
 (raise e)) 
 (except [StopIteration] 
 (raise (HyTypeError expr ((. "Keyword argument {kw} needs a value." format) :kw (str (get expr (slice 1 None None)))))))) 
 (setv compiled_value (self.compile value)) 
 (setv ret (+ ret compiled_value)) 
 (setv keyword (str (get expr (slice 2 None None)))) 
 (when (and (in "-" keyword) (!= keyword "-")) 
 (setv keyword (keyword.replace "-" "_"))) 
 (keywords.append (ast.keyword :arg keyword :value compiled_value.force_expr :lineno expr.start_line :col_offset expr.start_column))) 
 (do 
 (setv ret (+ ret (self.compile expr))) 
 (compiled_exprs.append ret.force_expr)))))))) 
 (if oldpy_unpack 
 (do 
 (raise (Py2HyReturnException (, compiled_exprs ret keywords oldpy_starargs oldpy_kwargs)))) 
 (do 
 (raise (Py2HyReturnException (, compiled_exprs ret keywords)))))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (defn _compile_branch [self exprs] 
 (try 
 [(raise (Py2HyReturnException (_branch (genexpr (self.compile expr) [expr exprs]))))] 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (defn _parse_lambda_list [self exprs] 
 " Return FunctionDef parameter values from lambda list." 
 (try 
 (do 
 (setv ll_keywords (, "&rest" "&optional" "&key" "&kwonly" "&kwargs")) 
 (setv ret (Result)) 
 (setv args []) 
 (setv defaults []) 
 (setv varargs None) 
 (setv kwonlyargs []) 
 (setv kwonlydefaults []) 
 (setv kwargs None) 
 (setv lambda_keyword None) 
 (for [expr exprs] 
 (when (in expr ll_keywords) 
 (if (= expr "&optional") 
 (do 
 (when (> (len defaults) 0) 
 (raise (HyTypeError expr "There can only be &optional arguments or one &key argument"))) 
 (setv lambda_keyword expr)) 
 (do 
 (if (in expr (, "&rest" "&key" "&kwonly" "&kwargs")) 
 (do 
 (setv lambda_keyword expr)) 
 (do 
 (raise (HyTypeError expr ((. "{0} is in an invalid position." format) (repr expr)))))))) 
 (continue)) 
 (if (is lambda_keyword None) 
 (do 
 (args.append expr)) 
 (do 
 (if (= lambda_keyword "&rest") 
 (do 
 (when varargs 
 (raise (HyTypeError expr "There can only be one &rest argument"))) 
 (setv varargs expr)) 
 (do 
 (if (= lambda_keyword "&key") 
 (do 
 (if (!= (type expr) HyDict) 
 (do 
 (raise (HyTypeError expr "There can only be one &key argument"))) 
 (do 
 (when (> (len defaults) 0) 
 (raise (HyTypeError expr "There can only be &optional arguments or one &key argument"))) 
 (setv it (iter expr)) 
 (for [[k v] (zip it it)] 
 (when (not (isinstance k HyString)) 
 (raise (HyTypeError expr "Only strings can be used as parameter names"))) 
 (args.append k) 
 (setv ret (+ ret (self.compile v))) 
 (defaults.append ret.force_expr))))) 
 (do 
 (if (= lambda_keyword "&optional") 
 (do 
 (if (isinstance expr HyList) 
 (do 
 (when (not (= (len expr) 2)) 
 (raise (HyTypeError expr "optional args should be bare names or 2-item lists"))) 
 (do 
 (setv _py2hy_anon_var_G_1237 expr) 
 (setv k (nth _py2hy_anon_var_G_1237 0)) 
 (setv v (nth _py2hy_anon_var_G_1237 1)))) 
 (do 
 (setv k expr) 
 (setv v ((. (HySymbol "None") replace) k)))) 
 (when (not (isinstance k HyString)) 
 (raise (HyTypeError expr "Only strings can be used as parameter names"))) 
 (args.append k) 
 (setv ret (+ ret (self.compile v))) 
 (defaults.append ret.force_expr)) 
 (do 
 (if (= lambda_keyword "&kwonly") 
 (do 
 (when (not PY3) 
 (raise (HyTypeError expr "keyword-only arguments are only available under Python 3"))) 
 (if (isinstance expr HyList) 
 (do 
 (when (!= (len expr) 2) 
 (raise (HyTypeError expr "keyword-only args should be bare names or 2-item lists"))) 
 (do 
 (setv _py2hy_anon_var_G_1236 expr) 
 (setv k (nth _py2hy_anon_var_G_1236 0)) 
 (setv v (nth _py2hy_anon_var_G_1236 1))) 
 (kwonlyargs.append k) 
 (setv ret (+ ret (self.compile v))) 
 (kwonlydefaults.append ret.force_expr)) 
 (do 
 (setv k expr) 
 (kwonlyargs.append k) 
 (kwonlydefaults.append None)))) 
 (do 
 (when (= lambda_keyword "&kwargs") 
 (when kwargs 
 (raise (HyTypeError expr "There can only be one &kwargs argument"))) 
 (setv kwargs expr))))))))))))) 
 (raise (Py2HyReturnException (, ret args defaults varargs kwonlyargs kwonlydefaults kwargs)))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (defn _storeize [self expr name &optional [func None]] 
 "Return a new `name` object with an ast.Store() context" 
 (try 
 (do 
 (when (not func) 
 (setv func ast.Store)) 
 (when (isinstance name Result) 
 (when (not (name.is_expr)) 
 (raise (HyTypeError expr "Can't assign or delete a non-expression"))) 
 (setv name name.expr)) 
 (if (isinstance name (, ast.Tuple ast.List)) 
 (do 
 (setv typ (type name)) 
 (setv new_elts []) 
 (for [x name.elts] 
 (new_elts.append (self._storeize expr x func))) 
 (setv new_name (typ :elts new_elts))) 
 (do 
 (if (isinstance name ast.Name) 
 (do 
 (setv new_name (ast.Name :id name.id :arg name.arg))) 
 (do 
 (if (isinstance name ast.Subscript) 
 (do 
 (setv new_name (ast.Subscript :value name.value :slice name.slice))) 
 (do 
 (if (isinstance name ast.Attribute) 
 (do 
 (setv new_name (ast.Attribute :value name.value :attr name.attr))) 
 (do 
 (if (and PY3 (isinstance name ast.Starred)) 
 (do 
 (setv new_name (ast.Starred :value (self._storeize expr name.value func)))) 
 (do 
 (raise (HyTypeError expr (% "Can't assign or delete a %s" (. (type expr) __name__)))))))))))))) 
 (setv new_name.ctx (func)) 
 (ast.copy_location new_name name) 
 (raise (Py2HyReturnException new_name))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (defn _render_quoted_form [self form level] 
 "
        Render a quoted form as a new HyExpression.

        `level` is the level of quasiquoting of the current form. We can
        unquote if level is 0.

        Returns a three-tuple (`imports`, `expression`, `splice`).

        The `splice` return value is used to mark `unquote-splice`d forms.
        We need to distinguish them as want to concatenate them instead of
        just nesting them.
        " 
 (try 
 (do 
 (when (= level 0) 
 (when (isinstance form HyExpression) 
 (when (and form (in (get form 0) (, "unquote" "unquote_splice"))) 
 (when (!= (len form) 2) 
 (raise (HyTypeError form (, (% "`%s' needs 1 argument, got %s" (get form 0)) (- (len form) 1))))) 
 (raise (Py2HyReturnException (, (set) (get form 1) (= (get form 0) "unquote_splice"))))))) 
 (when (isinstance form HyExpression) 
 (when (and form (= (get form 0) "quasiquote")) 
 (setv level (+ level 1))) 
 (when (and form (in (get form 0) (, "unquote" "unquote_splice"))) 
 (setv level (- level 1)))) 
 (setv name form.__class__.__name__) 
 (setv imports (set [name])) 
 (if (isinstance form (, HyList HyDict HySet)) 
 (do 
 (if (not form) 
 (do 
 (setv contents (HyList))) 
 (do 
 (setv contents (HyExpression [(HySymbol "+") (HyList)])))) 
 (for [x form] 
 (do 
 (setv _py2hy_anon_var_G_1240 (self._render_quoted_form x level)) 
 (setv f_imports (nth _py2hy_anon_var_G_1240 0)) 
 (setv f_contents (nth _py2hy_anon_var_G_1240 1)) 
 (setv splice (nth _py2hy_anon_var_G_1240 2))) 
 (imports.update f_imports) 
 (if splice 
 (do 
 (setv to_add (HyExpression [(HySymbol "list") (HyExpression [(HySymbol "or") f_contents (HyList)])]))) 
 (do 
 (setv to_add (HyList [f_contents])))) 
 (contents.append to_add)) 
 (raise (Py2HyReturnException (, imports ((. (HyExpression [(HySymbol name) contents]) replace) form) False)))) 
 (do 
 (if (isinstance form HyCons) 
 (do 
 (setv ret (HyExpression [(HySymbol name)])) 
 (do 
 (setv _py2hy_anon_var_G_1238 (self._render_quoted_form form.car level)) 
 (setv nimport (nth _py2hy_anon_var_G_1238 0)) 
 (setv contents (nth _py2hy_anon_var_G_1238 1)) 
 (setv splice (nth _py2hy_anon_var_G_1238 2))) 
 (when splice 
 (raise (HyTypeError form "Can't splice dotted lists yet"))) 
 (imports.update nimport) 
 (ret.append contents) 
 (do 
 (setv _py2hy_anon_var_G_1239 (self._render_quoted_form form.cdr level)) 
 (setv nimport (nth _py2hy_anon_var_G_1239 0)) 
 (setv contents (nth _py2hy_anon_var_G_1239 1)) 
 (setv splice (nth _py2hy_anon_var_G_1239 2))) 
 (when splice 
 (raise (HyTypeError form "Can't splice the cdr of a cons"))) 
 (imports.update nimport) 
 (ret.append contents) 
 (raise (Py2HyReturnException (, imports (ret.replace form) False)))) 
 (do 
 (when (isinstance form HySymbol) 
 (raise (Py2HyReturnException (, imports ((. (HyExpression [(HySymbol name) (HyString form)]) replace) form) False)))))))) 
 (raise (Py2HyReturnException (, imports ((. (HyExpression [(HySymbol name) form]) replace) form) False)))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (with_decorator 
 (builds "quote") 
 (builds "quasiquote") 
 (checkargs :exact 1) 
 (defn compile_quote [self entries] 
 (try 
 (do 
 (if (= (get entries 0) "quote") 
 (do 
 (setv level (float "inf"))) 
 (do 
 (setv level 0))) 
 (do 
 (setv _py2hy_anon_var_G_1241 (self._render_quoted_form (get entries 1) level)) 
 (setv imports (nth _py2hy_anon_var_G_1241 0)) 
 (setv stmts (nth _py2hy_anon_var_G_1241 1)) 
 (setv splice (nth _py2hy_anon_var_G_1241 2))) 
 (setv ret (self.compile stmts)) 
 (ret.add_imports "hyhy" imports) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "unquote") 
 (builds "unquote_splicing") 
 (defn compile_unquote [self expr] 
 (raise (HyTypeError expr (% "`%s' can't be used at the top-level" (get expr 0)))))) 
 (with_decorator 
 (builds "unpack_iterable") 
 (checkargs :exact 1) 
 (defn compile_unpack_iterable [self expr] 
 (try 
 (do 
 (when (not PY3) 
 (raise (HyTypeError expr "`unpack-iterable` isn't allowed here"))) 
 (setv ret (self.compile (get expr 1))) 
 (setv ret (+ ret (ast.Starred :value ret.force_expr :lineno expr.start_line :col_offset expr.start_column :ctx (ast.Load)))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "unpack_mapping") 
 (checkargs :exact 1) 
 (defn compile_unpack_mapping [self expr] 
 (raise (HyTypeError expr "`unpack-mapping` isn't allowed here")))) 
 (with_decorator 
 (builds "do") 
 (defn compile_do [self expression] 
 (try 
 (do 
 (expression.pop 0) 
 (raise (Py2HyReturnException (self._compile_branch expression)))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "raise") 
 (checkargs :multiple [0 1 3]) 
 (defn compile_raise_expression [self expr] 
 (try 
 (do 
 (expr.pop 0) 
 (setv ret (Result)) 
 (when expr 
 (setv ret (+ ret (self.compile (expr.pop 0))))) 
 (setv cause None) 
 (when (and (= (len expr) 2) (= (get expr 0) (HyKeyword ":from"))) 
 (when (not PY3) 
 (raise (HyCompileError "raise from only supported in python 3"))) 
 (expr.pop 0) 
 (setv cause (self.compile (expr.pop 0))) 
 (setv cause cause.expr)) 
 (setv ret (+ ret (ast.Raise :lineno expr.start_line :col_offset expr.start_column :type ret.expr :exc ret.expr :inst None :tback None :cause cause))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "try") 
 (checkargs :min 2) 
 (defn compile_try_expression [self expr] 
 (try 
 (do 
 (expr.pop 0) 
 (setv body (self.compile (if expr 
 (expr.pop 0) 
 []))) 
 (setv var (self.get_anon_var)) 
 (setv name (ast.Name :id (ast_str var) :arg (ast_str var) :ctx (ast.Store) :lineno expr.start_line :col_offset expr.start_column)) 
 (setv expr_name (ast.Name :id (ast_str var) :arg (ast_str var) :ctx (ast.Load) :lineno expr.start_line :col_offset expr.start_column)) 
 (setv returnable (Result :expr expr_name :temp_variables [expr_name name] :contains_yield body.contains_yield)) 
 (when (not (all expr)) 
 (raise (HyTypeError expr "Empty list not allowed in `try'"))) 
 (setv handler_results (Result)) 
 (setv handlers []) 
 (while (and expr (= (get (get expr 0) 0) (HySymbol "except"))) 
 (setv handler_results (+ handler_results (self._compile_catch_expression (expr.pop 0) name))) 
 (handlers.append (handler_results.stmts.pop))) 
 (setv orelse []) 
 (when (and expr (= (get (get expr 0) 0) (HySymbol "else"))) 
 (setv orelse (self._compile_branch (get (expr.pop 0) (slice 1 None None)))) 
 (setv orelse (+ orelse (ast.Assign :targets [name] :value orelse.force_expr :lineno expr.start_line :col_offset expr.start_column))) 
 (setv orelse (+ orelse (orelse.expr_as_stmt))) 
 (setv orelse orelse.stmts)) 
 (setv finalbody []) 
 (when (and expr (= (get (get expr 0) 0) (HySymbol "finally"))) 
 (setv finalbody (self._compile_branch (get (expr.pop 0) (slice 1 None None)))) 
 (setv finalbody (+ finalbody (finalbody.expr_as_stmt))) 
 (setv finalbody finalbody.stmts)) 
 (when expr 
 (when (in (get (get expr 0) 0) (, "except" "else" "finally")) 
 (raise (HyTypeError expr "Incorrect order of `except'/`else'/`finally' in `try'"))) 
 (raise (HyTypeError expr "Unknown expression in `try'"))) 
 (when (and orelse (not handlers)) 
 (raise (HyTypeError expr "`try' cannot have `else' without `except'"))) 
 (when (not (or handlers finalbody)) 
 (raise (HyTypeError expr "`try' must have an `except' or `finally' clause"))) 
 (setv ret handler_results) 
 (setv body (+ body (if orelse 
 (body.expr_as_stmt) 
 (ast.Assign :targets [name] :value body.force_expr :lineno expr.start_line :col_offset expr.start_column)))) 
 (setv body (or body.stmts [(ast.Pass :lineno expr.start_line :col_offset expr.start_column)])) 
 (when PY3 
 (raise (Py2HyReturnException (+ (+ ret (ast.Try :lineno expr.start_line :col_offset expr.start_column :body body :handlers handlers :orelse orelse :finalbody finalbody)) returnable)))) 
 (when finalbody 
 (when handlers 
 (raise (Py2HyReturnException (+ (+ ret (ast.TryFinally :lineno expr.start_line :col_offset expr.start_column :body [(ast.TryExcept :lineno expr.start_line :col_offset expr.start_column :handlers handlers :body body :orelse orelse)] :finalbody finalbody)) returnable)))) 
 (raise (Py2HyReturnException (+ (+ ret (ast.TryFinally :lineno expr.start_line :col_offset expr.start_column :body body :finalbody finalbody)) returnable)))) 
 (raise (Py2HyReturnException (+ (+ ret (ast.TryExcept :lineno expr.start_line :col_offset expr.start_column :handlers handlers :body body :orelse orelse)) returnable)))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "except") 
 (defn magic_internal_form [self expr] 
 (raise (HyTypeError expr (% "Error: `%s' can't be used like that." (get expr 0)))))) 
 (defn _compile_catch_expression [self expr var] 
 (try 
 (do 
 (setv catch (expr.pop 0)) 
 (setv exceptions (if expr 
 (expr.pop 0) 
 (HyList))) 
 (when (not (isinstance exceptions HyList)) 
 (raise (HyTypeError exceptions (% "`%s' exceptions list is not a list" catch)))) 
 (when (> (len exceptions) 2) 
 (raise (HyTypeError exceptions (% "`%s' exceptions list is too long" catch)))) 
 (setv name None) 
 (when (= (len exceptions) 2) 
 (setv name (exceptions.pop 0)) 
 (when (not (isinstance name HySymbol)) 
 (raise (HyTypeError exceptions "Exception storage target name must be a symbol."))) 
 (if PY3 
 (do 
 (setv name (ast_str name))) 
 (do 
 (setv name (self._storeize name (self.compile name)))))) 
 (setv exceptions_list (if exceptions 
 (exceptions.pop 0) 
 [])) 
 (if (isinstance exceptions_list list) 
 (do 
 (if (len exceptions_list) 
 (do 
 (do 
 (setv _py2hy_anon_var_G_1242 (self._compile_collect exceptions_list)) 
 (setv elts (nth _py2hy_anon_var_G_1242 0)) 
 (setv _type (nth _py2hy_anon_var_G_1242 1))) 
 (setv _type (+ _type (ast.Tuple :elts elts :lineno expr.start_line :col_offset expr.start_column :ctx (ast.Load))))) 
 (do 
 (setv _type (Result))))) 
 (do 
 (if (isinstance exceptions_list HySymbol) 
 (do 
 (setv _type (self.compile exceptions_list))) 
 (do 
 (raise (HyTypeError exceptions (% "`%s' needs a valid exception list" catch))))))) 
 (setv body (self._compile_branch expr)) 
 (setv body (+ body (ast.Assign :targets [var] :value body.force_expr :lineno expr.start_line :col_offset expr.start_column))) 
 (setv body (+ body (body.expr_as_stmt))) 
 (setv body body.stmts) 
 (when (not body) 
 (setv body [(ast.Pass :lineno expr.start_line :col_offset expr.start_column)])) 
 (raise (Py2HyReturnException (+ _type (ast.ExceptHandler :lineno expr.start_line :col_offset expr.start_column :type _type.expr :name name :body body))))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (with_decorator 
 (builds "if*") 
 (checkargs :min 2 :max 3) 
 (defn compile_if [self expression] 
 (try 
 (do 
 (expression.pop 0) 
 (setv cond (self.compile (expression.pop 0))) 
 (setv body (self.compile (expression.pop 0))) 
 (setv orel (Result)) 
 (do 
 (setv _py2hy_anon_var_G_1243 False) 
 (setv nested _py2hy_anon_var_G_1243) 
 (setv root _py2hy_anon_var_G_1243)) 
 (when expression 
 (setv orel_expr (expression.pop 0)) 
 (when (and (isinstance orel_expr HyExpression) (isinstance (get orel_expr 0) HySymbol) (= (get orel_expr 0) "if*")) 
 (setv root (is self.temp_if None)) 
 (setv nested True) 
 (setv self.temp_if (or self.temp_if (self.get_anon_var)))) 
 (setv orel (self.compile orel_expr))) 
 (when (and (not cond.stmts) (isinstance cond.force_expr ast.Name)) 
 (setv name cond.force_expr.id) 
 (setv branch None) 
 (if (= name "True") 
 (do 
 (setv branch body)) 
 (do 
 (when (in name (, "False" "None")) 
 (setv branch orel)))) 
 (when (is_not branch None) 
 (when (and self.temp_if branch.stmts) 
 (setv name (ast.Name :id (ast_str self.temp_if) :arg (ast_str self.temp_if) :ctx (ast.Store) :lineno expression.start_line :col_offset expression.start_column)) 
 (setv branch (+ branch (ast.Assign :targets [name] :value body.force_expr :lineno expression.start_line :col_offset expression.start_column)))) 
 (raise (Py2HyReturnException branch)))) 
 (setv ret cond) 
 (if (or body.stmts orel.stmts) 
 (do 
 (setv var (or self.temp_if (self.get_anon_var))) 
 (setv name (ast.Name :id (ast_str var) :arg (ast_str var) :ctx (ast.Store) :lineno expression.start_line :col_offset expression.start_column)) 
 (setv body (+ body (ast.Assign :targets [name] :value body.force_expr :lineno expression.start_line :col_offset expression.start_column))) 
 (when (or (not nested) (not orel.stmts) (and (not root) (!= var self.temp_if))) 
 (setv orel (+ orel (ast.Assign :targets [name] :value orel.force_expr :lineno expression.start_line :col_offset expression.start_column)))) 
 (setv ret (+ ret (ast.If :test ret.force_expr :body body.stmts :orelse orel.stmts :lineno expression.start_line :col_offset expression.start_column))) 
 (setv expr_name (ast.Name :id (ast_str var) :arg (ast_str var) :ctx (ast.Load) :lineno expression.start_line :col_offset expression.start_column)) 
 (setv ret (+ ret (Result :expr expr_name :temp_variables [expr_name name])))) 
 (do 
 (setv ret (+ ret (ast.IfExp :test ret.force_expr :body body.force_expr :orelse orel.force_expr :lineno expression.start_line :col_offset expression.start_column))))) 
 (when root 
 (setv self.temp_if None)) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "break") 
 (defn compile_break_expression [self expr] 
 (try 
 (do 
 (setv ret (ast.Break :lineno expr.start_line :col_offset expr.start_column)) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "continue") 
 (defn compile_continue_expression [self expr] 
 (try 
 (do 
 (setv ret (ast.Continue :lineno expr.start_line :col_offset expr.start_column)) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "assert") 
 (checkargs :min 1 :max 2) 
 (defn compile_assert_expression [self expr] 
 (try 
 (do 
 (expr.pop 0) 
 (setv e (expr.pop 0)) 
 (if (= (len expr) 1) 
 (do 
 (setv msg (. (self.compile (expr.pop 0)) force_expr))) 
 (do 
 (setv msg None))) 
 (setv ret (self.compile e)) 
 (setv ret (+ ret (ast.Assert :test ret.force_expr :msg msg :lineno e.start_line :col_offset e.start_column))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "global") 
 (checkargs :min 1) 
 (defn compile_global_expression [self expr] 
 (try 
 (do 
 (expr.pop 0) 
 (setv names []) 
 (while (> (len expr) 0) 
 (setv identifier (expr.pop 0)) 
 (setv name (ast_str identifier)) 
 (names.append name) 
 (when (not (isinstance identifier HySymbol)) 
 (raise (HyTypeError identifier "(global) arguments must  be Symbols")))) 
 (raise (Py2HyReturnException (ast.Global :names names :lineno expr.start_line :col_offset expr.start_column)))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "nonlocal") 
 (checkargs :min 1) 
 (defn compile_nonlocal_expression [self expr] 
 (try 
 (do 
 (when (not PY3) 
 (raise (HyCompileError "nonlocal only supported in python 3!"))) 
 (expr.pop 0) 
 (setv names []) 
 (while (> (len expr) 0) 
 (setv identifier (expr.pop 0)) 
 (setv name (ast_str identifier)) 
 (names.append name) 
 (when (not (isinstance identifier HySymbol)) 
 (raise (HyTypeError identifier "(nonlocal) arguments must be Symbols.")))) 
 (raise (Py2HyReturnException (ast.Nonlocal :names names :lineno expr.start_line :col_offset expr.start_column)))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "yield") 
 (checkargs :max 1) 
 (defn compile_yield_expression [self expr] 
 (try 
 (do 
 (expr.pop 0) 
 (setv ret (Result :contains_yield (not PY3))) 
 (setv value None) 
 (when (!= expr []) 
 (setv ret (+ ret (self.compile (expr.pop 0)))) 
 (setv value ret.force_expr)) 
 (setv ret (+ ret (ast.Yield :value value :lineno expr.start_line :col_offset expr.start_column))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds_if "yield_from" PY3) 
 (checkargs :max 1) 
 (defn compile_yield_from_expression [self expr] 
 (try 
 (do 
 (expr.pop 0) 
 (setv ret (Result :contains_yield True)) 
 (setv value None) 
 (when (!= expr []) 
 (setv ret (+ ret (self.compile (expr.pop 0)))) 
 (setv value ret.force_expr)) 
 (setv ret (+ ret (ast.YieldFrom :value value :lineno expr.start_line :col_offset expr.start_column))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "import") 
 (defn compile_import_expression [self expr] 
 (try 
 (do 
 (defn _compile_import [expr module &optional [names None] [importer ast.Import]] 
 (try 
 (do 
 (when (not names) 
 (setv names [(ast.alias :name (ast_str module) :asname None)])) 
 (setv ret (importer :lineno expr.start_line :col_offset expr.start_column :module (ast_str module) :names names :level 0)) 
 (raise (Py2HyReturnException (+ (Result) ret)))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (expr.pop 0) 
 (setv rimports (Result)) 
 (while (> (len expr) 0) 
 (setv iexpr (expr.pop 0)) 
 (when (not (isinstance iexpr (, HySymbol HyList))) 
 (raise (HyTypeError iexpr "(import) requires a Symbol or a List."))) 
 (when (isinstance iexpr HySymbol) 
 (setv rimports (+ rimports (_compile_import expr iexpr))) 
 (continue)) 
 (when (and (isinstance iexpr HyList) (= (len iexpr) 1)) 
 (setv rimports (+ rimports (_compile_import expr (iexpr.pop 0)))) 
 (continue)) 
 (when (and (isinstance iexpr HyList) iexpr) 
 (setv module (iexpr.pop 0)) 
 (setv entry (get iexpr 0)) 
 (when (and (isinstance entry HyKeyword) (= entry (HyKeyword ":as"))) 
 (when (not (= (len iexpr) 2)) 
 (raise (HyTypeError iexpr "garbage after aliased import"))) 
 (iexpr.pop 0) 
 (setv alias (iexpr.pop 0)) 
 (setv names [(ast.alias :name (ast_str module) :asname (ast_str alias))]) 
 (setv rimports (+ rimports (_compile_import expr (ast_str module) names))) 
 (continue)) 
 (when (isinstance entry HyList) 
 (setv names []) 
 (while entry 
 (setv sym (entry.pop 0)) 
 (if (and entry (isinstance (get entry 0) HyKeyword)) 
 (do 
 (entry.pop 0) 
 (setv alias (ast_str (entry.pop 0)))) 
 (do 
 (setv alias None))) 
 (names.append (ast.alias :name (ast_str sym) :asname alias))) 
 (setv rimports (+ rimports (_compile_import expr module names ast.ImportFrom))) 
 (continue)) 
 (raise (HyTypeError entry (% "Unknown entry (`%s`) in the HyList" entry))))) 
 (raise (Py2HyReturnException rimports))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "get") 
 (checkargs :min 2) 
 (defn compile_index_expression [self expr] 
 (try 
 (do 
 (expr.pop 0) 
 (setv val (self.compile (expr.pop 0))) 
 (do 
 (setv _py2hy_anon_var_G_1244 (self._compile_collect expr)) 
 (setv slices (nth _py2hy_anon_var_G_1244 0)) 
 (setv ret (nth _py2hy_anon_var_G_1244 1))) 
 (when val.stmts 
 (setv ret (+ ret val))) 
 (for [sli slices] 
 (setv val (+ (Result) (ast.Subscript :lineno expr.start_line :col_offset expr.start_column :value val.force_expr :slice (ast.Index :value sli) :ctx (ast.Load))))) 
 (raise (Py2HyReturnException (+ ret val)))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds ".") 
 (checkargs :min 1) 
 (defn compile_attribute_access [self expr] 
 (try 
 (do 
 (expr.pop 0) 
 (setv ret (self.compile (expr.pop 0))) 
 (for [attr expr] 
 (if (isinstance attr HySymbol) 
 (do 
 (setv ret (+ ret (ast.Attribute :lineno attr.start_line :col_offset attr.start_column :value ret.force_expr :attr (ast_str attr) :ctx (ast.Load))))) 
 (do 
 (if (= (type attr) HyList) 
 (do 
 (when (!= (len attr) 1) 
 (raise (HyTypeError attr ((. "The attribute access DSL only accepts HySymbols and one-item lists, got {0}-item list instead" format) (len attr))))) 
 (setv compiled_attr (self.compile (attr.pop 0))) 
 (setv ret (+ (+ compiled_attr ret) (ast.Subscript :lineno attr.start_line :col_offset attr.start_column :value ret.force_expr :slice (ast.Index :value compiled_attr.force_expr) :ctx (ast.Load))))) 
 (do 
 (raise (HyTypeError attr ((. "The attribute access DSL only accepts HySymbols and one-item lists, got {0} instead" format) (. (type attr) __name__))))))))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "del") 
 (defn compile_del_expression [self expr] 
 (try 
 (do 
 (setv root (expr.pop 0)) 
 (when (not expr) 
 (setv result (Result)) 
 (setv result (+ result (ast.Name :id "None" :ctx (ast.Load) :lineno root.start_line :col_offset root.start_column))) 
 (raise (Py2HyReturnException result))) 
 (setv del_targets []) 
 (setv ret (Result)) 
 (for [target expr] 
 (setv compiled_target (self.compile target)) 
 (setv ret (+ ret compiled_target)) 
 (del_targets.append (self._storeize target compiled_target ast.Del))) 
 (raise (Py2HyReturnException (+ ret (ast.Delete :lineno expr.start_line :col_offset expr.start_column :targets del_targets))))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "cut") 
 (checkargs :min 1 :max 4) 
 (defn compile_cut_expression [self expr] 
 (try 
 (do 
 (expr.pop 0) 
 (setv val (self.compile (expr.pop 0))) 
 (setv low (Result)) 
 (when (!= expr []) 
 (setv low (self.compile (expr.pop 0)))) 
 (setv high (Result)) 
 (when (!= expr []) 
 (setv high (self.compile (expr.pop 0)))) 
 (setv step (Result)) 
 (when (!= expr []) 
 (setv step (self.compile (expr.pop 0)))) 
 (raise (Py2HyReturnException (+ (+ (+ (+ val low) high) step) (ast.Subscript :lineno expr.start_line :col_offset expr.start_column :value val.force_expr :slice (ast.Slice :lower low.expr :upper high.expr :step step.expr) :ctx (ast.Load)))))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "assoc") 
 (checkargs :min 3 :even False) 
 (defn compile_assoc_expression [self expr] 
 (try 
 (do 
 (expr.pop 0) 
 (setv target (self.compile (expr.pop 0))) 
 (setv ret target) 
 (setv i (iter expr)) 
 (for [[key val] (genexpr (, (self.compile x) (self.compile y)) [[x y] (zip i i)])] 
 (setv ret (+ ret (+ (+ key val) (ast.Assign :lineno expr.start_line :col_offset expr.start_column :targets [(ast.Subscript :lineno expr.start_line :col_offset expr.start_column :value target.force_expr :slice (ast.Index :value key.force_expr) :ctx (ast.Store))] :value val.force_expr))))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "with_decorator") 
 (checkargs :min 1) 
 (defn compile_decorate_expression [self expr] 
 (try 
 (do 
 (expr.pop 0) 
 (setv fn_py2hy_mangling (self.compile (expr.pop (- 1)))) 
 (when (or (not fn_py2hy_mangling.stmts) (not (isinstance (get fn_py2hy_mangling.stmts (- 1)) (, ast.FunctionDef ast.ClassDef)))) 
 (raise (HyTypeError expr "Decorated a non-function"))) 
 (do 
 (setv _py2hy_anon_var_G_1245 (self._compile_collect expr)) 
 (setv decorators (nth _py2hy_anon_var_G_1245 0)) 
 (setv ret (nth _py2hy_anon_var_G_1245 1))) 
 (setv (. (get fn_py2hy_mangling.stmts (- 1)) decorator_list) (+ decorators (. (get fn_py2hy_mangling.stmts (- 1)) decorator_list))) 
 (raise (Py2HyReturnException (+ ret fn_py2hy_mangling)))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "with*") 
 (checkargs :min 2) 
 (defn compile_with_expression [self expr] 
 (try 
 (do 
 (expr.pop 0) 
 (setv args (expr.pop 0)) 
 (when (not (isinstance args HyList)) 
 (raise (HyTypeError expr ((. "with expects a list, received `{0}'" format) (. (type args) __name__))))) 
 (when (< (len args) 1) 
 (raise (HyTypeError expr "with needs [[arg (expr)]] or [[(expr)]]]"))) 
 (args.reverse) 
 (setv ctx (self.compile (args.pop 0))) 
 (setv thing None) 
 (when (!= args []) 
 (setv thing (self._storeize (get args 0) (self.compile (args.pop 0))))) 
 (setv body (self._compile_branch expr)) 
 (setv var (self.get_anon_var)) 
 (setv name (ast.Name :id (ast_str var) :arg (ast_str var) :ctx (ast.Store) :lineno expr.start_line :col_offset expr.start_column)) 
 (setv body (+ body (ast.Assign :targets [name] :value body.force_expr :lineno expr.start_line :col_offset expr.start_column))) 
 (setv the_with (ast.With :context_expr ctx.force_expr :lineno expr.start_line :col_offset expr.start_column :optional_vars thing :body body.stmts)) 
 (when PY3 
 (setv the_with.items [(ast.withitem :context_expr ctx.force_expr :optional_vars thing)])) 
 (setv ret (+ ctx the_with)) 
 (setv ret.contains_yield (or ret.contains_yield body.contains_yield)) 
 (setv expr_name (ast.Name :id (ast_str var) :arg (ast_str var) :ctx (ast.Load) :lineno expr.start_line :col_offset expr.start_column)) 
 (setv ret (+ ret (Result :expr expr_name :temp_variables [expr_name name]))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds ",") 
 (defn compile_tuple [self expr] 
 (try 
 (do 
 (expr.pop 0) 
 (do 
 (setv _py2hy_anon_var_G_1246 (self._compile_collect expr)) 
 (setv elts (nth _py2hy_anon_var_G_1246 0)) 
 (setv ret (nth _py2hy_anon_var_G_1246 1))) 
 (setv ret (+ ret (ast.Tuple :elts elts :lineno expr.start_line :col_offset expr.start_column :ctx (ast.Load)))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (defn _compile_generator_iterables [self trailers] 
 "Helper to compile the \"trailing\" parts of comprehensions:
        generators and conditions" 
 (try 
 (do 
 (setv generators (trailers.pop 0)) 
 (setv cond (if (!= trailers []) 
 (self.compile (trailers.pop 0)) 
 (Result))) 
 (setv gen_it (iter generators)) 
 (setv paired_gens (zip gen_it gen_it)) 
 (setv gen_res (Result)) 
 (setv gen []) 
 (for [[target iterable] paired_gens] 
 (setv comp_target (self.compile target)) 
 (setv target (self._storeize target comp_target)) 
 (setv gen_res (+ gen_res (self.compile iterable))) 
 (gen.append (ast.comprehension :target target :iter gen_res.force_expr :ifs [] :is_async False))) 
 (when cond.expr 
 ((. (. (get gen (- 1)) ifs) append) cond.expr)) 
 (raise (Py2HyReturnException (, (+ gen_res cond) gen)))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (with_decorator 
 (builds "list_comp") 
 (checkargs :min 2 :max 3) 
 (defn compile_list_comprehension [self expr] 
 (try 
 (do 
 (expr.pop 0) 
 (setv expression (expr.pop 0)) 
 (setv gen_gen (get expr 0)) 
 (when (not (isinstance gen_gen HyList)) 
 (raise (HyTypeError gen_gen "Generator expression must be a list."))) 
 (do 
 (setv _py2hy_anon_var_G_1247 (self._compile_generator_iterables expr)) 
 (setv gen_res (nth _py2hy_anon_var_G_1247 0)) 
 (setv gen (nth _py2hy_anon_var_G_1247 1))) 
 (when (= (len gen) 0) 
 (raise (HyTypeError gen_gen "Generator expression cannot be empty."))) 
 (setv compiled_expression (self.compile expression)) 
 (setv ret (+ compiled_expression gen_res)) 
 (setv ret (+ ret (ast.ListComp :lineno expr.start_line :col_offset expr.start_column :elt compiled_expression.force_expr :generators gen))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "set_comp") 
 (checkargs :min 2 :max 3) 
 (defn compile_set_comprehension [self expr] 
 (try 
 (do 
 (setv ret (self.compile_list_comprehension expr)) 
 (setv expr ret.expr) 
 (setv ret.expr (ast.SetComp :lineno expr.lineno :col_offset expr.col_offset :elt expr.elt :generators expr.generators)) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "dict_comp") 
 (checkargs :min 3 :max 4) 
 (defn compile_dict_comprehension [self expr] 
 (try 
 (do 
 (expr.pop 0) 
 (setv key (expr.pop 0)) 
 (setv value (expr.pop 0)) 
 (do 
 (setv _py2hy_anon_var_G_1248 (self._compile_generator_iterables expr)) 
 (setv gen_res (nth _py2hy_anon_var_G_1248 0)) 
 (setv gen (nth _py2hy_anon_var_G_1248 1))) 
 (setv compiled_key (self.compile key)) 
 (setv compiled_value (self.compile value)) 
 (setv ret (+ (+ compiled_key compiled_value) gen_res)) 
 (setv ret (+ ret (ast.DictComp :lineno expr.start_line :col_offset expr.start_column :key compiled_key.force_expr :value compiled_value.force_expr :generators gen))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "genexpr") 
 (defn compile_genexpr [self expr] 
 (try 
 (do 
 (setv ret (self.compile_list_comprehension expr)) 
 (setv expr ret.expr) 
 (setv ret.expr (ast.GeneratorExp :lineno expr.lineno :col_offset expr.col_offset :elt expr.elt :generators expr.generators)) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "not") 
 (builds "~") 
 (checkargs 1) 
 (defn compile_unary_operator [self expression] 
 (try 
 (do 
 (setv ops {"not" ast.Not "~" ast.Invert}) 
 (setv operator (expression.pop 0)) 
 (setv operand (self.compile (expression.pop 0))) 
 (setv operand (+ operand (ast.UnaryOp :op ((get ops operator)) :operand operand.expr :lineno operator.start_line :col_offset operator.start_column))) 
 (raise (Py2HyReturnException operand))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "require") 
 (defn compile_require [self expression] 
 "
        TODO: keep track of what we've imported in this run and then
        \"unimport\" it after we've completed `thing' so that we don't pollute
        other envs.
        " 
 (try 
 (do 
 (for [entry (get expression (slice 1 None None))] 
 (if (isinstance entry HySymbol) 
 (do 
 (__import__ entry) 
 (hyhy.macros.require_hyhy entry self.module_name :all_macros True :prefix entry)) 
 (do 
 (if (and (isinstance entry HyList) (= (len entry) 2)) 
 (do 
 (do 
 (setv _py2hy_anon_var_G_1250 entry) 
 (setv module (nth _py2hy_anon_var_G_1250 0)) 
 (setv names (nth _py2hy_anon_var_G_1250 1))) 
 (when (not (isinstance names HyList)) 
 (raise (HyTypeError names "(require) name lists should be HyLists"))) 
 (__import__ module) 
 (if (in "*" names) 
 (do 
 (when (!= (len names) 1) 
 (raise (HyTypeError names "* in a (require) name list must be on its own"))) 
 (hyhy.macros.require_hyhy module self.module_name :all_macros True)) 
 (do 
 (setv assignments {}) 
 (while names 
 (if (and (> (len names) 1) (= (get names 1) (HyKeyword ":as"))) 
 (do 
 (do 
 (setv _py2hy_anon_var_G_1251 (get names (slice None 3 None))) 
 (setv k (nth _py2hy_anon_var_G_1251 0)) 
 (setv v (nth _py2hy_anon_var_G_1251 2))) 
 (del (get names (slice None 3 None))) 
 (assoc assignments k v)) 
 (do 
 (setv symbol (names.pop 0)) 
 (assoc assignments symbol symbol)))) 
 (hyhy.macros.require_hyhy module self.module_name :assignments assignments)))) 
 (do 
 (if (and (isinstance entry HyList) (= (len entry) 3) (= (get entry 1) (HyKeyword ":as"))) 
 (do 
 (do 
 (setv _py2hy_anon_var_G_1249 entry) 
 (setv module (nth _py2hy_anon_var_G_1249 0)) 
 (setv prefix (nth _py2hy_anon_var_G_1249 2))) 
 (__import__ module) 
 (hyhy.macros.require_hyhy module self.module_name :all_macros True :prefix prefix)) 
 (do 
 (raise (HyTypeError entry "unrecognized (require) syntax"))))))))) 
 (raise (Py2HyReturnException (Result)))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "and") 
 (builds "or") 
 (defn compile_logical_or_and_and_operator [self expression] 
 (try 
 (do 
 (setv ops {"and" (, ast.And "True") "or" (, ast.Or "None")}) 
 (setv operator (expression.pop 0)) 
 (do 
 (setv _py2hy_anon_var_G_1252 (get ops operator)) 
 (setv opnode (nth _py2hy_anon_var_G_1252 0)) 
 (setv default (nth _py2hy_anon_var_G_1252 1))) 
 (do 
 (setv _py2hy_anon_var_G_1253 (, operator.start_line operator.start_column)) 
 (setv root_line (nth _py2hy_anon_var_G_1253 0)) 
 (setv root_column (nth _py2hy_anon_var_G_1253 1))) 
 (if (= (len expression) 0) 
 (do 
 (raise (Py2HyReturnException (ast.Name :id default :ctx (ast.Load) :lineno root_line :col_offset root_column)))) 
 (do 
 (when (= (len expression) 1) 
 (raise (Py2HyReturnException (self.compile (get expression 0))))))) 
 (setv ret (Result)) 
 (setv values (list (map self.compile expression))) 
 (setv has_stmt (any (genexpr value.stmts [value values]))) 
 (if has_stmt 
 (do 
 (setv var (self.get_anon_var)) 
 (setv name (ast.Name :id var :ctx (ast.Store) :lineno root_line :col_offset root_column)) 
 (setv expr_name (ast.Name :id var :ctx (ast.Load) :lineno root_line :col_offset root_column)) 
 (setv temp_variables [name expr_name]) 
 (defn make_assign [value &optional [node None]] 
 (try 
 (do 
 (if (is node None) 
 (do 
 (do 
 (setv _py2hy_anon_var_G_1255 (, root_line root_column)) 
 (setv line (nth _py2hy_anon_var_G_1255 0)) 
 (setv column (nth _py2hy_anon_var_G_1255 1)))) 
 (do 
 (do 
 (setv _py2hy_anon_var_G_1254 (, node.lineno node.col_offset)) 
 (setv line (nth _py2hy_anon_var_G_1254 0)) 
 (setv column (nth _py2hy_anon_var_G_1254 1))))) 
 (setv positioned_name (ast.Name :id var :ctx (ast.Store) :lineno line :col_offset column)) 
 (temp_variables.append positioned_name) 
 (raise (Py2HyReturnException (ast.Assign :targets [positioned_name] :value value :lineno line :col_offset column)))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (setv root []) 
 (setv current root) 
 (for [[i value] (enumerate values)] 
 (if value.stmts 
 (do 
 (setv node (get value.stmts 0)) 
 (current.extend value.stmts)) 
 (do 
 (setv node value.expr))) 
 (current.append (make_assign value.force_expr value.force_expr)) 
 (when (= i (- (len values) 1)) 
 (break)) 
 (if (= operator "and") 
 (do 
 (setv cond expr_name)) 
 (do 
 (when (= operator "or") 
 (setv cond (ast.UnaryOp :op (ast.Not) :operand expr_name :lineno node.lineno :col_offset node.col_offset))))) 
 (current.append (ast.If :test cond :body [] :lineno node.lineno :col_offset node.col_offset :orelse [])) 
 (setv current (. (get current (- 1)) body))) 
 (setv ret (sum root ret)) 
 (setv ret (+ ret (Result :expr expr_name :temp_variables temp_variables)))) 
 (do 
 (setv ret (+ ret (ast.BoolOp :op (opnode) :lineno root_line :col_offset root_column :values (list_comp value.force_expr [value values])))))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (defn _compile_compare_op_expression [self expression] 
 (try 
 (do 
 (setv ops {"=" ast.Eq "!=" ast.NotEq "<" ast.Lt "<=" ast.LtE ">" ast.Gt ">=" ast.GtE "is" ast.Is "is_not" ast.IsNot "in" ast.In "not_in" ast.NotIn}) 
 (setv inv (expression.pop 0)) 
 (setv op (get ops inv)) 
 (setv ops (list_comp (op) [x (range 1 (len expression))])) 
 (setv e (get expression 0)) 
 (do 
 (setv _py2hy_anon_var_G_1256 (self._compile_collect expression)) 
 (setv exprs (nth _py2hy_anon_var_G_1256 0)) 
 (setv ret (nth _py2hy_anon_var_G_1256 1))) 
 (raise (Py2HyReturnException (+ ret (ast.Compare :left (get exprs 0) :ops ops :comparators (get exprs (slice 1 None None)) :lineno e.start_line :col_offset e.start_column))))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (with_decorator 
 (builds "=") 
 (builds "is") 
 (builds "<") 
 (builds "<=") 
 (builds ">") 
 (builds ">=") 
 (checkargs :min 1) 
 (defn compile_compare_op_expression [self expression] 
 (try 
 (do 
 (when (= (len expression) 2) 
 (raise (Py2HyReturnException (ast.Name :id "True" :ctx (ast.Load) :lineno expression.start_line :col_offset expression.start_column)))) 
 (raise (Py2HyReturnException (self._compile_compare_op_expression expression)))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "!=") 
 (builds "is_not") 
 (checkargs :min 2) 
 (defn compile_compare_op_expression_coll [self expression] 
 (try 
 [(raise (Py2HyReturnException (self._compile_compare_op_expression expression)))] 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "in") 
 (builds "not_in") 
 (checkargs 2) 
 (defn compile_compare_op_expression_binary [self expression] 
 (try 
 [(raise (Py2HyReturnException (self._compile_compare_op_expression expression)))] 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (defn _compile_maths_expression [self expression] 
 (try 
 (do 
 (setv ops {"+" ast.Add "/" ast.Div "//" ast.FloorDiv "*" ast.Mult "-" ast.Sub "%" ast.Mod "**" ast.Pow "<<" ast.LShift ">>" ast.RShift "|" ast.BitOr "^" ast.BitXor "&" ast.BitAnd}) 
 (when PY35 
 (ops.update {"@" ast.MatMult})) 
 (setv op (get ops (expression.pop 0))) 
 (setv right_associative (= op ast.Pow)) 
 (do 
 (setv _py2hy_anon_var_G_1257 (, expression.start_line expression.start_column)) 
 (setv lineno (nth _py2hy_anon_var_G_1257 0)) 
 (setv col_offset (nth _py2hy_anon_var_G_1257 1))) 
 (when right_associative 
 (setv expression (get expression (slice None None (- 1))))) 
 (setv ret (self.compile (expression.pop 0))) 
 (for [child expression] 
 (setv left_expr ret.force_expr) 
 (setv ret (+ ret (self.compile child))) 
 (setv right_expr ret.force_expr) 
 (when right_associative 
 (do 
 (setv _py2hy_anon_var_G_1258 (, right_expr left_expr)) 
 (setv left_expr (nth _py2hy_anon_var_G_1258 0)) 
 (setv right_expr (nth _py2hy_anon_var_G_1258 1)))) 
 (setv ret (+ ret (ast.BinOp :left left_expr :op (op) :right right_expr :lineno lineno :col_offset col_offset)))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (with_decorator 
 (builds "**") 
 (builds "//") 
 (builds "<<") 
 (builds ">>") 
 (builds "&") 
 (checkargs :min 2) 
 (defn compile_maths_expression_2_or_more [self expression] 
 (try 
 [(raise (Py2HyReturnException (self._compile_maths_expression expression)))] 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "%") 
 (builds "^") 
 (checkargs 2) 
 (defn compile_maths_expression_exactly_2 [self expression] 
 (try 
 [(raise (Py2HyReturnException (self._compile_maths_expression expression)))] 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "*") 
 (builds "|") 
 (defn compile_maths_expression_mul [self expression] 
 (try 
 (do 
 (setv id_elem (get {"*" 1 "|" 0} (get expression 0))) 
 (if (= (len expression) 1) 
 (do 
 (raise (Py2HyReturnException (ast.Num :n (long_type id_elem) :lineno expression.start_line :col_offset expression.start_column)))) 
 (do 
 (if (= (len expression) 2) 
 (do 
 (raise (Py2HyReturnException (self.compile (get expression 1))))) 
 (do 
 (raise (Py2HyReturnException (self._compile_maths_expression expression)))))))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "/") 
 (checkargs :min 1) 
 (defn compile_maths_expression_div [self expression] 
 (try 
 (do 
 (when (= (len expression) 2) 
 (setv expression ((. (HyExpression [(HySymbol "/") (HyInteger 1) (get expression 1)]) replace) expression))) 
 (raise (Py2HyReturnException (self._compile_maths_expression expression)))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (defn _compile_maths_expression_additive [self expression] 
 (try 
 [(if (> (len expression) 2) (do (raise (Py2HyReturnException (self._compile_maths_expression expression)))) (do (setv op ((get {"+" ast.UAdd "-" ast.USub} (expression.pop 0)))) (setv arg (expression.pop 0)) (setv ret (self.compile arg)) (setv ret (+ ret (ast.UnaryOp :op op :operand ret.force_expr :lineno arg.start_line :col_offset arg.start_column))) (raise (Py2HyReturnException ret))))] 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (with_decorator 
 (builds "&") 
 (builds_if "@" PY35) 
 (checkargs :min 1) 
 (defn compile_maths_expression_unary_idempotent [self expression] 
 (try 
 [(if (= (len expression) 2) (do (raise (Py2HyReturnException (self.compile (get expression 1))))) (do (raise (Py2HyReturnException (self._compile_maths_expression expression)))))] 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "+") 
 (defn compile_maths_expression_add [self expression] 
 (try 
 [(if (= (len expression) 1) (do (raise (Py2HyReturnException (ast.Num :n (long_type 0) :lineno expression.start_line :col_offset expression.start_column)))) (do (raise (Py2HyReturnException (self._compile_maths_expression_additive expression)))))] 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "-") 
 (checkargs :min 1) 
 (defn compile_maths_expression_sub [self expression] 
 (try 
 [(raise (Py2HyReturnException (self._compile_maths_expression_additive expression)))] 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "+=") 
 (builds "/=") 
 (builds "//=") 
 (builds "*=") 
 (builds "_=") 
 (builds "%=") 
 (builds "**=") 
 (builds "<<=") 
 (builds ">>=") 
 (builds "|=") 
 (builds "^=") 
 (builds "&=") 
 (builds_if "@=" PY35) 
 (checkargs 2) 
 (defn compile_augassign_expression [self expression] 
 (try 
 (do 
 (setv ops {"+=" ast.Add "/=" ast.Div "//=" ast.FloorDiv "*=" ast.Mult "_=" ast.Sub "%=" ast.Mod "**=" ast.Pow "<<=" ast.LShift ">>=" ast.RShift "|=" ast.BitOr "^=" ast.BitXor "&=" ast.BitAnd}) 
 (when PY35 
 (ops.update {"@=" ast.MatMult})) 
 (setv op (get ops (get expression 0))) 
 (setv target (self._storeize (get expression 1) (self.compile (get expression 1)))) 
 (setv ret (self.compile (get expression 2))) 
 (setv ret (+ ret (ast.AugAssign :target target :value ret.force_expr :op (op) :lineno expression.start_line :col_offset expression.start_column))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (checkargs 1) 
 (defn _compile_keyword_call [self expression] 
 (try 
 (do 
 (expression.append (expression.pop 0)) 
 (expression.insert 0 (HySymbol "get")) 
 (raise (Py2HyReturnException (self.compile expression)))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds HyExpression) 
 (defn compile_expression [self expression] 
 (try 
 (do 
 (setv expression (hyhy.macros.macroexpand expression self)) 
 (when (not (isinstance expression HyExpression)) 
 (raise (Py2HyReturnException (self.compile expression)))) 
 (when (= expression []) 
 (raise (Py2HyReturnException (self.compile_list expression)))) 
 (setv fn_py2hy_mangling (get expression 0)) 
 (setv func None) 
 (when (isinstance fn_py2hy_mangling HyKeyword) 
 (raise (Py2HyReturnException (self._compile_keyword_call expression)))) 
 (when (isinstance fn_py2hy_mangling HySymbol) 
 (when (or (= fn_py2hy_mangling ",") (not (any (genexpr (is_unpack "iterable" x) [x (get expression (slice 1 None None))])))) 
 (setv ret (self.compile_atom fn_py2hy_mangling expression)) 
 (when ret 
 (raise (Py2HyReturnException ret)))) 
 (when (fn_py2hy_mangling.startswith ".") 
 (setv attrs (list_comp ((. (HySymbol a) replace) fn_py2hy_mangling) [a (get (fn_py2hy_mangling.split ".") (slice 1 None None))])) 
 (setv fn_py2hy_mangling (attrs.pop)) 
 (setv i 1) 
 (when (!= (len expression) 2) 
 (while (< i (len expression)) 
 (if (isinstance (get expression i) HyKeyword) 
 (do 
 (setv i (+ i 1))) 
 (do 
 (break))) 
 (setv i (+ i 1)))) 
 (setv func (self.compile (HyExpression (+ [((. (HySymbol ".") replace) fn_py2hy_mangling) (expression.pop i)] attrs)))) 
 (setv func (+ func (ast.Attribute :lineno fn_py2hy_mangling.start_line :col_offset fn_py2hy_mangling.start_column :value func.force_expr :attr (ast_str fn_py2hy_mangling) :ctx (ast.Load)))))) 
 (when (not func) 
 (setv func (self.compile fn_py2hy_mangling))) 
 (if (in fn_py2hy_mangling (, "type" "HyKeyword" "keyword" "name" "is_keyword")) 
 (do 
 (setv with_kwargs False)) 
 (do 
 (setv with_kwargs True))) 
 (do 
 (setv _py2hy_anon_var_G_1259 (self._compile_collect (get expression (slice 1 None None)) with_kwargs :oldpy_unpack True)) 
 (setv args (nth _py2hy_anon_var_G_1259 0)) 
 (setv ret (nth _py2hy_anon_var_G_1259 1)) 
 (setv keywords (nth _py2hy_anon_var_G_1259 2)) 
 (setv oldpy_starargs (nth _py2hy_anon_var_G_1259 3)) 
 (setv oldpy_kwargs (nth _py2hy_anon_var_G_1259 4))) 
 (setv ret (+ ret (ast.Call :func func.expr :args args :keywords keywords :starargs oldpy_starargs :kwargs oldpy_kwargs :lineno expression.start_line :col_offset expression.start_column))) 
 (raise (Py2HyReturnException (+ func ret)))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "def") 
 (builds "setv") 
 (defn compile_def_expression [self expression] 
 (try 
 (do 
 (setv root (expression.pop 0)) 
 (if (not expression) 
 (do 
 (setv result (Result)) 
 (setv result (+ result (ast.Name :id "None" :ctx (ast.Load) :lineno root.start_line :col_offset root.start_column))) 
 (raise (Py2HyReturnException result))) 
 (do 
 (if (= (len expression) 2) 
 (do 
 (raise (Py2HyReturnException (self._compile_assign (get expression 0) (get expression 1) expression.start_line expression.start_column)))) 
 (do 
 (if (!= (% (len expression) 2) 0) 
 (do 
 (raise (HyTypeError expression ((. "`{}' needs an even number of arguments" format) root)))) 
 (do 
 (setv result (Result)) 
 (for [[tgt target] (zip (get expression (slice None None 2)) (get expression (slice 1 None 2)))] 
 (setv result (+ result (self._compile_assign tgt target tgt.start_line tgt.start_column)))) 
 (raise (Py2HyReturnException result))))))))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (defn _compile_assign [self name result start_line start_column] 
 (try 
 (do 
 (setv str_name (% "%s" name)) 
 (when (and (_is_hy_builtin str_name self.module_name) (not self.allow_builtins)) 
 (raise (HyTypeError name (% "Can't assign to a builtin: `%s'" str_name)))) 
 (setv result (self.compile result)) 
 (setv ld_name (self.compile name)) 
 (when (isinstance ld_name.expr ast.Call) 
 (raise (HyTypeError name (% "Can't assign to a callable: `%s'" str_name)))) 
 (if (and result.temp_variables (isinstance name HyString) (not_in "." name)) 
 (do 
 (result.rename name) 
 (setv result.expr None)) 
 (do 
 (setv st_name (self._storeize name ld_name)) 
 (setv result (+ result (ast.Assign :lineno start_line :col_offset start_column :targets [st_name] :value result.force_expr))))) 
 (raise (Py2HyReturnException result))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (with_decorator 
 (builds "for*") 
 (checkargs :min 1) 
 (defn compile_for_expression [self expression] 
 (try 
 (do 
 (expression.pop 0) 
 (setv args (expression.pop 0)) 
 (when (not (isinstance args HyList)) 
 (raise (HyTypeError expression ((. "for expects a list, received `{0}'" format) (. (type args) __name__))))) 
 (try 
 [(do (setv _py2hy_anon_var_G_1260 args) (setv target_name (nth _py2hy_anon_var_G_1260 0)) (setv iterable (nth _py2hy_anon_var_G_1260 1)))] 
 (except [e Py2HyReturnException] 
 (raise e)) 
 (except [ValueError] 
 (raise (HyTypeError expression "for requires two forms in the list")))) 
 (setv target (self._storeize target_name (self.compile target_name))) 
 (setv ret (Result)) 
 (setv orel (Result)) 
 (when (and expression (= (get (get expression (- 1)) 0) (HySymbol "else"))) 
 (setv else_expr (expression.pop)) 
 (if (> (len else_expr) 2) 
 (do 
 (raise (HyTypeError else_expr "`else' statement in `for' is too long"))) 
 (do 
 (when (= (len else_expr) 2) 
 (setv orel (+ orel (self.compile (get else_expr 1)))) 
 (setv orel (+ orel (orel.expr_as_stmt))))))) 
 (setv ret (+ ret (self.compile iterable))) 
 (setv body (self._compile_branch expression)) 
 (setv body (+ body (body.expr_as_stmt))) 
 (setv ret (+ ret (ast.For :lineno expression.start_line :col_offset expression.start_column :target target :iter ret.force_expr :body body.stmts :orelse orel.stmts))) 
 (setv ret.contains_yield body.contains_yield) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "while") 
 (checkargs :min 2) 
 (defn compile_while_expression [self expr] 
 (try 
 (do 
 (expr.pop 0) 
 (setv ret (self.compile (expr.pop 0))) 
 (setv body (self._compile_branch expr)) 
 (setv body (+ body (body.expr_as_stmt))) 
 (setv ret (+ ret (ast.While :test ret.force_expr :body body.stmts :orelse [] :lineno expr.start_line :col_offset expr.start_column))) 
 (setv ret.contains_yield body.contains_yield) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds HyList) 
 (defn compile_list [self expression] 
 (try 
 (do 
 (do 
 (setv _py2hy_anon_var_G_1261 (self._compile_collect expression)) 
 (setv elts (nth _py2hy_anon_var_G_1261 0)) 
 (setv ret (nth _py2hy_anon_var_G_1261 1))) 
 (setv ret (+ ret (ast.List :elts elts :ctx (ast.Load) :lineno expression.start_line :col_offset expression.start_column))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds HySet) 
 (defn compile_set [self expression] 
 (try 
 (do 
 (do 
 (setv _py2hy_anon_var_G_1262 (self._compile_collect expression)) 
 (setv elts (nth _py2hy_anon_var_G_1262 0)) 
 (setv ret (nth _py2hy_anon_var_G_1262 1))) 
 (setv ret (+ ret (ast.Set :elts elts :ctx (ast.Load) :lineno expression.start_line :col_offset expression.start_column))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "fn") 
 (builds "fn*") 
 (checkargs :min 1) 
 (defn compile_function_def [self expression] 
 (try 
 (do 
 (setv force_functiondef (= (expression.pop 0) "fn*")) 
 (setv arglist (expression.pop 0)) 
 (when (not (isinstance arglist HyList)) 
 (raise (HyTypeError expression "First argument to `fn' must be a list"))) 
 (do 
 (setv _py2hy_anon_var_G_1263 (self._parse_lambda_list arglist)) 
 (setv ret (nth _py2hy_anon_var_G_1263 0)) 
 (setv args (nth _py2hy_anon_var_G_1263 1)) 
 (setv defaults (nth _py2hy_anon_var_G_1263 2)) 
 (setv stararg (nth _py2hy_anon_var_G_1263 3)) 
 (setv kwonlyargs (nth _py2hy_anon_var_G_1263 4)) 
 (setv kwonlydefaults (nth _py2hy_anon_var_G_1263 5)) 
 (setv kwargs (nth _py2hy_anon_var_G_1263 6))) 
 (for [[i arg] (enumerate args)] 
 (when (isinstance arg HyList) 
 (when (not arg) 
 (raise (HyTypeError arglist "Cannot destruct empty list"))) 
 (do 
 (setv _py2hy_anon_var_G_1264 (HySymbol (self.get_anon_var))) 
 (assoc args i _py2hy_anon_var_G_1264) 
 (setv var _py2hy_anon_var_G_1264)) 
 (setv expression (+ (HyExpression [(HyExpression [(HySymbol "setv") arg var])]) expression)) 
 (setv expression (expression.replace (get arg 0))))) 
 (if PY34 
 (do 
 (setv args (list_comp (ast.arg :arg (ast_str x) :annotation None :lineno x.start_line :col_offset x.start_column) [x args])) 
 (setv kwonlyargs (list_comp (ast.arg :arg (ast_str x) :annotation None :lineno x.start_line :col_offset x.start_column) [x kwonlyargs])) 
 (when kwargs 
 (setv kwargs (ast.arg :arg (ast_str kwargs) :annotation None :lineno kwargs.start_line :col_offset kwargs.start_column))) 
 (when stararg 
 (setv stararg (ast.arg :arg (ast_str stararg) :annotation None :lineno stararg.start_line :col_offset stararg.start_column)))) 
 (do 
 (setv args (list_comp (ast.Name :arg (ast_str x) :id (ast_str x) :ctx (ast.Param) :lineno x.start_line :col_offset x.start_column) [x args])) 
 (when PY3 
 (setv kwonlyargs (list_comp (ast.Name :arg (ast_str x) :id (ast_str x) :ctx (ast.Param) :lineno x.start_line :col_offset x.start_column) [x kwonlyargs]))) 
 (when kwargs 
 (setv kwargs (ast_str kwargs))) 
 (when stararg 
 (setv stararg (ast_str stararg))))) 
 (setv args (ast.arguments :args args :vararg stararg :kwarg kwargs :kwonlyargs kwonlyargs :kw_defaults kwonlydefaults :defaults defaults)) 
 (setv body (self._compile_branch expression)) 
 (when (and (not force_functiondef) (not body.stmts)) 
 (setv ret (+ ret (ast.Lambda :lineno expression.start_line :col_offset expression.start_column :args args :body body.force_expr))) 
 (raise (Py2HyReturnException ret))) 
 (when body.expr 
 (if (and body.contains_yield (not PY3)) 
 (do 
 (setv body (+ body (body.expr_as_stmt)))) 
 (do 
 (setv body (+ body (ast.Return :value body.expr :lineno body.expr.lineno :col_offset body.expr.col_offset)))))) 
 (when (not body.stmts) 
 (setv body (+ body (ast.Pass :lineno expression.start_line :col_offset expression.start_column)))) 
 (setv name (self.get_anon_fn)) 
 (setv ret (+ ret (ast.FunctionDef :name name :lineno expression.start_line :col_offset expression.start_column :args args :body body.stmts :decorator_list []))) 
 (setv ast_name (ast.Name :id name :arg name :ctx (ast.Load) :lineno expression.start_line :col_offset expression.start_column)) 
 (setv ret (+ ret (Result :expr ast_name :temp_variables [ast_name (get ret.stmts (- 1))]))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "defclass") 
 (checkargs :min 1) 
 (defn compile_class_expression [self expressions] 
 (try 
 (do 
 (defn rewire_init [expr] 
 (try 
 (do 
 (setv new_args []) 
 (when (= (get expr 0) (HySymbol "setv")) 
 (setv pairs (get expr (slice 1 None None))) 
 (while (> (len pairs) 0) 
 (do 
 (setv _py2hy_anon_var_G_1265 (, (pairs.pop 0) (pairs.pop 0))) 
 (setv k (nth _py2hy_anon_var_G_1265 0)) 
 (setv v (nth _py2hy_anon_var_G_1265 1))) 
 (when (= k (HySymbol "__init__")) 
 (v.append (HySymbol "None"))) 
 (new_args.append k) 
 (new_args.append v)) 
 (setv expr ((. (HyExpression (+ [(HySymbol "setv")] new_args)) replace) expr))) 
 (raise (Py2HyReturnException expr))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (expressions.pop 0) 
 (setv class_name (expressions.pop 0)) 
 (if expressions 
 (do 
 (setv base_list (expressions.pop 0)) 
 (when (not (isinstance base_list HyList)) 
 (raise (HyTypeError expressions "Bases class must be a list"))) 
 (do 
 (setv _py2hy_anon_var_G_1266 (self._compile_collect base_list)) 
 (setv bases_expr (nth _py2hy_anon_var_G_1266 0)) 
 (setv bases (nth _py2hy_anon_var_G_1266 1)))) 
 (do 
 (setv bases_expr []) 
 (setv bases (Result)))) 
 (setv body (Result)) 
 (when (and expressions (isinstance (get expressions 0) HyString)) 
 (setv docstring (expressions.pop 0)) 
 (setv symb (HySymbol "__doc__")) 
 (setv symb.start_line docstring.start_line) 
 (setv symb.start_column docstring.start_column) 
 (setv body (+ body (self._compile_assign symb docstring docstring.start_line docstring.start_column))) 
 (setv body (+ body (body.expr_as_stmt)))) 
 (setv allow_builtins self.allow_builtins) 
 (setv self.allow_builtins True) 
 (when (and expressions (isinstance (get expressions 0) HyList) (not (isinstance (get expressions 0) HyExpression))) 
 (setv expr (expressions.pop 0)) 
 (setv expr ((. (HyExpression (+ [(HySymbol "setv")] expr)) replace) expr)) 
 (setv body (+ body (self.compile (rewire_init expr))))) 
 (for [expression expressions] 
 (setv expr (rewire_init (hyhy.macros.macroexpand expression self))) 
 (setv body (+ body (self.compile expr)))) 
 (setv self.allow_builtins allow_builtins) 
 (when (not body.stmts) 
 (setv body (+ body (ast.Pass :lineno expressions.start_line :col_offset expressions.start_column)))) 
 (raise (Py2HyReturnException (+ bases (ast.ClassDef :lineno expressions.start_line :col_offset expressions.start_column :decorator_list [] :name (ast_str class_name) :keywords [] :starargs None :kwargs None :bases bases_expr :body body.stmts))))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (defn _compile_time_hack [self expression] 
 "Compile-time hack: we want to get our new macro now
        We must provide __name__ in the namespace to make the Python
        compiler set the __module__ attribute of the macro function." 
 (try 
 (do 
 (hyhy.importer.hy_eval expression (compile_time_ns self.module_name) self.module_name) 
 (setv ret (self.compile expression)) 
 (ret.add_imports "hyhy" [None]) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (with_decorator 
 (builds "defmacro") 
 (checkargs :min 1) 
 (defn compile_macro [self expression] 
 (try 
 (do 
 (expression.pop 0) 
 (setv name (expression.pop 0)) 
 (when (not (isinstance name HySymbol)) 
 (raise (HyTypeError name (% "received a `%s' instead of a symbol for macro name" (. (type name) __name__))))) 
 (setv name ((. (HyString name) replace) name)) 
 (for [kw (, "&kwonly" "&kwargs" "&key")] 
 (when (in kw (get expression 0)) 
 (raise (HyTypeError name (% "macros cannot use %s" kw))))) 
 (setv new_expression ((. (HyExpression [(HyExpression [(HySymbol "hyhy.macros.macro") name]) (HyExpression (+ [(HySymbol "fn")] expression))]) replace) expression)) 
 (setv ret (self._compile_time_hack new_expression)) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "deftag") 
 (checkargs :min 2) 
 (defn compile_tag_macro [self expression] 
 (try 
 (do 
 (expression.pop 0) 
 (setv name (expression.pop 0)) 
 (when (or (= name ":") (= name "&")) 
 (raise (NameError (% "%s can't be used as a tag macro name" name)))) 
 (when (and (not (isinstance name HySymbol)) (not (isinstance name HyString))) 
 (raise (HyTypeError name (% "received a `%s' instead of a symbol for tag macro name" (. (type name) __name__))))) 
 (setv name ((. (HyString name) replace) name)) 
 (setv new_expression ((. (HyExpression [(HyExpression [(HySymbol "hyhy.macros.tag") name]) (HyExpression (+ [(HySymbol "fn")] expression))]) replace) expression)) 
 (setv ret (self._compile_time_hack new_expression)) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "dispatch_tag_macro") 
 (checkargs :exact 2) 
 (defn compile_dispatch_tag_macro [self expression] 
 (try 
 (do 
 (expression.pop 0) 
 (setv tag (expression.pop 0)) 
 (when (not (= (type tag) HyString)) 
 (raise (HyTypeError tag ((. "Trying to expand a tag macro using `{0}' instead of string" format) (. (type tag) __name__))))) 
 (setv tag ((. (HyString (hy_symbol_mangle (str tag))) replace) tag)) 
 (setv expr (hyhy.macros.tag_macroexpand tag (expression.pop 0) self)) 
 (raise (Py2HyReturnException (self.compile expr)))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "eval_and_compile") 
 (defn compile_eval_and_compile [self expression] 
 (try 
 (do 
 (assoc expression 0 (HySymbol "do")) 
 (hyhy.importer.hy_eval expression (compile_time_ns self.module_name) self.module_name) 
 (expression.pop 0) 
 (raise (Py2HyReturnException (self._compile_branch expression)))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds "eval_when_compile") 
 (defn compile_eval_when_compile [self expression] 
 (try 
 (do 
 (assoc expression 0 (HySymbol "do")) 
 (hyhy.importer.hy_eval expression (compile_time_ns self.module_name) self.module_name) 
 (raise (Py2HyReturnException (Result)))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds HyCons) 
 (defn compile_cons [self cons] 
 (raise (HyTypeError cons "Can't compile a top-level cons cell")))) 
 (with_decorator 
 (builds HyInteger) 
 (defn compile_integer [self number] 
 (try 
 [(raise (Py2HyReturnException (ast.Num :n (long_type number) :lineno number.start_line :col_offset number.start_column)))] 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds HyFloat) 
 (defn compile_float [self number] 
 (try 
 [(raise (Py2HyReturnException (ast.Num :n (float number) :lineno number.start_line :col_offset number.start_column)))] 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds HyComplex) 
 (defn compile_complex [self number] 
 (try 
 [(raise (Py2HyReturnException (ast.Num :n (complex number) :lineno number.start_line :col_offset number.start_column)))] 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds HySymbol) 
 (defn compile_symbol [self symbol] 
 (try 
 (do 
 (when (in "." symbol) 
 (do 
 (setv _py2hy_anon_var_G_1267 (symbol.rsplit "." 1)) 
 (setv glob (nth _py2hy_anon_var_G_1267 0)) 
 (setv local (nth _py2hy_anon_var_G_1267 1))) 
 (when (not glob) 
 (raise (HyTypeError symbol ((. "cannot access attribute on anything other than a name (in order to get attributes ofexpressions, use `(. <expression> {attr})` or `(.{attr} <expression>)`)" format) :attr local)))) 
 (when (not local) 
 (raise (HyTypeError symbol "cannot access empty attribute"))) 
 (setv glob ((. (HySymbol glob) replace) symbol)) 
 (setv ret (self.compile_symbol glob)) 
 (setv ret (ast.Attribute :lineno symbol.start_line :col_offset symbol.start_column :value ret :attr (ast_str local) :ctx (ast.Load))) 
 (raise (Py2HyReturnException ret))) 
 (when (in symbol _stdlib) 
 ((. (get self.imports (get _stdlib symbol)) add) symbol)) 
 (raise (Py2HyReturnException (ast.Name :id (ast_str symbol) :arg (ast_str symbol) :ctx (ast.Load) :lineno symbol.start_line :col_offset symbol.start_column)))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds HyString) 
 (defn compile_string [self string] 
 (try 
 [(raise (Py2HyReturnException (ast.Str :s (str_type string) :lineno string.start_line :col_offset string.start_column)))] 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds HyBytes) 
 (defn compile_bytes [self bytestring] 
 (try 
 (do 
 (setv f (if PY3 
 ast.Bytes 
 ast.Str)) 
 (raise (Py2HyReturnException (f :s (bytes_type bytestring) :lineno bytestring.start_line :col_offset bytestring.start_column)))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds HyKeyword) 
 (defn compile_keyword [self keyword] 
 (try 
 [(raise (Py2HyReturnException (ast.Str :s (str_type keyword) :lineno keyword.start_line :col_offset keyword.start_column)))] 
 (except [e Py2HyReturnException] 
 e.retvalue)))) 
 (with_decorator 
 (builds HyDict) 
 (defn compile_dict [self m] 
 (try 
 (do 
 (do 
 (setv _py2hy_anon_var_G_1268 (self._compile_collect m :dict_display True)) 
 (setv keyvalues (nth _py2hy_anon_var_G_1268 0)) 
 (setv ret (nth _py2hy_anon_var_G_1268 1))) 
 (setv ret (+ ret (ast.Dict :lineno m.start_line :col_offset m.start_column :keys (get keyvalues (slice None None 2)) :values (get keyvalues (slice 1 None 2))))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))))
(defn hy_compile [tree module_name &optional [root ast.Module] [get_expr False]] 
 "
    Compile a HyObject tree into a Python AST Module.

    If `get_expr` is True, return a tuple (module, last_expression), where
    `last_expression` is the.
    " 
 (try 
 (do 
 (setv body []) 
 (setv expr None) 
 (when (not (isinstance tree HyObject)) 
 (setv tree (wrap_value tree)) 
 (when (not (isinstance tree HyObject)) 
 (raise (HyCompileError "`tree` must be a HyObject or capable of being promoted to one"))) 
 (spoof_positions tree)) 
 (setv compiler (HyASTCompiler module_name)) 
 (setv result (compiler.compile tree)) 
 (setv expr result.force_expr) 
 (when (not get_expr) 
 (setv result (+ result (result.expr_as_stmt)))) 
 (setv body (+ (compiler.imports_as_stmts tree) result.stmts)) 
 (setv ret (root :body body)) 
 (when get_expr 
 (setv expr (ast.Expression :body expr)) 
 (setv ret (, ret expr))) 
 (raise (Py2HyReturnException ret))) 
 (except [e Py2HyReturnException] 
 e.retvalue)))