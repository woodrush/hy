(import [hy])
(defclass Py2HyReturnException [Exception] (defn __init__ [self retvalue] (setv self.retvalue retvalue)))
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
(if PY3 (do (import [builtins])) (do (import [__builtin__ :as builtins])))
(do (setv _compile_time_ns {}))
(defn compile_time_ns [module_name] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv ns (_compile_time_ns.get module_name))) 
 (when (is ns None) (do (do (setv ns {"hyhy" hyhy "__name__" module_name})) 
 (do (assoc _compile_time_ns module_name ns)))) 
 (raise (Py2HyReturnException ns))) (except [e Py2HyReturnException] e.retvalue)))
(do (setv _stdlib {}))
(defn load_stdlib [] 
 (import [hyhy.core]) 
 (for [module hyhy.core.STDLIB] 
 (do (setv mod (importlib.import_module module))) 
 (for [e mod.EXPORTS] 
 (when (is_not (getattr mod e) (getattr builtins e "")) (do (do (assoc _stdlib e module)))))))
(defn _is_hy_builtin [name module_name] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv extras ["True" "False" "None"])) 
 (when (or (in name extras) (keyword.iskeyword name)) (do (raise (Py2HyReturnException True)))) 
 (when (not (module_name.startswith "hyhy.")) (do (raise (Py2HyReturnException (in name _compile_table))))) 
 (raise (Py2HyReturnException False))) (except [e Py2HyReturnException] e.retvalue)))
(do (setv _compile_table {}))
(defn ast_str [foobar] 
 "Using a hacky implementation of `return`" 
 (try (do (when PY3 (do (raise (Py2HyReturnException (str foobar))))) 
 (try (do (raise (Py2HyReturnException (str foobar)))) (except [e Py2HyReturnException] (raise e)) (except [UnicodeEncodeError] (do))) 
 (do (setv enc (codecs.getencoder "punycode"))) 
 (do (do (setv foobar (nth (enc foobar) 0)) (setv _ (nth (enc foobar) 1)))) 
 (raise (Py2HyReturnException (% "hy_%s" ((. (str foobar) replace) "-" "_"))))) (except [e Py2HyReturnException] e.retvalue)))
(defn builds [_type] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv unpythonic_chars ["-"])) 
 (do (setv really_ok ["-"])) 
 (when (any (genexpr (in x unpythonic_chars) [x (str_type _type)])) (do (when (not_in _type really_ok) (do (raise (TypeError (% "Dear Hypster: `build' needs to be *post* translated strings... `%s' sucks." _type))))))) 
 (defn _dec [fn_py2hy_mangling] 
 "Using a hacky implementation of `return`" 
 (try (do (do (assoc _compile_table _type fn_py2hy_mangling)) 
 (raise (Py2HyReturnException fn_py2hy_mangling))) (except [e Py2HyReturnException] e.retvalue))) 
 (raise (Py2HyReturnException _dec))) (except [e Py2HyReturnException] e.retvalue)))
(defn builds_if [_type condition] 
 "Using a hacky implementation of `return`" 
 (try (do (if condition (do (raise (Py2HyReturnException (builds _type)))) (do (raise (Py2HyReturnException (fn [fn_py2hy_mangling] fn_py2hy_mangling)))))) (except [e Py2HyReturnException] e.retvalue)))
(defn spoof_positions [obj] 
 "Using a hacky implementation of `return`" 
 (try (do (when (or (not (isinstance obj HyObject)) (isinstance obj HyCons)) (do (raise (Py2HyReturnException None)))) 
 (when (not (hasattr obj "start_column")) (do (do (setv obj.start_column 0)))) 
 (when (not (hasattr obj "start_line")) (do (do (setv obj.start_line 0)))) 
 (when (and (hasattr obj "__iter__") (not (isinstance obj (, string_types bytes_type)))) (do (for [x obj] 
 (spoof_positions x))))) (except [e Py2HyReturnException] e.retvalue)))
(defclass Result [object] "
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
 (do (setv __slots__ (, "imports" "stmts" "temp_variables" "_expr" "__used_expr" "contains_yield"))) 
 (defn __init__ [self &kwargs kwargs &rest args] 
 (when args (do (raise (TypeError "Yo: Hacker: don't pass me real args, dingus")))) 
 (do (setv self.imports (defaultdict set))) 
 (do (setv self.stmts [])) 
 (do (setv self.temp_variables [])) 
 (do (setv self._expr None)) 
 (do (setv self.contains_yield False)) 
 (do (setv self.__used_expr False)) 
 (for [kwarg kwargs] 
 (when (not_in kwarg ["imports" "contains_yield" "stmts" "expr" "temp_variables"]) (do (raise (TypeError (% "%s() got an unexpected keyword argument '%s'" (, self.__class__.__name__ kwarg)))))) 
 (setattr self kwarg (get kwargs kwarg)))) 
 (with_decorator 
 property 
 (defn expr [self] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv self.__used_expr True)) 
 (raise (Py2HyReturnException self._expr))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 expr.setter 
 (defn expr [self value] 
 (do (setv self.__used_expr False)) 
 (do (setv self._expr value)))) 
 (defn add_imports [self mod imports] 
 "Autoimport `imports` from `mod`" 
 ((. (get self.imports mod) update) imports)) 
 (defn is_expr [self] 
 "Check whether I am a pure expression" 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException (and self._expr (not (or self.imports self.stmts)))))) (except [e Py2HyReturnException] e.retvalue))) 
 (with_decorator 
 property 
 (defn force_expr [self] 
 "Force the expression context of the Result.

        If there is no expression context, we return a \"None\" expression.
        " 
 "Using a hacky implementation of `return`" 
 (try (do (when self.expr (do (raise (Py2HyReturnException self.expr)))) 
 (do (setv lineno 0)) 
 (do (setv col_offset 0)) 
 (when self.stmts (do (do (setv lineno (. (get self.stmts (- 1)) lineno))) 
 (do (setv col_offset (. (get self.stmts (- 1)) col_offset))))) 
 (raise (Py2HyReturnException (ast.Name :id (ast_str "None") :arg (ast_str "None") :ctx (ast.Load) :lineno lineno :col_offset col_offset)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (defn expr_as_stmt [self] 
 "Convert the Result's expression context to a statement

        This is useful when we want to use the stored expression in a
        statement context (for instance in a code branch).

        We drop ast.Names if they are appended to statements, as they
        can't have any side effect. \"Bare\" names still get converted to
        statements.

        If there is no expression context, return an empty result.
        " 
 "Using a hacky implementation of `return`" 
 (try (do (when (and self.expr (not (and (isinstance self.expr ast.Name) self.stmts))) (do (raise (Py2HyReturnException (+ (Result) (ast.Expr :lineno self.expr.lineno :col_offset self.expr.col_offset :value self.expr)))))) 
 (raise (Py2HyReturnException (Result)))) (except [e Py2HyReturnException] e.retvalue))) 
 (defn rename [self new_name] 
 "Rename the Result's temporary variables to a `new_name`.

        We know how to handle ast.Names and ast.FunctionDefs.
        " 
 (do (setv new_name (ast_str new_name))) 
 (for [var self.temp_variables] 
 (if (isinstance var ast.Name) (do (do (setv var.id new_name)) (do (setv var.arg new_name))) (do (if (isinstance var ast.FunctionDef) (do (do (setv var.name new_name))) (do (raise (TypeError (% "Don't know how to rename a %s!" var.__class__.__name__)))))))) 
 (do (setv self.temp_variables []))) 
 (defn __add__ [self other] 
 "Using a hacky implementation of `return`" 
 (try (do (when (isinstance other ast.stmt) (do (raise (Py2HyReturnException (+ self (Result :stmts [other])))))) 
 (when (isinstance other ast.expr) (do (raise (Py2HyReturnException (+ self (Result :expr other)))))) 
 (when (isinstance other ast.excepthandler) (do (raise (Py2HyReturnException (+ self (Result :stmts [other])))))) 
 (when (not (isinstance other Result)) (do (raise (TypeError (% "Can't add %r with non-compiler result %r" (, self other)))))) 
 (when (and self.expr (not self.__used_expr)) (do (traceback.print_stack) 
 (print (% "Bad boy clobbered expr %s with %s" (, (ast.dump self.expr) (ast.dump other.expr)))))) 
 (do (setv result (Result))) 
 (do (setv result.imports other.imports)) 
 (do (setv result.stmts (+ self.stmts other.stmts))) 
 (do (setv result.expr other.expr)) 
 (do (setv result.temp_variables other.temp_variables)) 
 (do (setv result.contains_yield False)) 
 (when (or self.contains_yield other.contains_yield) (do (do (setv result.contains_yield True)))) 
 (raise (Py2HyReturnException result))) (except [e Py2HyReturnException] e.retvalue))) 
 (defn __str__ [self] 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException (% "Result(imports=[%s], stmts=[%s], expr=%s, contains_yield=%s)" (, ((. ", " join) (genexpr (ast.dump x) [x self.imports])) ((. ", " join) (genexpr (ast.dump x) [x self.stmts])) (if self.expr (ast.dump self.expr) None) self.contains_yield))))) (except [e Py2HyReturnException] e.retvalue))))
(defn _branch [results] 
 "Make a branch out of a list of Result objects

    This generates a Result from the given sequence of Results, forcing each
    expression context as a statement before the next result is used.

    We keep the expression context of the last argument for the returned Result
    " 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv results (list results))) 
 (do (setv ret (Result))) 
 (for [result (get results (slice None (- 1) None))] 
 (setv ret (+ ret result)) 
 (setv ret (+ ret (result.expr_as_stmt)))) 
 (for [result (get results (slice (- 1) None None))] 
 (setv ret (+ ret result))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))
(defn _raise_wrong_args_number [expression error] 
 (raise (HyTypeError expression (% error (, (expression.pop 0) (len expression))))))
(defn checkargs [&optional [exact None] [min None] [max None] [even None] [multiple None]] 
 "Using a hacky implementation of `return`" 
 (try (do (defn _dec [fn_py2hy_mangling] 
 "Using a hacky implementation of `return`" 
 (try (do (defn checker [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (when (and (is_not exact None) (!= (- (len expression) 1) exact)) (do (_raise_wrong_args_number expression (% "`%%s' needs %d arguments, got %%d" exact)))) 
 (when (and (is_not min None) (< (- (len expression) 1) min)) (do (_raise_wrong_args_number expression (% "`%%s' needs at least %d arguments, got %%d." min)))) 
 (when (and (is_not max None) (> (- (len expression) 1) max)) (do (_raise_wrong_args_number expression (% "`%%s' needs at most %d arguments, got %%d" max)))) 
 (do (setv is_even (not (% (- (len expression) 1) 2)))) 
 (when (and (is_not even None) (!= is_even even)) (do (do (setv even_str (if even "even" "odd"))) 
 (_raise_wrong_args_number expression (% "`%%s' needs an %s number of arguments, got %%d" even_str)))) 
 (when (is_not multiple None) (do (when (not (in (- (len expression) 1) multiple)) (do (do (setv choices ((. ", " join) (list_comp (str val) [val (get multiple (slice None (- 1) None))])))) 
 (setv choices (+ choices (% " or %s" (get multiple (- 1))))) 
 (_raise_wrong_args_number expression (% "`%%s' needs %s arguments, got %%d" choices)))))) 
 (raise (Py2HyReturnException (fn_py2hy_mangling self expression)))) (except [e Py2HyReturnException] e.retvalue))) 
 (raise (Py2HyReturnException checker))) (except [e Py2HyReturnException] e.retvalue))) 
 (raise (Py2HyReturnException _dec))) (except [e Py2HyReturnException] e.retvalue)))
(defn is_unpack [kind x] 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException (and (isinstance x HyExpression) (> (len x) 0) (isinstance (get x 0) HySymbol) (= (get x 0) (+ "unpack_" kind)))))) (except [e Py2HyReturnException] e.retvalue)))
(defclass HyASTCompilerHelper [object] (defn __init__ [self module_name] 
 (do (setv self.allow_builtins (module_name.startswith "hyhy.core"))) 
 (do (setv self.anon_fn_count 0)) 
 (do (setv self.anon_var_count 0)) 
 (do (setv self.imports (defaultdict set))) 
 (do (setv self.module_name module_name)) 
 (do (setv self.temp_if None)) 
 (when (not (module_name.startswith "hyhy.core")) (do (load_stdlib)))) 
 (defn get_anon_var [self] 
 "Using a hacky implementation of `return`" 
 (try (do (setv self.anon_var_count (+ self.anon_var_count 1)) 
 (raise (Py2HyReturnException (% "_hy_anon_var_%s" self.anon_var_count)))) (except [e Py2HyReturnException] e.retvalue))) 
 (defn get_anon_fn [self] 
 "Using a hacky implementation of `return`" 
 (try (do (setv self.anon_fn_count (+ self.anon_fn_count 1)) 
 (raise (Py2HyReturnException (% "_hy_anon_fn_%d" self.anon_fn_count)))) (except [e Py2HyReturnException] e.retvalue))) 
 (defn update_imports [self result] 
 "Retrieve the imports from the result object" 
 (for [mod result.imports] 
 ((. (get self.imports mod) update) (get result.imports mod)))) 
 (defn imports_as_stmts [self expr] 
 "Convert the Result's imports to statements" 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv ret (Result))) 
 (for [[module names] (self.imports.items)] 
 (when (in None names) (do (do (setv e ((. (HyExpression [(HySymbol "import") (HySymbol module)]) replace) expr))) 
 (spoof_positions e) 
 (setv ret (+ ret (self.compile e))))) 
 (do (setv names (sorted (genexpr name [name names] (and name))))) 
 (when names (do (do (setv e ((. (HyExpression [(HySymbol "import") (HyList [(HySymbol module) (HyList (list_comp (HySymbol name) [name names]))])]) replace) expr))) 
 (spoof_positions e) 
 (setv ret (+ ret (self.compile e)))))) 
 (do (setv self.imports (defaultdict set))) 
 (raise (Py2HyReturnException ret.stmts))) (except [e Py2HyReturnException] e.retvalue))) 
 (defn compile_atom [self atom_type atom] 
 "Using a hacky implementation of `return`" 
 (try (do (when (in atom_type _compile_table) (do (do (setv ret ((get _compile_table atom_type) self atom))) 
 (when (not (isinstance ret Result)) (do (do (setv ret (+ (Result) ret))))) 
 (raise (Py2HyReturnException ret)))) 
 (when (not (isinstance atom HyObject)) (do (do (setv atom (wrap_value atom))) 
 (when (isinstance atom HyObject) (do (spoof_positions atom) 
 (raise (Py2HyReturnException (self.compile_atom (type atom) atom)))))))) (except [e Py2HyReturnException] e.retvalue))) 
 (defn compile [self tree] 
 "Using a hacky implementation of `return`" 
 (try (do (try (do (do (setv _type (type tree))) 
 (do (setv ret (self.compile_atom _type tree))) 
 (when ret (do (self.update_imports ret) 
 (raise (Py2HyReturnException ret))))) (except [e Py2HyReturnException] (raise e)) (except [HyCompileError] (raise)) (except [e HyTypeError] (raise)) (except [e Exception] (raise_empty HyCompileError e (get (sys.exc_info) 2)))) 
 (raise (HyCompileError (Exception (% "Unknown type: `%s'" _type))))) (except [e Py2HyReturnException] e.retvalue))) 
 (defn _compile_collect [self exprs &optional [with_kwargs False] [dict_display False] [oldpy_unpack False]] 
 "Collect the expression contexts from a list of compiled expression.

        This returns a list of the expression contexts, and the sum of the
        Result objects passed as arguments.

        " 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv compiled_exprs [])) 
 (do (setv ret (Result))) 
 (do (setv keywords [])) 
 (do (setv oldpy_starargs None)) 
 (do (setv oldpy_kwargs None)) 
 (do (setv exprs_iter (iter exprs))) 
 (for [expr exprs_iter] 
 (if (and (not PY35) oldpy_unpack (is_unpack "iterable" expr)) (do (when oldpy_starargs (do (raise (HyTypeError expr "Pythons < 3.5 allow only one `unpack-iterable` per call")))) (do (setv oldpy_starargs (self.compile (get expr 1)))) (setv ret (+ ret oldpy_starargs)) (do (setv oldpy_starargs oldpy_starargs.force_expr))) (do (if (is_unpack "mapping" expr) (do (setv ret (+ ret (self.compile (get expr 1)))) (if PY35 (do (if dict_display (do (compiled_exprs.append None) (compiled_exprs.append ret.force_expr)) (do (when with_kwargs (do (keywords.append (ast.keyword :arg None :value ret.force_expr :lineno expr.start_line :col_offset expr.start_column))))))) (do (when oldpy_unpack (do (when oldpy_kwargs (do (raise (HyTypeError expr "Pythons < 3.5 allow only one `unpack-mapping` per call")))) 
 (do (setv oldpy_kwargs ret.force_expr))))))) (do (if (and with_kwargs (isinstance expr HyKeyword)) (do (try (do (do (setv value (next exprs_iter)))) (except [e Py2HyReturnException] (raise e)) (except [StopIteration] (raise (HyTypeError expr ((. "Keyword argument {kw} needs a value." format) :kw (str (get expr (slice 1 None None)))))))) (do (setv compiled_value (self.compile value))) (setv ret (+ ret compiled_value)) (do (setv keyword (str (get expr (slice 2 None None))))) (when (and (in "-" keyword) (!= keyword "-")) (do (do (setv keyword (keyword.replace "-" "_"))))) (keywords.append (ast.keyword :arg keyword :value compiled_value.force_expr :lineno expr.start_line :col_offset expr.start_column))) (do (setv ret (+ ret (self.compile expr))) (compiled_exprs.append ret.force_expr)))))))) 
 (if oldpy_unpack (do (raise (Py2HyReturnException (, compiled_exprs ret keywords oldpy_starargs oldpy_kwargs)))) (do (raise (Py2HyReturnException (, compiled_exprs ret keywords)))))) (except [e Py2HyReturnException] e.retvalue))) 
 (defn _compile_branch [self exprs] 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException (_branch (genexpr (self.compile expr) [expr exprs]))))) (except [e Py2HyReturnException] e.retvalue))) 
 (defn _parse_lambda_list [self exprs] 
 " Return FunctionDef parameter values from lambda list." 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv ll_keywords (, "&rest" "&optional" "&key" "&kwonly" "&kwargs"))) 
 (do (setv ret (Result))) 
 (do (setv args [])) 
 (do (setv defaults [])) 
 (do (setv varargs None)) 
 (do (setv kwonlyargs [])) 
 (do (setv kwonlydefaults [])) 
 (do (setv kwargs None)) 
 (do (setv lambda_keyword None)) 
 (for [expr exprs] 
 (when (in expr ll_keywords) (do (if (= expr "&optional") (do (when (> (len defaults) 0) (do (raise (HyTypeError expr "There can only be &optional arguments or one &key argument")))) (do (setv lambda_keyword expr))) (do (if (in expr (, "&rest" "&key" "&kwonly" "&kwargs")) (do (do (setv lambda_keyword expr))) (do (raise (HyTypeError expr ((. "{0} is in an invalid position." format) (repr expr)))))))) 
 (continue))) 
 (if (is lambda_keyword None) (do (args.append expr)) (do (if (= lambda_keyword "&rest") (do (when varargs (do (raise (HyTypeError expr "There can only be one &rest argument")))) (do (setv varargs expr))) (do (if (= lambda_keyword "&key") (do (if (!= (type expr) HyDict) (do (raise (HyTypeError expr "There can only be one &key argument"))) (do (when (> (len defaults) 0) (do (raise (HyTypeError expr "There can only be &optional arguments or one &key argument")))) (do (setv it (iter expr))) (for [[k v] (zip it it)] 
 (when (not (isinstance k HyString)) (do (raise (HyTypeError expr "Only strings can be used as parameter names")))) 
 (args.append k) 
 (setv ret (+ ret (self.compile v))) 
 (defaults.append ret.force_expr))))) (do (if (= lambda_keyword "&optional") (do (if (isinstance expr HyList) (do (when (not (= (len expr) 2)) (do (raise (HyTypeError expr "optional args should be bare names or 2-item lists")))) (do (do (setv k (nth expr 0)) (setv v (nth expr 1))))) (do (do (setv k expr)) (do (setv v ((. (HySymbol "None") replace) k))))) (when (not (isinstance k HyString)) (do (raise (HyTypeError expr "Only strings can be used as parameter names")))) (args.append k) (setv ret (+ ret (self.compile v))) (defaults.append ret.force_expr)) (do (if (= lambda_keyword "&kwonly") (do (when (not PY3) (do (raise (HyTypeError expr "keyword-only arguments are only available under Python 3")))) (if (isinstance expr HyList) (do (when (!= (len expr) 2) (do (raise (HyTypeError expr "keyword-only args should be bare names or 2-item lists")))) (do (do (setv k (nth expr 0)) (setv v (nth expr 1)))) (kwonlyargs.append k) (setv ret (+ ret (self.compile v))) (kwonlydefaults.append ret.force_expr)) (do (do (setv k expr)) (kwonlyargs.append k) (kwonlydefaults.append None)))) (do (when (= lambda_keyword "&kwargs") (do (when kwargs (do (raise (HyTypeError expr "There can only be one &kwargs argument")))) 
 (do (setv kwargs expr))))))))))))))) 
 (raise (Py2HyReturnException (, ret args defaults varargs kwonlyargs kwonlydefaults kwargs)))) (except [e Py2HyReturnException] e.retvalue))) 
 (defn _storeize [self expr name &optional [func None]] 
 "Return a new `name` object with an ast.Store() context" 
 "Using a hacky implementation of `return`" 
 (try (do (when (not func) (do (do (setv func ast.Store)))) 
 (when (isinstance name Result) (do (when (not (name.is_expr)) (do (raise (HyTypeError expr "Can't assign or delete a non-expression")))) 
 (do (setv name name.expr)))) 
 (if (isinstance name (, ast.Tuple ast.List)) (do (do (setv typ (type name))) (do (setv new_elts [])) (for [x name.elts] 
 (new_elts.append (self._storeize expr x func))) (do (setv new_name (typ :elts new_elts)))) (do (if (isinstance name ast.Name) (do (do (setv new_name (ast.Name :id name.id :arg name.arg)))) (do (if (isinstance name ast.Subscript) (do (do (setv new_name (ast.Subscript :value name.value :slice name.slice)))) (do (if (isinstance name ast.Attribute) (do (do (setv new_name (ast.Attribute :value name.value :attr name.attr)))) (do (if (and PY3 (isinstance name ast.Starred)) (do (do (setv new_name (ast.Starred :value (self._storeize expr name.value func))))) (do (raise (HyTypeError expr (% "Can't assign or delete a %s" (. (type expr) __name__)))))))))))))) 
 (do (setv new_name.ctx (func))) 
 (ast.copy_location new_name name) 
 (raise (Py2HyReturnException new_name))) (except [e Py2HyReturnException] e.retvalue))) 
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
 "Using a hacky implementation of `return`" 
 (try (do (when (= level 0) (do (when (isinstance form HyExpression) (do (when (and form (in (get form 0) (, "unquote" "unquote_splice"))) (do (when (!= (len form) 2) (do (raise (HyTypeError form (, (% "`%s' needs 1 argument, got %s" (get form 0)) (- (len form) 1)))))) 
 (raise (Py2HyReturnException (, (set) (get form 1) (= (get form 0) "unquote_splice")))))))))) 
 (when (isinstance form HyExpression) (do (when (and form (= (get form 0) "quasiquote")) (do (setv level (+ level 1)))) 
 (when (and form (in (get form 0) (, "unquote" "unquote_splice"))) (do (setv level (- level 1)))))) 
 (do (setv name form.__class__.__name__)) 
 (do (setv imports (set [name]))) 
 (if (isinstance form (, HyList HyDict HySet)) (do (if (not form) (do (do (setv contents (HyList)))) (do (do (setv contents (HyExpression [(HySymbol "+") (HyList)]))))) (for [x form] 
 (do (do (setv f_imports (nth (self._render_quoted_form x level) 0)) (setv f_contents (nth (self._render_quoted_form x level) 1)) (setv splice (nth (self._render_quoted_form x level) 2)))) 
 (imports.update f_imports) 
 (if splice (do (do (setv to_add (HyExpression [(HySymbol "list") (HyExpression [(HySymbol "or") f_contents (HyList)])])))) (do (do (setv to_add (HyList [f_contents]))))) 
 (contents.append to_add)) (raise (Py2HyReturnException (, imports ((. (HyExpression [(HySymbol name) contents]) replace) form) False)))) (do (if (isinstance form HyCons) (do (do (setv ret (HyExpression [(HySymbol name)]))) (do (do (setv nimport (nth (self._render_quoted_form form.car level) 0)) (setv contents (nth (self._render_quoted_form form.car level) 1)) (setv splice (nth (self._render_quoted_form form.car level) 2)))) (when splice (do (raise (HyTypeError form "Can't splice dotted lists yet")))) (imports.update nimport) (ret.append contents) (do (do (setv nimport (nth (self._render_quoted_form form.cdr level) 0)) (setv contents (nth (self._render_quoted_form form.cdr level) 1)) (setv splice (nth (self._render_quoted_form form.cdr level) 2)))) (when splice (do (raise (HyTypeError form "Can't splice the cdr of a cons")))) (imports.update nimport) (ret.append contents) (raise (Py2HyReturnException (, imports (ret.replace form) False)))) (do (when (isinstance form HySymbol) (do (raise (Py2HyReturnException (, imports ((. (HyExpression [(HySymbol name) (HyString form)]) replace) form) False))))))))) 
 (raise (Py2HyReturnException (, imports ((. (HyExpression [(HySymbol name) form]) replace) form) False)))) (except [e Py2HyReturnException] e.retvalue))) 
 (with_decorator 
 (builds "quote") (builds "quasiquote") (checkargs :exact 1) 
 (defn compile_quote [self entries] 
 "Using a hacky implementation of `return`" 
 (try (do (if (= (get entries 0) "quote") (do (do (setv level (float "inf")))) (do (do (setv level 0)))) 
 (do (do (setv imports (nth (self._render_quoted_form (get entries 1) level) 0)) (setv stmts (nth (self._render_quoted_form (get entries 1) level) 1)) (setv splice (nth (self._render_quoted_form (get entries 1) level) 2)))) 
 (do (setv ret (self.compile stmts))) 
 (ret.add_imports "hyhy" imports) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "unquote") (builds "unquote_splicing") 
 (defn compile_unquote [self expr] 
 (raise (HyTypeError expr (% "`%s' can't be used at the top-level" (get expr 0)))))) 
 (with_decorator 
 (builds "unpack_iterable") (checkargs :exact 1) 
 (defn compile_unpack_iterable [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (when (not PY3) (do (raise (HyTypeError expr "`unpack-iterable` isn't allowed here")))) 
 (do (setv ret (self.compile (get expr 1)))) 
 (setv ret (+ ret (ast.Starred :value ret.force_expr :lineno expr.start_line :col_offset expr.start_column :ctx (ast.Load)))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "unpack_mapping") (checkargs :exact 1) 
 (defn compile_unpack_mapping [self expr] 
 (raise (HyTypeError expr "`unpack-mapping` isn't allowed here")))) 
 (with_decorator 
 (builds "do") 
 (defn compile_do [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (expression.pop 0) 
 (raise (Py2HyReturnException (self._compile_branch expression)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "raise") (checkargs :multiple [0 1 3]) 
 (defn compile_raise_expression [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (expr.pop 0) 
 (do (setv ret (Result))) 
 (when expr (do (setv ret (+ ret (self.compile (expr.pop 0)))))) 
 (do (setv cause None)) 
 (when (and (= (len expr) 2) (= (get expr 0) (HyKeyword ":from"))) (do (when (not PY3) (do (raise (HyCompileError "raise from only supported in python 3")))) 
 (expr.pop 0) 
 (do (setv cause (self.compile (expr.pop 0)))) 
 (do (setv cause cause.expr)))) 
 (setv ret (+ ret (ast.Raise :lineno expr.start_line :col_offset expr.start_column :type ret.expr :exc ret.expr :inst None :tback None :cause cause))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "try") (checkargs :min 2) 
 (defn compile_try_expression [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (expr.pop 0) 
 (do (setv body (self.compile (if expr (expr.pop 0) [])))) 
 (do (setv var (self.get_anon_var))) 
 (do (setv name (ast.Name :id (ast_str var) :arg (ast_str var) :ctx (ast.Store) :lineno expr.start_line :col_offset expr.start_column))) 
 (do (setv expr_name (ast.Name :id (ast_str var) :arg (ast_str var) :ctx (ast.Load) :lineno expr.start_line :col_offset expr.start_column))) 
 (do (setv returnable (Result :expr expr_name :temp_variables [expr_name name] :contains_yield body.contains_yield))) 
 (when (not (all expr)) (do (raise (HyTypeError expr "Empty list not allowed in `try'")))) 
 (do (setv handler_results (Result))) 
 (do (setv handlers [])) 
 (while (and expr (= (get (get expr 0) 0) (HySymbol "except"))) (setv handler_results (+ handler_results (self._compile_catch_expression (expr.pop 0) name))) (handlers.append (handler_results.stmts.pop))) 
 (do (setv orelse [])) 
 (when (and expr (= (get (get expr 0) 0) (HySymbol "else"))) (do (do (setv orelse (self._compile_branch (get (expr.pop 0) (slice 1 None None))))) 
 (setv orelse (+ orelse (ast.Assign :targets [name] :value orelse.force_expr :lineno expr.start_line :col_offset expr.start_column))) 
 (setv orelse (+ orelse (orelse.expr_as_stmt))) 
 (do (setv orelse orelse.stmts)))) 
 (do (setv finalbody [])) 
 (when (and expr (= (get (get expr 0) 0) (HySymbol "finally"))) (do (do (setv finalbody (self._compile_branch (get (expr.pop 0) (slice 1 None None))))) 
 (setv finalbody (+ finalbody (finalbody.expr_as_stmt))) 
 (do (setv finalbody finalbody.stmts)))) 
 (when expr (do (when (in (get (get expr 0) 0) (, "except" "else" "finally")) (do (raise (HyTypeError expr "Incorrect order of `except'/`else'/`finally' in `try'")))) 
 (raise (HyTypeError expr "Unknown expression in `try'")))) 
 (when (and orelse (not handlers)) (do (raise (HyTypeError expr "`try' cannot have `else' without `except'")))) 
 (when (not (or handlers finalbody)) (do (raise (HyTypeError expr "`try' must have an `except' or `finally' clause")))) 
 (do (setv ret handler_results)) 
 (setv body (+ body (if orelse (body.expr_as_stmt) (ast.Assign :targets [name] :value body.force_expr :lineno expr.start_line :col_offset expr.start_column)))) 
 (do (setv body (or body.stmts [(ast.Pass :lineno expr.start_line :col_offset expr.start_column)]))) 
 (when PY3 (do (raise (Py2HyReturnException (+ (+ ret (ast.Try :lineno expr.start_line :col_offset expr.start_column :body body :handlers handlers :orelse orelse :finalbody finalbody)) returnable))))) 
 (when finalbody (do (when handlers (do (raise (Py2HyReturnException (+ (+ ret (ast.TryFinally :lineno expr.start_line :col_offset expr.start_column :body [(ast.TryExcept :lineno expr.start_line :col_offset expr.start_column :handlers handlers :body body :orelse orelse)] :finalbody finalbody)) returnable))))) 
 (raise (Py2HyReturnException (+ (+ ret (ast.TryFinally :lineno expr.start_line :col_offset expr.start_column :body body :finalbody finalbody)) returnable))))) 
 (raise (Py2HyReturnException (+ (+ ret (ast.TryExcept :lineno expr.start_line :col_offset expr.start_column :handlers handlers :body body :orelse orelse)) returnable)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "except") 
 (defn magic_internal_form [self expr] 
 (raise (HyTypeError expr (% "Error: `%s' can't be used like that." (get expr 0)))))) 
 (defn _compile_catch_expression [self expr var] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv catch (expr.pop 0))) 
 (do (setv exceptions (if expr (expr.pop 0) (HyList)))) 
 (when (not (isinstance exceptions HyList)) (do (raise (HyTypeError exceptions (% "`%s' exceptions list is not a list" catch))))) 
 (when (> (len exceptions) 2) (do (raise (HyTypeError exceptions (% "`%s' exceptions list is too long" catch))))) 
 (do (setv name None)) 
 (when (= (len exceptions) 2) (do (do (setv name (exceptions.pop 0))) 
 (when (not (isinstance name HySymbol)) (do (raise (HyTypeError exceptions "Exception storage target name must be a symbol.")))) 
 (if PY3 (do (do (setv name (ast_str name)))) (do (do (setv name (self._storeize name (self.compile name)))))))) 
 (do (setv exceptions_list (if exceptions (exceptions.pop 0) []))) 
 (if (isinstance exceptions_list list) (do (if (len exceptions_list) (do (do (do (setv elts (nth (self._compile_collect exceptions_list) 0)) (setv _type (nth (self._compile_collect exceptions_list) 1)) (setv _ (nth (self._compile_collect exceptions_list) 2)))) (setv _type (+ _type (ast.Tuple :elts elts :lineno expr.start_line :col_offset expr.start_column :ctx (ast.Load))))) (do (do (setv _type (Result)))))) (do (if (isinstance exceptions_list HySymbol) (do (do (setv _type (self.compile exceptions_list)))) (do (raise (HyTypeError exceptions (% "`%s' needs a valid exception list" catch))))))) 
 (do (setv body (self._compile_branch expr))) 
 (setv body (+ body (ast.Assign :targets [var] :value body.force_expr :lineno expr.start_line :col_offset expr.start_column))) 
 (setv body (+ body (body.expr_as_stmt))) 
 (do (setv body body.stmts)) 
 (when (not body) (do (do (setv body [(ast.Pass :lineno expr.start_line :col_offset expr.start_column)])))) 
 (raise (Py2HyReturnException (+ _type (ast.ExceptHandler :lineno expr.start_line :col_offset expr.start_column :type _type.expr :name name :body body))))) (except [e Py2HyReturnException] e.retvalue))) 
 (with_decorator 
 (builds "if*") (checkargs :min 2 :max 3) 
 (defn compile_if [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (expression.pop 0) 
 (do (setv cond (self.compile (expression.pop 0)))) 
 (do (setv body (self.compile (expression.pop 0)))) 
 (do (setv orel (Result))) 
 (do (setv _py2hy_anon_var_G_1235 False) (setv nested _py2hy_anon_var_G_1235) (setv root _py2hy_anon_var_G_1235)) 
 (when expression (do (do (setv orel_expr (expression.pop 0))) 
 (when (and (isinstance orel_expr HyExpression) (isinstance (get orel_expr 0) HySymbol) (= (get orel_expr 0) "if*")) (do (do (setv root (is self.temp_if None))) 
 (do (setv nested True)) 
 (do (setv self.temp_if (or self.temp_if (self.get_anon_var)))))) 
 (do (setv orel (self.compile orel_expr))))) 
 (when (and (not cond.stmts) (isinstance cond.force_expr ast.Name)) (do (do (setv name cond.force_expr.id)) 
 (do (setv branch None)) 
 (if (= name "True") (do (do (setv branch body))) (do (when (in name (, "False" "None")) (do (do (setv branch orel)))))) 
 (when (is_not branch None) (do (when (and self.temp_if branch.stmts) (do (do (setv name (ast.Name :id (ast_str self.temp_if) :arg (ast_str self.temp_if) :ctx (ast.Store) :lineno expression.start_line :col_offset expression.start_column))) 
 (setv branch (+ branch (ast.Assign :targets [name] :value body.force_expr :lineno expression.start_line :col_offset expression.start_column))))) 
 (raise (Py2HyReturnException branch)))))) 
 (do (setv ret cond)) 
 (if (or body.stmts orel.stmts) (do (do (setv var (or self.temp_if (self.get_anon_var)))) (do (setv name (ast.Name :id (ast_str var) :arg (ast_str var) :ctx (ast.Store) :lineno expression.start_line :col_offset expression.start_column))) (setv body (+ body (ast.Assign :targets [name] :value body.force_expr :lineno expression.start_line :col_offset expression.start_column))) (when (or (not nested) (not orel.stmts) (and (not root) (!= var self.temp_if))) (do (setv orel (+ orel (ast.Assign :targets [name] :value orel.force_expr :lineno expression.start_line :col_offset expression.start_column))))) (setv ret (+ ret (ast.If :test ret.force_expr :body body.stmts :orelse orel.stmts :lineno expression.start_line :col_offset expression.start_column))) (do (setv expr_name (ast.Name :id (ast_str var) :arg (ast_str var) :ctx (ast.Load) :lineno expression.start_line :col_offset expression.start_column))) (setv ret (+ ret (Result :expr expr_name :temp_variables [expr_name name])))) (do (setv ret (+ ret (ast.IfExp :test ret.force_expr :body body.force_expr :orelse orel.force_expr :lineno expression.start_line :col_offset expression.start_column))))) 
 (when root (do (do (setv self.temp_if None)))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "break") 
 (defn compile_break_expression [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv ret (ast.Break :lineno expr.start_line :col_offset expr.start_column))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "continue") 
 (defn compile_continue_expression [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv ret (ast.Continue :lineno expr.start_line :col_offset expr.start_column))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "assert") (checkargs :min 1 :max 2) 
 (defn compile_assert_expression [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (expr.pop 0) 
 (do (setv e (expr.pop 0))) 
 (if (= (len expr) 1) (do (do (setv msg (. (self.compile (expr.pop 0)) force_expr)))) (do (do (setv msg None)))) 
 (do (setv ret (self.compile e))) 
 (setv ret (+ ret (ast.Assert :test ret.force_expr :msg msg :lineno e.start_line :col_offset e.start_column))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "global") (checkargs :min 1) 
 (defn compile_global_expression [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (expr.pop 0) 
 (do (setv names [])) 
 (while (> (len expr) 0) (do (setv identifier (expr.pop 0))) (do (setv name (ast_str identifier))) (names.append name) (when (not (isinstance identifier HySymbol)) (do (raise (HyTypeError identifier "(global) arguments must  be Symbols"))))) 
 (raise (Py2HyReturnException (ast.Global :names names :lineno expr.start_line :col_offset expr.start_column)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "nonlocal") (checkargs :min 1) 
 (defn compile_nonlocal_expression [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (when (not PY3) (do (raise (HyCompileError "nonlocal only supported in python 3!")))) 
 (expr.pop 0) 
 (do (setv names [])) 
 (while (> (len expr) 0) (do (setv identifier (expr.pop 0))) (do (setv name (ast_str identifier))) (names.append name) (when (not (isinstance identifier HySymbol)) (do (raise (HyTypeError identifier "(nonlocal) arguments must be Symbols."))))) 
 (raise (Py2HyReturnException (ast.Nonlocal :names names :lineno expr.start_line :col_offset expr.start_column)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "yield") (checkargs :max 1) 
 (defn compile_yield_expression [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (expr.pop 0) 
 (do (setv ret (Result :contains_yield (not PY3)))) 
 (do (setv value None)) 
 (when (!= expr []) (do (setv ret (+ ret (self.compile (expr.pop 0)))) 
 (do (setv value ret.force_expr)))) 
 (setv ret (+ ret (ast.Yield :value value :lineno expr.start_line :col_offset expr.start_column))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds_if "yield_from" PY3) (checkargs :max 1) 
 (defn compile_yield_from_expression [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (expr.pop 0) 
 (do (setv ret (Result :contains_yield True))) 
 (do (setv value None)) 
 (when (!= expr []) (do (setv ret (+ ret (self.compile (expr.pop 0)))) 
 (do (setv value ret.force_expr)))) 
 (setv ret (+ ret (ast.YieldFrom :value value :lineno expr.start_line :col_offset expr.start_column))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "import") 
 (defn compile_import_expression [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (defn _compile_import [expr module &optional [names None] [importer ast.Import]] 
 "Using a hacky implementation of `return`" 
 (try (do (when (not names) (do (do (setv names [(ast.alias :name (ast_str module) :asname None)])))) 
 (do (setv ret (importer :lineno expr.start_line :col_offset expr.start_column :module (ast_str module) :names names :level 0))) 
 (raise (Py2HyReturnException (+ (Result) ret)))) (except [e Py2HyReturnException] e.retvalue))) 
 (expr.pop 0) 
 (do (setv rimports (Result))) 
 (while (> (len expr) 0) (do (setv iexpr (expr.pop 0))) (when (not (isinstance iexpr (, HySymbol HyList))) (do (raise (HyTypeError iexpr "(import) requires a Symbol or a List.")))) (when (isinstance iexpr HySymbol) (do (setv rimports (+ rimports (_compile_import expr iexpr))) 
 (continue))) (when (and (isinstance iexpr HyList) (= (len iexpr) 1)) (do (setv rimports (+ rimports (_compile_import expr (iexpr.pop 0)))) 
 (continue))) (when (and (isinstance iexpr HyList) iexpr) (do (do (setv module (iexpr.pop 0))) 
 (do (setv entry (get iexpr 0))) 
 (when (and (isinstance entry HyKeyword) (= entry (HyKeyword ":as"))) (do (when (not (= (len iexpr) 2)) (do (raise (HyTypeError iexpr "garbage after aliased import")))) 
 (iexpr.pop 0) 
 (do (setv alias (iexpr.pop 0))) 
 (do (setv names [(ast.alias :name (ast_str module) :asname (ast_str alias))])) 
 (setv rimports (+ rimports (_compile_import expr (ast_str module) names))) 
 (continue))) 
 (when (isinstance entry HyList) (do (do (setv names [])) 
 (while entry (do (setv sym (entry.pop 0))) (if (and entry (isinstance (get entry 0) HyKeyword)) (do (entry.pop 0) (do (setv alias (ast_str (entry.pop 0))))) (do (do (setv alias None)))) (names.append (ast.alias :name (ast_str sym) :asname alias))) 
 (setv rimports (+ rimports (_compile_import expr module names ast.ImportFrom))) 
 (continue))) 
 (raise (HyTypeError entry (% "Unknown entry (`%s`) in the HyList" entry)))))) 
 (raise (Py2HyReturnException rimports))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "get") (checkargs :min 2) 
 (defn compile_index_expression [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (expr.pop 0) 
 (do (setv val (self.compile (expr.pop 0)))) 
 (do (do (setv slices (nth (self._compile_collect expr) 0)) (setv ret (nth (self._compile_collect expr) 1)) (setv _ (nth (self._compile_collect expr) 2)))) 
 (when val.stmts (do (setv ret (+ ret val)))) 
 (for [sli slices] 
 (do (setv val (+ (Result) (ast.Subscript :lineno expr.start_line :col_offset expr.start_column :value val.force_expr :slice (ast.Index :value sli) :ctx (ast.Load)))))) 
 (raise (Py2HyReturnException (+ ret val)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds ".") (checkargs :min 1) 
 (defn compile_attribute_access [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (expr.pop 0) 
 (do (setv ret (self.compile (expr.pop 0)))) 
 (for [attr expr] 
 (if (isinstance attr HySymbol) (do (setv ret (+ ret (ast.Attribute :lineno attr.start_line :col_offset attr.start_column :value ret.force_expr :attr (ast_str attr) :ctx (ast.Load))))) (do (if (= (type attr) HyList) (do (when (!= (len attr) 1) (do (raise (HyTypeError attr ((. "The attribute access DSL only accepts HySymbols and one-item lists, got {0}-item list instead" format) (len attr)))))) (do (setv compiled_attr (self.compile (attr.pop 0)))) (do (setv ret (+ (+ compiled_attr ret) (ast.Subscript :lineno attr.start_line :col_offset attr.start_column :value ret.force_expr :slice (ast.Index :value compiled_attr.force_expr) :ctx (ast.Load)))))) (do (raise (HyTypeError attr ((. "The attribute access DSL only accepts HySymbols and one-item lists, got {0} instead" format) (. (type attr) __name__))))))))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "del") 
 (defn compile_del_expression [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv root (expr.pop 0))) 
 (when (not expr) (do (do (setv result (Result))) 
 (setv result (+ result (ast.Name :id "None" :ctx (ast.Load) :lineno root.start_line :col_offset root.start_column))) 
 (raise (Py2HyReturnException result)))) 
 (do (setv del_targets [])) 
 (do (setv ret (Result))) 
 (for [target expr] 
 (do (setv compiled_target (self.compile target))) 
 (setv ret (+ ret compiled_target)) 
 (del_targets.append (self._storeize target compiled_target ast.Del))) 
 (raise (Py2HyReturnException (+ ret (ast.Delete :lineno expr.start_line :col_offset expr.start_column :targets del_targets))))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "cut") (checkargs :min 1 :max 4) 
 (defn compile_cut_expression [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (expr.pop 0) 
 (do (setv val (self.compile (expr.pop 0)))) 
 (do (setv low (Result))) 
 (when (!= expr []) (do (do (setv low (self.compile (expr.pop 0)))))) 
 (do (setv high (Result))) 
 (when (!= expr []) (do (do (setv high (self.compile (expr.pop 0)))))) 
 (do (setv step (Result))) 
 (when (!= expr []) (do (do (setv step (self.compile (expr.pop 0)))))) 
 (raise (Py2HyReturnException (+ (+ (+ (+ val low) high) step) (ast.Subscript :lineno expr.start_line :col_offset expr.start_column :value val.force_expr :slice (ast.Slice :lower low.expr :upper high.expr :step step.expr) :ctx (ast.Load)))))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "assoc") (checkargs :min 3 :even False) 
 (defn compile_assoc_expression [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (expr.pop 0) 
 (do (setv target (self.compile (expr.pop 0)))) 
 (do (setv ret target)) 
 (do (setv i (iter expr))) 
 (for [[key val] (genexpr (, (self.compile x) (self.compile y)) [[x y] (zip i i)])] 
 (setv ret (+ ret (+ (+ key val) (ast.Assign :lineno expr.start_line :col_offset expr.start_column :targets [(ast.Subscript :lineno expr.start_line :col_offset expr.start_column :value target.force_expr :slice (ast.Index :value key.force_expr) :ctx (ast.Store))] :value val.force_expr))))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "with_decorator") (checkargs :min 1) 
 (defn compile_decorate_expression [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (expr.pop 0) 
 (do (setv fn_py2hy_mangling (self.compile (expr.pop (- 1))))) 
 (when (or (not fn_py2hy_mangling.stmts) (not (isinstance (get fn_py2hy_mangling.stmts (- 1)) (, ast.FunctionDef ast.ClassDef)))) (do (raise (HyTypeError expr "Decorated a non-function")))) 
 (do (do (setv decorators (nth (self._compile_collect expr) 0)) (setv ret (nth (self._compile_collect expr) 1)) (setv _ (nth (self._compile_collect expr) 2)))) 
 (do (setv (. (get fn_py2hy_mangling.stmts (- 1)) decorator_list) (+ decorators (. (get fn_py2hy_mangling.stmts (- 1)) decorator_list)))) 
 (raise (Py2HyReturnException (+ ret fn_py2hy_mangling)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "with*") (checkargs :min 2) 
 (defn compile_with_expression [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (expr.pop 0) 
 (do (setv args (expr.pop 0))) 
 (when (not (isinstance args HyList)) (do (raise (HyTypeError expr ((. "with expects a list, received `{0}'" format) (. (type args) __name__)))))) 
 (when (< (len args) 1) (do (raise (HyTypeError expr "with needs [[arg (expr)]] or [[(expr)]]]")))) 
 (args.reverse) 
 (do (setv ctx (self.compile (args.pop 0)))) 
 (do (setv thing None)) 
 (when (!= args []) (do (do (setv thing (self._storeize (get args 0) (self.compile (args.pop 0))))))) 
 (do (setv body (self._compile_branch expr))) 
 (do (setv var (self.get_anon_var))) 
 (do (setv name (ast.Name :id (ast_str var) :arg (ast_str var) :ctx (ast.Store) :lineno expr.start_line :col_offset expr.start_column))) 
 (setv body (+ body (ast.Assign :targets [name] :value body.force_expr :lineno expr.start_line :col_offset expr.start_column))) 
 (do (setv the_with (ast.With :context_expr ctx.force_expr :lineno expr.start_line :col_offset expr.start_column :optional_vars thing :body body.stmts))) 
 (when PY3 (do (do (setv the_with.items [(ast.withitem :context_expr ctx.force_expr :optional_vars thing)])))) 
 (do (setv ret (+ ctx the_with))) 
 (do (setv ret.contains_yield (or ret.contains_yield body.contains_yield))) 
 (do (setv expr_name (ast.Name :id (ast_str var) :arg (ast_str var) :ctx (ast.Load) :lineno expr.start_line :col_offset expr.start_column))) 
 (setv ret (+ ret (Result :expr expr_name :temp_variables [expr_name name]))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds ",") 
 (defn compile_tuple [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (expr.pop 0) 
 (do (do (setv elts (nth (self._compile_collect expr) 0)) (setv ret (nth (self._compile_collect expr) 1)) (setv _ (nth (self._compile_collect expr) 2)))) 
 (setv ret (+ ret (ast.Tuple :elts elts :lineno expr.start_line :col_offset expr.start_column :ctx (ast.Load)))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (defn _compile_generator_iterables [self trailers] 
 "Helper to compile the \"trailing\" parts of comprehensions:
        generators and conditions" 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv generators (trailers.pop 0))) 
 (do (setv cond (if (!= trailers []) (self.compile (trailers.pop 0)) (Result)))) 
 (do (setv gen_it (iter generators))) 
 (do (setv paired_gens (zip gen_it gen_it))) 
 (do (setv gen_res (Result))) 
 (do (setv gen [])) 
 (for [[target iterable] paired_gens] 
 (do (setv comp_target (self.compile target))) 
 (do (setv target (self._storeize target comp_target))) 
 (setv gen_res (+ gen_res (self.compile iterable))) 
 (gen.append (ast.comprehension :target target :iter gen_res.force_expr :ifs [] :is_async False))) 
 (when cond.expr (do ((. (. (get gen (- 1)) ifs) append) cond.expr))) 
 (raise (Py2HyReturnException (, (+ gen_res cond) gen)))) (except [e Py2HyReturnException] e.retvalue))) 
 (with_decorator 
 (builds "list_comp") (checkargs :min 2 :max 3) 
 (defn compile_list_comprehension [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (expr.pop 0) 
 (do (setv expression (expr.pop 0))) 
 (do (setv gen_gen (get expr 0))) 
 (when (not (isinstance gen_gen HyList)) (do (raise (HyTypeError gen_gen "Generator expression must be a list.")))) 
 (do (do (setv gen_res (nth (self._compile_generator_iterables expr) 0)) (setv gen (nth (self._compile_generator_iterables expr) 1)))) 
 (when (= (len gen) 0) (do (raise (HyTypeError gen_gen "Generator expression cannot be empty.")))) 
 (do (setv compiled_expression (self.compile expression))) 
 (do (setv ret (+ compiled_expression gen_res))) 
 (setv ret (+ ret (ast.ListComp :lineno expr.start_line :col_offset expr.start_column :elt compiled_expression.force_expr :generators gen))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "set_comp") (checkargs :min 2 :max 3) 
 (defn compile_set_comprehension [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv ret (self.compile_list_comprehension expr))) 
 (do (setv expr ret.expr)) 
 (do (setv ret.expr (ast.SetComp :lineno expr.lineno :col_offset expr.col_offset :elt expr.elt :generators expr.generators))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "dict_comp") (checkargs :min 3 :max 4) 
 (defn compile_dict_comprehension [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (expr.pop 0) 
 (do (setv key (expr.pop 0))) 
 (do (setv value (expr.pop 0))) 
 (do (do (setv gen_res (nth (self._compile_generator_iterables expr) 0)) (setv gen (nth (self._compile_generator_iterables expr) 1)))) 
 (do (setv compiled_key (self.compile key))) 
 (do (setv compiled_value (self.compile value))) 
 (do (setv ret (+ (+ compiled_key compiled_value) gen_res))) 
 (setv ret (+ ret (ast.DictComp :lineno expr.start_line :col_offset expr.start_column :key compiled_key.force_expr :value compiled_value.force_expr :generators gen))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "genexpr") 
 (defn compile_genexpr [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv ret (self.compile_list_comprehension expr))) 
 (do (setv expr ret.expr)) 
 (do (setv ret.expr (ast.GeneratorExp :lineno expr.lineno :col_offset expr.col_offset :elt expr.elt :generators expr.generators))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "not") (builds "~") (checkargs 1) 
 (defn compile_unary_operator [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv ops {"not" ast.Not "~" ast.Invert})) 
 (do (setv operator (expression.pop 0))) 
 (do (setv operand (self.compile (expression.pop 0)))) 
 (setv operand (+ operand (ast.UnaryOp :op ((get ops operator)) :operand operand.expr :lineno operator.start_line :col_offset operator.start_column))) 
 (raise (Py2HyReturnException operand))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "require") 
 (defn compile_require [self expression] 
 "
        TODO: keep track of what we've imported in this run and then
        \"unimport\" it after we've completed `thing' so that we don't pollute
        other envs.
        " 
 "Using a hacky implementation of `return`" 
 (try (do (for [entry (get expression (slice 1 None None))] 
 (if (isinstance entry HySymbol) (do (__import__ entry) (hyhy.macros.require entry self.module_name :all_macros True :prefix entry)) (do (if (and (isinstance entry HyList) (= (len entry) 2)) (do (do (do (setv module (nth entry 0)) (setv names (nth entry 1)))) (when (not (isinstance names HyList)) (do (raise (HyTypeError names "(require) name lists should be HyLists")))) (__import__ module) (if (in "*" names) (do (when (!= (len names) 1) (do (raise (HyTypeError names "* in a (require) name list must be on its own")))) (hyhy.macros.require module self.module_name :all_macros True)) (do (do (setv assignments {})) (while names (if (and (> (len names) 1) (= (get names 1) (HyKeyword ":as"))) (do (do (do (setv k (nth (get names (slice None 3 None)) 0)) (setv _ (nth (get names (slice None 3 None)) 1)) (setv v (nth (get names (slice None 3 None)) 2)))) (del (get names (slice None 3 None))) (do (assoc assignments k v))) (do (do (setv symbol (names.pop 0))) (do (assoc assignments symbol symbol))))) (hyhy.macros.require module self.module_name :assignments assignments)))) (do (if (and (isinstance entry HyList) (= (len entry) 3) (= (get entry 1) (HyKeyword ":as"))) (do (do (do (setv module (nth entry 0)) (setv _ (nth entry 1)) (setv prefix (nth entry 2)))) (__import__ module) (hyhy.macros.require module self.module_name :all_macros True :prefix prefix)) (do (raise (HyTypeError entry "unrecognized (require) syntax"))))))))) 
 (raise (Py2HyReturnException (Result)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "and") (builds "or") 
 (defn compile_logical_or_and_and_operator [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv ops {"and" (, ast.And "True") "or" (, ast.Or "None")})) 
 (do (setv operator (expression.pop 0))) 
 (do (do (setv opnode (nth (get ops operator) 0)) (setv default (nth (get ops operator) 1)))) 
 (do (do (setv root_line (nth (, operator.start_line operator.start_column) 0)) (setv root_column (nth (, operator.start_line operator.start_column) 1)))) 
 (if (= (len expression) 0) (do (raise (Py2HyReturnException (ast.Name :id default :ctx (ast.Load) :lineno root_line :col_offset root_column)))) (do (when (= (len expression) 1) (do (raise (Py2HyReturnException (self.compile (get expression 0)))))))) 
 (do (setv ret (Result))) 
 (do (setv values (list (map self.compile expression)))) 
 (do (setv has_stmt (any (genexpr value.stmts [value values])))) 
 (if has_stmt (do (do (setv var (self.get_anon_var))) (do (setv name (ast.Name :id var :ctx (ast.Store) :lineno root_line :col_offset root_column))) (do (setv expr_name (ast.Name :id var :ctx (ast.Load) :lineno root_line :col_offset root_column))) (do (setv temp_variables [name expr_name])) (defn make_assign [value &optional [node None]] 
 "Using a hacky implementation of `return`" 
 (try (do (if (is node None) (do (do (do (setv line (nth (, root_line root_column) 0)) (setv column (nth (, root_line root_column) 1))))) (do (do (do (setv line (nth (, node.lineno node.col_offset) 0)) (setv column (nth (, node.lineno node.col_offset) 1)))))) 
 (do (setv positioned_name (ast.Name :id var :ctx (ast.Store) :lineno line :col_offset column))) 
 (temp_variables.append positioned_name) 
 (raise (Py2HyReturnException (ast.Assign :targets [positioned_name] :value value :lineno line :col_offset column)))) (except [e Py2HyReturnException] e.retvalue))) (do (setv root [])) (do (setv current root)) (for [[i value] (enumerate values)] 
 (if value.stmts (do (do (setv node (get value.stmts 0))) (current.extend value.stmts)) (do (do (setv node value.expr)))) 
 (current.append (make_assign value.force_expr value.force_expr)) 
 (when (= i (- (len values) 1)) (do (break))) 
 (if (= operator "and") (do (do (setv cond expr_name))) (do (when (= operator "or") (do (do (setv cond (ast.UnaryOp :op (ast.Not) :operand expr_name :lineno node.lineno :col_offset node.col_offset))))))) 
 (current.append (ast.If :test cond :body [] :lineno node.lineno :col_offset node.col_offset :orelse [])) 
 (do (setv current (. (get current (- 1)) body)))) (do (setv ret (sum root ret))) (setv ret (+ ret (Result :expr expr_name :temp_variables temp_variables)))) (do (setv ret (+ ret (ast.BoolOp :op (opnode) :lineno root_line :col_offset root_column :values (list_comp value.force_expr [value values])))))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (defn _compile_compare_op_expression [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv ops {"=" ast.Eq "!=" ast.NotEq "<" ast.Lt "<=" ast.LtE ">" ast.Gt ">=" ast.GtE "is" ast.Is "is_not" ast.IsNot "in" ast.In "not_in" ast.NotIn})) 
 (do (setv inv (expression.pop 0))) 
 (do (setv op (get ops inv))) 
 (do (setv ops (list_comp (op) [x (range 1 (len expression))]))) 
 (do (setv e (get expression 0))) 
 (do (do (setv exprs (nth (self._compile_collect expression) 0)) (setv ret (nth (self._compile_collect expression) 1)) (setv _ (nth (self._compile_collect expression) 2)))) 
 (raise (Py2HyReturnException (+ ret (ast.Compare :left (get exprs 0) :ops ops :comparators (get exprs (slice 1 None None)) :lineno e.start_line :col_offset e.start_column))))) (except [e Py2HyReturnException] e.retvalue))) 
 (with_decorator 
 (builds "=") (builds "is") (builds "<") (builds "<=") (builds ">") (builds ">=") (checkargs :min 1) 
 (defn compile_compare_op_expression [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (when (= (len expression) 2) (do (raise (Py2HyReturnException (ast.Name :id "True" :ctx (ast.Load) :lineno expression.start_line :col_offset expression.start_column))))) 
 (raise (Py2HyReturnException (self._compile_compare_op_expression expression)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "!=") (builds "is_not") (checkargs :min 2) 
 (defn compile_compare_op_expression_coll [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException (self._compile_compare_op_expression expression)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "in") (builds "not_in") (checkargs 2) 
 (defn compile_compare_op_expression_binary [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException (self._compile_compare_op_expression expression)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (defn _compile_maths_expression [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv ops {"+" ast.Add "/" ast.Div "//" ast.FloorDiv "*" ast.Mult "-" ast.Sub "%" ast.Mod "**" ast.Pow "<<" ast.LShift ">>" ast.RShift "|" ast.BitOr "^" ast.BitXor "&" ast.BitAnd})) 
 (when PY35 (do (ops.update {"@" ast.MatMult}))) 
 (do (setv op (get ops (expression.pop 0)))) 
 (do (setv right_associative (= op ast.Pow))) 
 (do (do (setv lineno (nth (, expression.start_line expression.start_column) 0)) (setv col_offset (nth (, expression.start_line expression.start_column) 1)))) 
 (when right_associative (do (do (setv expression (get expression (slice None None (- 1))))))) 
 (do (setv ret (self.compile (expression.pop 0)))) 
 (for [child expression] 
 (do (setv left_expr ret.force_expr)) 
 (setv ret (+ ret (self.compile child))) 
 (do (setv right_expr ret.force_expr)) 
 (when right_associative (do (do (do (setv left_expr (nth (, right_expr left_expr) 0)) (setv right_expr (nth (, right_expr left_expr) 1)))))) 
 (setv ret (+ ret (ast.BinOp :left left_expr :op (op) :right right_expr :lineno lineno :col_offset col_offset)))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue))) 
 (with_decorator 
 (builds "**") (builds "//") (builds "<<") (builds ">>") (builds "&") (checkargs :min 2) 
 (defn compile_maths_expression_2_or_more [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException (self._compile_maths_expression expression)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "%") (builds "^") (checkargs 2) 
 (defn compile_maths_expression_exactly_2 [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException (self._compile_maths_expression expression)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "*") (builds "|") 
 (defn compile_maths_expression_mul [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv id_elem (get {"*" 1 "|" 0} (get expression 0)))) 
 (if (= (len expression) 1) (do (raise (Py2HyReturnException (ast.Num :n (long_type id_elem) :lineno expression.start_line :col_offset expression.start_column)))) (do (if (= (len expression) 2) (do (raise (Py2HyReturnException (self.compile (get expression 1))))) (do (raise (Py2HyReturnException (self._compile_maths_expression expression)))))))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "/") (checkargs :min 1) 
 (defn compile_maths_expression_div [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (when (= (len expression) 2) (do (do (setv expression ((. (HyExpression [(HySymbol "/") (HyInteger 1) (get expression 1)]) replace) expression))))) 
 (raise (Py2HyReturnException (self._compile_maths_expression expression)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (defn _compile_maths_expression_additive [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (if (> (len expression) 2) (do (raise (Py2HyReturnException (self._compile_maths_expression expression)))) (do (do (setv op ((get {"+" ast.UAdd "-" ast.USub} (expression.pop 0))))) (do (setv arg (expression.pop 0))) (do (setv ret (self.compile arg))) (setv ret (+ ret (ast.UnaryOp :op op :operand ret.force_expr :lineno arg.start_line :col_offset arg.start_column))) (raise (Py2HyReturnException ret))))) (except [e Py2HyReturnException] e.retvalue))) 
 (with_decorator 
 (builds "&") (builds_if "@" PY35) (checkargs :min 1) 
 (defn compile_maths_expression_unary_idempotent [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (if (= (len expression) 2) (do (raise (Py2HyReturnException (self.compile (get expression 1))))) (do (raise (Py2HyReturnException (self._compile_maths_expression expression)))))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "+") 
 (defn compile_maths_expression_add [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (if (= (len expression) 1) (do (raise (Py2HyReturnException (ast.Num :n (long_type 0) :lineno expression.start_line :col_offset expression.start_column)))) (do (raise (Py2HyReturnException (self._compile_maths_expression_additive expression)))))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "-") (checkargs :min 1) 
 (defn compile_maths_expression_sub [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException (self._compile_maths_expression_additive expression)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "+=") (builds "/=") (builds "//=") (builds "*=") (builds "_=") (builds "%=") (builds "**=") (builds "<<=") (builds ">>=") (builds "|=") (builds "^=") (builds "&=") (builds_if "@=" PY35) (checkargs 2) 
 (defn compile_augassign_expression [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv ops {"+=" ast.Add "/=" ast.Div "//=" ast.FloorDiv "*=" ast.Mult "_=" ast.Sub "%=" ast.Mod "**=" ast.Pow "<<=" ast.LShift ">>=" ast.RShift "|=" ast.BitOr "^=" ast.BitXor "&=" ast.BitAnd})) 
 (when PY35 (do (ops.update {"@=" ast.MatMult}))) 
 (do (setv op (get ops (get expression 0)))) 
 (do (setv target (self._storeize (get expression 1) (self.compile (get expression 1))))) 
 (do (setv ret (self.compile (get expression 2)))) 
 (setv ret (+ ret (ast.AugAssign :target target :value ret.force_expr :op (op) :lineno expression.start_line :col_offset expression.start_column))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (checkargs 1) 
 (defn _compile_keyword_call [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (expression.append (expression.pop 0)) 
 (expression.insert 0 (HySymbol "get")) 
 (raise (Py2HyReturnException (self.compile expression)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds HyExpression) 
 (defn compile_expression [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv expression (hyhy.macros.macroexpand expression self))) 
 (when (not (isinstance expression HyExpression)) (do (raise (Py2HyReturnException (self.compile expression))))) 
 (when (= expression []) (do (raise (Py2HyReturnException (self.compile_list expression))))) 
 (do (setv fn_py2hy_mangling (get expression 0))) 
 (do (setv func None)) 
 (when (isinstance fn_py2hy_mangling HyKeyword) (do (raise (Py2HyReturnException (self._compile_keyword_call expression))))) 
 (when (isinstance fn_py2hy_mangling HySymbol) (do (when (or (= fn_py2hy_mangling ",") (not (any (genexpr (is_unpack "iterable" x) [x (get expression (slice 1 None None))])))) (do (do (setv ret (self.compile_atom fn_py2hy_mangling expression))) 
 (when ret (do (raise (Py2HyReturnException ret)))))) 
 (when (fn_py2hy_mangling.startswith ".") (do (do (setv attrs (list_comp ((. (HySymbol a) replace) fn_py2hy_mangling) [a (get (fn_py2hy_mangling.split ".") (slice 1 None None))]))) 
 (do (setv fn_py2hy_mangling (attrs.pop))) 
 (do (setv i 1)) 
 (when (!= (len expression) 2) (do (while (< i (len expression)) (if (isinstance (get expression i) HyKeyword) (do (setv i (+ i 1))) (do (break))) (setv i (+ i 1))))) 
 (do (setv func (self.compile (HyExpression (+ [((. (HySymbol ".") replace) fn_py2hy_mangling) (expression.pop i)] attrs))))) 
 (setv func (+ func (ast.Attribute :lineno fn_py2hy_mangling.start_line :col_offset fn_py2hy_mangling.start_column :value func.force_expr :attr (ast_str fn_py2hy_mangling) :ctx (ast.Load)))))))) 
 (when (not func) (do (do (setv func (self.compile fn_py2hy_mangling))))) 
 (if (in fn_py2hy_mangling (, "type" "HyKeyword" "keyword" "name" "is_keyword")) (do (do (setv with_kwargs False))) (do (do (setv with_kwargs True)))) 
 (do (do (setv args (nth (self._compile_collect (get expression (slice 1 None None)) with_kwargs :oldpy_unpack True) 0)) (setv ret (nth (self._compile_collect (get expression (slice 1 None None)) with_kwargs :oldpy_unpack True) 1)) (setv keywords (nth (self._compile_collect (get expression (slice 1 None None)) with_kwargs :oldpy_unpack True) 2)) (setv oldpy_starargs (nth (self._compile_collect (get expression (slice 1 None None)) with_kwargs :oldpy_unpack True) 3)) (setv oldpy_kwargs (nth (self._compile_collect (get expression (slice 1 None None)) with_kwargs :oldpy_unpack True) 4)))) 
 (setv ret (+ ret (ast.Call :func func.expr :args args :keywords keywords :starargs oldpy_starargs :kwargs oldpy_kwargs :lineno expression.start_line :col_offset expression.start_column))) 
 (raise (Py2HyReturnException (+ func ret)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "def") (builds "setv") 
 (defn compile_def_expression [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv root (expression.pop 0))) 
 (if (not expression) (do (do (setv result (Result))) (setv result (+ result (ast.Name :id "None" :ctx (ast.Load) :lineno root.start_line :col_offset root.start_column))) (raise (Py2HyReturnException result))) (do (if (= (len expression) 2) (do (raise (Py2HyReturnException (self._compile_assign (get expression 0) (get expression 1) expression.start_line expression.start_column)))) (do (if (!= (% (len expression) 2) 0) (do (raise (HyTypeError expression ((. "`{}' needs an even number of arguments" format) root)))) (do (do (setv result (Result))) (for [[tgt target] (zip (get expression (slice None None 2)) (get expression (slice 1 None 2)))] 
 (setv result (+ result (self._compile_assign tgt target tgt.start_line tgt.start_column)))) (raise (Py2HyReturnException result))))))))) (except [e Py2HyReturnException] e.retvalue)))) 
 (defn _compile_assign [self name result start_line start_column] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv str_name (% "%s" name))) 
 (when (and (_is_hy_builtin str_name self.module_name) (not self.allow_builtins)) (do (raise (HyTypeError name (% "Can't assign to a builtin: `%s'" str_name))))) 
 (do (setv result (self.compile result))) 
 (do (setv ld_name (self.compile name))) 
 (when (isinstance ld_name.expr ast.Call) (do (raise (HyTypeError name (% "Can't assign to a callable: `%s'" str_name))))) 
 (if (and result.temp_variables (isinstance name HyString) (not_in "." name)) (do (result.rename name) (do (setv result.expr None))) (do (do (setv st_name (self._storeize name ld_name))) (setv result (+ result (ast.Assign :lineno start_line :col_offset start_column :targets [st_name] :value result.force_expr))))) 
 (raise (Py2HyReturnException result))) (except [e Py2HyReturnException] e.retvalue))) 
 (with_decorator 
 (builds "for*") (checkargs :min 1) 
 (defn compile_for_expression [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (expression.pop 0) 
 (do (setv args (expression.pop 0))) 
 (when (not (isinstance args HyList)) (do (raise (HyTypeError expression ((. "for expects a list, received `{0}'" format) (. (type args) __name__)))))) 
 (try (do (do (do (setv target_name (nth args 0)) (setv iterable (nth args 1))))) (except [e Py2HyReturnException] (raise e)) (except [ValueError] (raise (HyTypeError expression "for requires two forms in the list")))) 
 (do (setv target (self._storeize target_name (self.compile target_name)))) 
 (do (setv ret (Result))) 
 (do (setv orel (Result))) 
 (when (and expression (= (get (get expression (- 1)) 0) (HySymbol "else"))) (do (do (setv else_expr (expression.pop))) 
 (if (> (len else_expr) 2) (do (raise (HyTypeError else_expr "`else' statement in `for' is too long"))) (do (when (= (len else_expr) 2) (do (setv orel (+ orel (self.compile (get else_expr 1)))) 
 (setv orel (+ orel (orel.expr_as_stmt))))))))) 
 (setv ret (+ ret (self.compile iterable))) 
 (do (setv body (self._compile_branch expression))) 
 (setv body (+ body (body.expr_as_stmt))) 
 (setv ret (+ ret (ast.For :lineno expression.start_line :col_offset expression.start_column :target target :iter ret.force_expr :body body.stmts :orelse orel.stmts))) 
 (do (setv ret.contains_yield body.contains_yield)) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "while") (checkargs :min 2) 
 (defn compile_while_expression [self expr] 
 "Using a hacky implementation of `return`" 
 (try (do (expr.pop 0) 
 (do (setv ret (self.compile (expr.pop 0)))) 
 (do (setv body (self._compile_branch expr))) 
 (setv body (+ body (body.expr_as_stmt))) 
 (setv ret (+ ret (ast.While :test ret.force_expr :body body.stmts :orelse [] :lineno expr.start_line :col_offset expr.start_column))) 
 (do (setv ret.contains_yield body.contains_yield)) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds HyList) 
 (defn compile_list [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (do (do (setv elts (nth (self._compile_collect expression) 0)) (setv ret (nth (self._compile_collect expression) 1)) (setv _ (nth (self._compile_collect expression) 2)))) 
 (setv ret (+ ret (ast.List :elts elts :ctx (ast.Load) :lineno expression.start_line :col_offset expression.start_column))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds HySet) 
 (defn compile_set [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (do (do (setv elts (nth (self._compile_collect expression) 0)) (setv ret (nth (self._compile_collect expression) 1)) (setv _ (nth (self._compile_collect expression) 2)))) 
 (setv ret (+ ret (ast.Set :elts elts :ctx (ast.Load) :lineno expression.start_line :col_offset expression.start_column))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "fn") (builds "fn*") (checkargs :min 1) 
 (defn compile_function_def [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv force_functiondef (= (expression.pop 0) "fn*"))) 
 (do (setv arglist (expression.pop 0))) 
 (when (not (isinstance arglist HyList)) (do (raise (HyTypeError expression "First argument to `fn' must be a list")))) 
 (do (do (setv ret (nth (self._parse_lambda_list arglist) 0)) (setv args (nth (self._parse_lambda_list arglist) 1)) (setv defaults (nth (self._parse_lambda_list arglist) 2)) (setv stararg (nth (self._parse_lambda_list arglist) 3)) (setv kwonlyargs (nth (self._parse_lambda_list arglist) 4)) (setv kwonlydefaults (nth (self._parse_lambda_list arglist) 5)) (setv kwargs (nth (self._parse_lambda_list arglist) 6)))) 
 (for [[i arg] (enumerate args)] 
 (when (isinstance arg HyList) (do (when (not arg) (do (raise (HyTypeError arglist "Cannot destruct empty list")))) 
 (do (setv _py2hy_anon_var_G_1236 (HySymbol (self.get_anon_var))) (assoc args i _py2hy_anon_var_G_1236) (setv var _py2hy_anon_var_G_1236)) 
 (do (setv expression (+ (HyExpression [(HyExpression [(HySymbol "setv") arg var])]) expression))) 
 (do (setv expression (expression.replace (get arg 0))))))) 
 (if PY34 (do (do (setv args (list_comp (ast.arg :arg (ast_str x) :annotation None :lineno x.start_line :col_offset x.start_column) [x args]))) (do (setv kwonlyargs (list_comp (ast.arg :arg (ast_str x) :annotation None :lineno x.start_line :col_offset x.start_column) [x kwonlyargs]))) (when kwargs (do (do (setv kwargs (ast.arg :arg (ast_str kwargs) :annotation None :lineno kwargs.start_line :col_offset kwargs.start_column))))) (when stararg (do (do (setv stararg (ast.arg :arg (ast_str stararg) :annotation None :lineno stararg.start_line :col_offset stararg.start_column)))))) (do (do (setv args (list_comp (ast.Name :arg (ast_str x) :id (ast_str x) :ctx (ast.Param) :lineno x.start_line :col_offset x.start_column) [x args]))) (when PY3 (do (do (setv kwonlyargs (list_comp (ast.Name :arg (ast_str x) :id (ast_str x) :ctx (ast.Param) :lineno x.start_line :col_offset x.start_column) [x kwonlyargs]))))) (when kwargs (do (do (setv kwargs (ast_str kwargs))))) (when stararg (do (do (setv stararg (ast_str stararg))))))) 
 (do (setv args (ast.arguments :args args :vararg stararg :kwarg kwargs :kwonlyargs kwonlyargs :kw_defaults kwonlydefaults :defaults defaults))) 
 (do (setv body (self._compile_branch expression))) 
 (when (and (not force_functiondef) (not body.stmts)) (do (setv ret (+ ret (ast.Lambda :lineno expression.start_line :col_offset expression.start_column :args args :body body.force_expr))) 
 (raise (Py2HyReturnException ret)))) 
 (when body.expr (do (if (and body.contains_yield (not PY3)) (do (setv body (+ body (body.expr_as_stmt)))) (do (setv body (+ body (ast.Return :value body.expr :lineno body.expr.lineno :col_offset body.expr.col_offset))))))) 
 (when (not body.stmts) (do (setv body (+ body (ast.Pass :lineno expression.start_line :col_offset expression.start_column))))) 
 (do (setv name (self.get_anon_fn))) 
 (setv ret (+ ret (ast.FunctionDef :name name :lineno expression.start_line :col_offset expression.start_column :args args :body body.stmts :decorator_list []))) 
 (do (setv ast_name (ast.Name :id name :arg name :ctx (ast.Load) :lineno expression.start_line :col_offset expression.start_column))) 
 (setv ret (+ ret (Result :expr ast_name :temp_variables [ast_name (get ret.stmts (- 1))]))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "defclass") (checkargs :min 1) 
 (defn compile_class_expression [self expressions] 
 "Using a hacky implementation of `return`" 
 (try (do (defn rewire_init [expr] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv new_args [])) 
 (when (= (get expr 0) (HySymbol "setv")) (do (do (setv pairs (get expr (slice 1 None None)))) 
 (while (> (len pairs) 0) (do (do (setv k (nth (, (pairs.pop 0) (pairs.pop 0)) 0)) (setv v (nth (, (pairs.pop 0) (pairs.pop 0)) 1)))) (when (= k (HySymbol "__init__")) (do (v.append (HySymbol "None")))) (new_args.append k) (new_args.append v)) 
 (do (setv expr ((. (HyExpression (+ [(HySymbol "setv")] new_args)) replace) expr))))) 
 (raise (Py2HyReturnException expr))) (except [e Py2HyReturnException] e.retvalue))) 
 (expressions.pop 0) 
 (do (setv class_name (expressions.pop 0))) 
 (if expressions (do (do (setv base_list (expressions.pop 0))) (when (not (isinstance base_list HyList)) (do (raise (HyTypeError expressions "Bases class must be a list")))) (do (do (setv bases_expr (nth (self._compile_collect base_list) 0)) (setv bases (nth (self._compile_collect base_list) 1)) (setv _ (nth (self._compile_collect base_list) 2))))) (do (do (setv bases_expr [])) (do (setv bases (Result))))) 
 (do (setv body (Result))) 
 (when (and expressions (isinstance (get expressions 0) HyString)) (do (do (setv docstring (expressions.pop 0))) 
 (do (setv symb (HySymbol "__doc__"))) 
 (do (setv symb.start_line docstring.start_line)) 
 (do (setv symb.start_column docstring.start_column)) 
 (setv body (+ body (self._compile_assign symb docstring docstring.start_line docstring.start_column))) 
 (setv body (+ body (body.expr_as_stmt))))) 
 (do (setv allow_builtins self.allow_builtins)) 
 (do (setv self.allow_builtins True)) 
 (when (and expressions (isinstance (get expressions 0) HyList) (not (isinstance (get expressions 0) HyExpression))) (do (do (setv expr (expressions.pop 0))) 
 (do (setv expr ((. (HyExpression (+ [(HySymbol "setv")] expr)) replace) expr))) 
 (setv body (+ body (self.compile (rewire_init expr)))))) 
 (for [expression expressions] 
 (do (setv expr (rewire_init (hyhy.macros.macroexpand expression self)))) 
 (setv body (+ body (self.compile expr)))) 
 (do (setv self.allow_builtins allow_builtins)) 
 (when (not body.stmts) (do (setv body (+ body (ast.Pass :lineno expressions.start_line :col_offset expressions.start_column))))) 
 (raise (Py2HyReturnException (+ bases (ast.ClassDef :lineno expressions.start_line :col_offset expressions.start_column :decorator_list [] :name (ast_str class_name) :keywords [] :starargs None :kwargs None :bases bases_expr :body body.stmts))))) (except [e Py2HyReturnException] e.retvalue)))) 
 (defn _compile_time_hack [self expression] 
 "Compile-time hack: we want to get our new macro now
        We must provide __name__ in the namespace to make the Python
        compiler set the __module__ attribute of the macro function." 
 "Using a hacky implementation of `return`" 
 (try (do (hyhy.importer.hy_eval expression (compile_time_ns self.module_name) self.module_name) 
 (do (setv ret (self.compile expression))) 
 (ret.add_imports "hyhy" [None]) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue))) 
 (with_decorator 
 (builds "defmacro") (checkargs :min 1) 
 (defn compile_macro [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (expression.pop 0) 
 (do (setv name (expression.pop 0))) 
 (when (not (isinstance name HySymbol)) (do (raise (HyTypeError name (% "received a `%s' instead of a symbol for macro name" (. (type name) __name__)))))) 
 (do (setv name ((. (HyString name) replace) name))) 
 (for [kw (, "&kwonly" "&kwargs" "&key")] 
 (when (in kw (get expression 0)) (do (raise (HyTypeError name (% "macros cannot use %s" kw)))))) 
 (do (setv new_expression ((. (HyExpression [(HyExpression [(HySymbol "hyhy.macros.macro") name]) (HyExpression (+ [(HySymbol "fn")] expression))]) replace) expression))) 
 (do (setv ret (self._compile_time_hack new_expression))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "deftag") (checkargs :min 2) 
 (defn compile_tag_macro [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (expression.pop 0) 
 (do (setv name (expression.pop 0))) 
 (when (or (= name ":") (= name "&")) (do (raise (NameError (% "%s can't be used as a tag macro name" name))))) 
 (when (and (not (isinstance name HySymbol)) (not (isinstance name HyString))) (do (raise (HyTypeError name (% "received a `%s' instead of a symbol for tag macro name" (. (type name) __name__)))))) 
 (do (setv name ((. (HyString name) replace) name))) 
 (do (setv new_expression ((. (HyExpression [(HyExpression [(HySymbol "hyhy.macros.tag") name]) (HyExpression (+ [(HySymbol "fn")] expression))]) replace) expression))) 
 (do (setv ret (self._compile_time_hack new_expression))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "dispatch_tag_macro") (checkargs :exact 2) 
 (defn compile_dispatch_tag_macro [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (expression.pop 0) 
 (do (setv tag (expression.pop 0))) 
 (when (not (= (type tag) HyString)) (do (raise (HyTypeError tag ((. "Trying to expand a tag macro using `{0}' instead of string" format) (. (type tag) __name__)))))) 
 (do (setv tag ((. (HyString (hy_symbol_mangle (str tag))) replace) tag))) 
 (do (setv expr (hyhy.macros.tag_macroexpand tag (expression.pop 0) self))) 
 (raise (Py2HyReturnException (self.compile expr)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "eval_and_compile") 
 (defn compile_eval_and_compile [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (do (assoc expression 0 (HySymbol "do"))) 
 (hyhy.importer.hy_eval expression (compile_time_ns self.module_name) self.module_name) 
 (expression.pop 0) 
 (raise (Py2HyReturnException (self._compile_branch expression)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds "eval_when_compile") 
 (defn compile_eval_when_compile [self expression] 
 "Using a hacky implementation of `return`" 
 (try (do (do (assoc expression 0 (HySymbol "do"))) 
 (hyhy.importer.hy_eval expression (compile_time_ns self.module_name) self.module_name) 
 (raise (Py2HyReturnException (Result)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds HyCons) 
 (defn compile_cons [self cons] 
 (raise (HyTypeError cons "Can't compile a top-level cons cell")))) 
 (with_decorator 
 (builds HyInteger) 
 (defn compile_integer [self number] 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException (ast.Num :n (long_type number) :lineno number.start_line :col_offset number.start_column)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds HyFloat) 
 (defn compile_float [self number] 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException (ast.Num :n (float number) :lineno number.start_line :col_offset number.start_column)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds HyComplex) 
 (defn compile_complex [self number] 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException (ast.Num :n (complex number) :lineno number.start_line :col_offset number.start_column)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds HySymbol) 
 (defn compile_symbol [self symbol] 
 "Using a hacky implementation of `return`" 
 (try (do (when (in "." symbol) (do (do (do (setv glob (nth (symbol.rsplit "." 1) 0)) (setv local (nth (symbol.rsplit "." 1) 1)))) 
 (when (not glob) (do (raise (HyTypeError symbol ((. "cannot access attribute on anything other than a name (in order to get attributes ofexpressions, use `(. <expression> {attr})` or `(.{attr} <expression>)`)" format) :attr local))))) 
 (when (not local) (do (raise (HyTypeError symbol "cannot access empty attribute")))) 
 (do (setv glob ((. (HySymbol glob) replace) symbol))) 
 (do (setv ret (self.compile_symbol glob))) 
 (do (setv ret (ast.Attribute :lineno symbol.start_line :col_offset symbol.start_column :value ret :attr (ast_str local) :ctx (ast.Load)))) 
 (raise (Py2HyReturnException ret)))) 
 (when (in symbol _stdlib) (do ((. (get self.imports (get _stdlib symbol)) add) symbol))) 
 (raise (Py2HyReturnException (ast.Name :id (ast_str symbol) :arg (ast_str symbol) :ctx (ast.Load) :lineno symbol.start_line :col_offset symbol.start_column)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds HyString) 
 (defn compile_string [self string] 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException (ast.Str :s (str_type string) :lineno string.start_line :col_offset string.start_column)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds HyBytes) 
 (defn compile_bytes [self bytestring] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv f (if PY3 ast.Bytes ast.Str))) 
 (raise (Py2HyReturnException (f :s (bytes_type bytestring) :lineno bytestring.start_line :col_offset bytestring.start_column)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds HyKeyword) 
 (defn compile_keyword [self keyword] 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException (ast.Str :s (str_type keyword) :lineno keyword.start_line :col_offset keyword.start_column)))) (except [e Py2HyReturnException] e.retvalue)))) 
 (with_decorator 
 (builds HyDict) 
 (defn compile_dict [self m] 
 "Using a hacky implementation of `return`" 
 (try (do (do (do (setv keyvalues (nth (self._compile_collect m :dict_display True) 0)) (setv ret (nth (self._compile_collect m :dict_display True) 1)) (setv _ (nth (self._compile_collect m :dict_display True) 2)))) 
 (setv ret (+ ret (ast.Dict :lineno m.start_line :col_offset m.start_column :keys (get keyvalues (slice None None 2)) :values (get keyvalues (slice 1 None 2))))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))))
(defn hy_compile [tree module_name &optional [root ast.Module] [get_expr False]] 
 "
    Compile a HyObject tree into a Python AST Module.

    If `get_expr` is True, return a tuple (module, last_expression), where
    `last_expression` is the.
    " 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv body [])) 
 (do (setv expr None)) 
 (when (not (isinstance tree HyObject)) (do (do (setv tree (wrap_value tree))) 
 (when (not (isinstance tree HyObject)) (do (raise (HyCompileError "`tree` must be a HyObject or capable of being promoted to one")))) 
 (spoof_positions tree))) 
 (do (setv compiler (HyASTCompiler module_name))) 
 (do (setv result (compiler.compile tree))) 
 (do (setv expr result.force_expr)) 
 (when (not get_expr) (do (setv result (+ result (result.expr_as_stmt))))) 
 (do (setv body (+ (compiler.imports_as_stmts tree) result.stmts))) 
 (do (setv ret (root :body body))) 
 (when get_expr (do (do (setv expr (ast.Expression :body expr))) 
 (do (setv ret (, ret expr))))) 
 (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue)))
