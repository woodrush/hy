(import [inspect [getargspec formatargspec]])
(defclass Py2HyReturnException [Exception] (defn __init__ [self retvalue] (setv self.retvalue retvalue)))
(import [hyhy.models [replace_hy_obj HyExpression HySymbol]])
(import [hyhy.errors [HyTypeError HyMacroExpansionError]])
(import [collections [defaultdict]])
(import [builtins])
(do (setv CORE_MACROS ["hyhy.core.bootstrap"]))
(do (setv EXTRA_MACROS ["hyhy.core.macros"]))
(do (setv _hy_macros (defaultdict dict)))
(do (setv _hy_tag (defaultdict dict)))
(defn macro [name] 
 "Decorator to define a macro called `name`.

    This stores the macro `name` in the namespace for the module where it is
    defined.

    If the module where it is defined is in `hyhy.core`, then the macro is stored
    in the default `None` namespace.

    This function is called from the `defmacro` special form in the compiler.

    " 
 "Using a hacky implementation of `return`" 
 (try (do (defn _ [fn_py2hy_mangling] 
 "Using a hacky implementation of `return`" 
 (try (do (try (do (do (setv argspec (getargspec fn_py2hy_mangling))) 
 (do (setv fn_py2hy_mangling._hy_macro_pass_compiler (is_not argspec.keywords None)))) (except [e Py2HyReturnException] (raise e)) (except [Exception] (do (setv fn_py2hy_mangling._hy_macro_pass_compiler False)))) 
 (do (setv module_name fn_py2hy_mangling.__module__)) 
 (when (module_name.startswith "hyhy.core") (do (do (setv module_name None)))) 
 (do (assoc (get _hy_macros module_name) name fn_py2hy_mangling)) 
 (raise (Py2HyReturnException fn_py2hy_mangling))) (except [e Py2HyReturnException] e.retvalue))) 
 (raise (Py2HyReturnException _))) (except [e Py2HyReturnException] e.retvalue)))
(defn tag [name] 
 "Decorator to define a tag macro called `name`.

    This stores the macro `name` in the namespace for the module where it is
    defined.

    If the module where it is defined is in `hyhy.core`, then the macro is stored
    in the default `None` namespace.

    This function is called from the `deftag` special form in the compiler.

    " 
 "Using a hacky implementation of `return`" 
 (try (do (defn _ [fn_py2hy_mangling] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv module_name fn_py2hy_mangling.__module__)) 
 (when (module_name.startswith "hyhy.core") (do (do (setv module_name None)))) 
 (do (assoc (get _hy_tag module_name) name fn_py2hy_mangling)) 
 (raise (Py2HyReturnException fn_py2hy_mangling))) (except [e Py2HyReturnException] e.retvalue))) 
 (raise (Py2HyReturnException _))) (except [e Py2HyReturnException] e.retvalue)))
(defn require_hyhy [source_module target_module &optional [all_macros False] [assignments {}] [prefix ""]] 
 "Load macros from `source_module` in the namespace of
    `target_module`. `assignments` maps old names to new names, but is
    ignored if `all_macros` is true. If `prefix` is nonempty, it is
    prepended to the name of each imported macro. (This means you get
    macros named things like \"mymacromodule.mymacro\", which looks like
    an attribute of a module, although it's actually just a symbol
    with a period in its name.)

    This function is called from the `require` special form in the compiler.

    " 
 (do (setv seen_names (set))) 
 (when prefix (do (setv prefix (+ prefix ".")))) 
 (for [d (, _hy_macros _hy_tag)] 
 (for [[name macro] ((. (get d source_module) items))] 
 (seen_names.add name) 
 (if all_macros (do (do (assoc (get d target_module) (+ prefix name) macro))) (do (when (in name assignments) (do (do (assoc (get d target_module) (+ prefix (get assignments name)) macro)))))))) 
 (when (not all_macros) (do (do (setv unseen ((. (frozenset (assignments.keys)) difference) seen_names))) 
 (when unseen (do (raise (ImportError (+ "cannot require names: " (repr (list unseen))))))))))
(defn load_macros [module_name] 
 "Load the hy builtin macros for module `module_name`.

    Modules from `hyhy.core` can only use the macros from CORE_MACROS.
    Other modules get the macros from CORE_MACROS and EXTRA_MACROS.

    " 
 "Using a hacky implementation of `return`" 
 (try (do (defn _import [module &optional [module_name module_name]] 
 "__import__ a module, avoiding recursions" 
 (when (!= module module_name) (do (__import__ module)))) 
 (for [module CORE_MACROS] 
 (_import module)) 
 (when (module_name.startswith "hyhy.core") (do (raise (Py2HyReturnException None)))) 
 (for [module EXTRA_MACROS] 
 (_import module))) (except [e Py2HyReturnException] e.retvalue)))
(defn make_empty_fn_copy [fn_py2hy_mangling] 
 "Using a hacky implementation of `return`" 
 (try (do (try (do (do (setv argspec (getargspec fn_py2hy_mangling))) 
 (do (setv formatted_args (formatargspec (unpack_iterable argspec)))) 
 (do (setv fn_str ((. "lambda {}: None" format) ((. (formatted_args.lstrip "(") rstrip) ")")))) 
 (do (setv empty_fn (builtins.eval fn_str)))) (except [e Py2HyReturnException] (raise e)) (except [Exception] (defn empty_fn [&kwargs kwargs &rest args] 
 None))) 
 (raise (Py2HyReturnException empty_fn))) (except [e Py2HyReturnException] e.retvalue)))
(defn macroexpand [tree compiler] 
 "Expand the toplevel macros for the `tree`.

    Load the macros from the given `module_name`, then expand the (top-level)
    macros in `tree` until it stops changing.

    " 
 "Using a hacky implementation of `return`" 
 (try (do (load_macros compiler.module_name) 
 (do (setv old None)) 
 (while (!= old tree) (do (setv old tree)) (do (setv tree (macroexpand_1 tree compiler)))) 
 (raise (Py2HyReturnException tree))) (except [e Py2HyReturnException] e.retvalue)))
(defn macroexpand_1 [tree compiler] 
 "Expand the toplevel macro from `tree` once, in the context of
    `module_name`." 
 "Using a hacky implementation of `return`" 
 (try (do (when (isinstance tree HyExpression) (do (when (= tree []) (do (raise (Py2HyReturnException tree)))) 
 (do (setv fn_py2hy_mangling (get tree 0))) 
 (when (in fn_py2hy_mangling (, "quote" "quasiquote")) (do (raise (Py2HyReturnException tree)))) 
 (do (setv ntree (HyExpression (get tree (slice None None None))))) 
 (ntree.replace tree) 
 (do (setv opts {})) 
 (when (isinstance fn_py2hy_mangling HySymbol) (do (do (setv m ((. (get _hy_macros compiler.module_name) get) fn_py2hy_mangling))) 
 (when (is m None) (do (do (setv m ((. (get _hy_macros None) get) fn_py2hy_mangling))))) 
 (when (is_not m None) (do (when m._hy_macro_pass_compiler (do (do (assoc opts "compiler" compiler)))) 
 (try (do (do (setv m_copy (make_empty_fn_copy m))) 
 (m_copy (unpack_iterable (get ntree (slice 1 None None))) (unpack_mapping opts))) (except [e Py2HyReturnException] (raise e)) (except [e TypeError] (do (setv msg (+ (+ "expanding `" (str (get tree 0))) "': "))) (setv msg (+ msg ((. ((. (str e) replace) "<lambda>()" "" 1) strip)))) (raise (HyMacroExpansionError tree msg)))) 
 (try (do (do (setv obj (m (unpack_iterable (get ntree (slice 1 None None))) (unpack_mapping opts))))) (except [e Py2HyReturnException] (raise e)) (except [e HyTypeError] (when (is e.expression None) (do (do (setv e.expression tree)))) (raise)) (except [e Exception] (do (setv msg (+ (+ (+ "expanding `" (str (get tree 0))) "': ") (repr e)))) (raise (HyMacroExpansionError tree msg)))) 
 (replace_hy_obj obj tree) 
 (raise (Py2HyReturnException obj)))))) 
 (raise (Py2HyReturnException ntree)))) 
 (raise (Py2HyReturnException tree))) (except [e Py2HyReturnException] e.retvalue)))
(defn tag_macroexpand [tag tree compiler] 
 "Expand the tag macro \"tag\" with argument `tree`." 
 "Using a hacky implementation of `return`" 
 (try (do (load_macros compiler.module_name) 
 (do (setv tag_macro ((. (get _hy_tag compiler.module_name) get) tag))) 
 (when (is tag_macro None) (do (try (do (do (setv tag_macro (get (get _hy_tag None) tag)))) (except [e Py2HyReturnException] (raise e)) (except [KeyError] (raise (HyTypeError tag ((. "`{0}' is not a defined tag macro." format) tag))))))) 
 (do (setv expr (tag_macro tree))) 
 (raise (Py2HyReturnException (replace_hy_obj expr tree)))) (except [e Py2HyReturnException] e.retvalue)))
