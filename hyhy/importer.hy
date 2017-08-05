(import [hy])
(defclass Py2HyReturnException [Exception] (defn __init__ [self retvalue] (setv self.retvalue retvalue)))
(import [hyhy.compiler [hy_compile HyTypeError]])
(import [hyhy.models [HyObject HyExpression HySymbol replace_hy_obj]])
(import [hyhy.lex [tokenize LexException]])
(import [hyhy.errors [HyIOError]])
(import [io [open]])
(import [re])
(import [marshal])
(import [struct])
(import [imp])
(import [sys])
(import [ast])
(import [inspect])
(import [os])
(import [__future__])
(import [builtins])
(import [hyhy._compat [PY3 PY34 MAGIC builtins long_type wr_long]])
(import [hyhy._compat [string_types]])
(defn ast_compile [ast filename mode] "Compile AST.
    Like Python's compile, but with some special flags." "Using a hacky implementation of `return`" (try (do (setv flags (| (. __future__ CO_FUTURE_DIVISION) (. __future__ CO_FUTURE_PRINT_FUNCTION))) (raise (Py2HyReturnException (compile ast filename mode flags)))) (except [e Py2HyReturnException] e.retvalue)))
(defn import_buffer_to_hst [buf] "Import content from buf and return a Hy AST." "Using a hacky implementation of `return`" (try (do (raise (Py2HyReturnException (HyExpression (+ [(HySymbol "do")] (tokenize (+ buf "
"))))))) (except [e Py2HyReturnException] e.retvalue)))
(defn import_file_to_hst [fpath] "Import content from fpath and return a Hy AST." "Using a hacky implementation of `return`" (try (do (try (do (with [f (open fpath "r" :encoding "utf-8")] (setv buf ((. f read)))) (setv buf ((. re sub) "\\A#!.*" "" buf)) (raise (Py2HyReturnException (import_buffer_to_hst buf)))) (except [e Py2HyReturnException] (raise e)) (except [e IOError] (raise (HyIOError (. e errno) (. e strerror) (. e filename)))))) (except [e Py2HyReturnException] e.retvalue)))
(defn import_buffer_to_ast [buf module_name] " Import content from buf and return a Python AST." "Using a hacky implementation of `return`" (try (do (raise (Py2HyReturnException (hy_compile (import_buffer_to_hst buf) module_name)))) (except [e Py2HyReturnException] e.retvalue)))
(defn import_file_to_ast [fpath module_name] "Import content from fpath and return a Python AST." "Using a hacky implementation of `return`" (try (do (raise (Py2HyReturnException (hy_compile (import_file_to_hst fpath) module_name)))) (except [e Py2HyReturnException] e.retvalue)))
(defn import_file_to_module [module_name fpath &optional [loader None]] "Import Hy source from fpath and put it into a Python module.

    If there's an up-to-date byte-compiled version of this module, load that
    instead. Otherwise, byte-compile the module once we're done loading it, if
    we can.

    Return the module." "Using a hacky implementation of `return`" (try (do (setv module None) (setv bytecode_path (get_bytecode_path fpath)) (try (do (setv source_mtime (int (. ((. os stat) fpath) st_mtime))) (with [bc_f (open bytecode_path "rb")] (setv bytecode_magic ((. bc_f read) 4)) (do (setv _py2hy_anon_var_G_1235 ((. struct unpack) "<i" ((. bc_f read) 4))) (setv bytecode_mtime (nth _py2hy_anon_var_G_1235 0))))) (except [e Py2HyReturnException] (raise e)) (except [[IOError OSError]] None) (else (when (and (= bytecode_magic MAGIC) (>= bytecode_mtime source_mtime)) (do (if PY3 (do (import [importlib.machinery [SourcelessFileLoader]]) (setv module ((. (SourcelessFileLoader module_name bytecode_path) load_module) module_name))) (do (setv module ((. imp load_compiled) module_name bytecode_path)))))))) (when (not module) (do (assoc (. sys modules) module_name None) (try (do (setv _ast (import_file_to_ast fpath module_name)) (setv module ((. imp new_module) module_name)) (setv (. module __file__) fpath) (setv code (ast_compile _ast fpath "exec")) (when (not ((. (. os environ) get) "PYTHONDONTWRITEBYTECODE")) (do (try (do (write_code_as_pyc fpath code)) (except [e Py2HyReturnException] (raise e)) (except [[IOError OSError]] None)))) ((. builtins eval) code (. module __dict__))) (except [e Py2HyReturnException] (raise e)) (except [e [HyTypeError LexException]] (when (is (. e source) None) (do (with [fp (open fpath "rt")] (setv (. e source) ((. fp read)))) (setv (. e filename) fpath))) (raise None)) (except [Exception] ((. (. sys modules) pop) module_name None) (raise None))) (assoc (. sys modules) module_name module) (setv (. module __name__) module_name))) (setv (. module __file__) fpath) (when loader (do (setv (. module __loader__) loader))) (if (is_package module_name) (do (setv (. module __path__) []) (setv (. module __package__) module_name)) (do (setv (. module __package__) (get ((. module_name rpartition) ".") 0)))) (raise (Py2HyReturnException module))) (except [e Py2HyReturnException] e.retvalue)))
(defn import_buffer_to_module [module_name buf] "Using a hacky implementation of `return`" (try (do (try (do (setv _ast (import_buffer_to_ast buf module_name)) (setv mod ((. imp new_module) module_name)) ((. builtins eval) (ast_compile _ast "" "exec") (. mod __dict__))) (except [e Py2HyReturnException] (raise e)) (except [e [HyTypeError LexException]] (when (is (. e source) None) (do (setv (. e source) buf) (setv (. e filename) "<stdin>"))) (raise None))) (raise (Py2HyReturnException mod))) (except [e Py2HyReturnException] e.retvalue)))
(defn hy_eval [hytree &optional [namespace None] [module_name None] [ast_callback None]] "``eval`` evaluates a quoted expression and returns the value. The optional
    second and third arguments specify the dictionary of globals to use and the
    module name. The globals dictionary defaults to ``(local)`` and the module
    name defaults to the name of the current module.

       => (eval '(print \"Hello World\"))
       \"Hello World\"

    If you want to evaluate a string, use ``read-str`` to convert it to a
    form first:

       => (eval (read-str \"(+ 1 1)\"))
       2" "Using a hacky implementation of `return`" (try (do (when (is namespace None) (do (setv frame (get (get ((. inspect stack)) 1) 0)) (setv namespace (. ((. inspect getargvalues) frame) locals)))) (when (is module_name None) (do (setv m ((. inspect getmodule) (get (get ((. inspect stack)) 1) 0))) (setv module_name (if (is m None) "__eval__" (. m __name__))))) (setv foo (HyObject)) (setv (. foo start_line) 0) (setv (. foo end_line) 0) (setv (. foo start_column) 0) (setv (. foo end_column) 0) (replace_hy_obj hytree foo) (when (not (isinstance module_name string_types)) (do (raise (HyTypeError foo "Module name must be a string")))) (do (setv _py2hy_anon_var_G_1236 (hy_compile hytree module_name :get_expr True)) (setv _ast (nth _py2hy_anon_var_G_1236 0) expr (nth _py2hy_anon_var_G_1236 1))) (for [node ((. ast walk) _ast)] (setv (. node lineno) 1) (setv (. node col_offset) 1)) (for [node ((. ast walk) expr)] (setv (. node lineno) 1) (setv (. node col_offset) 1)) (when ast_callback (do (ast_callback _ast expr))) (when (not (isinstance namespace dict)) (do (raise (HyTypeError foo "Globals must be a dictionary")))) ((. builtins eval) (ast_compile _ast "<eval_body>" "exec") namespace) (raise (Py2HyReturnException ((. builtins eval) (ast_compile expr "<eval>" "eval") namespace)))) (except [e Py2HyReturnException] e.retvalue)))
(defn write_hy_as_pyc [fname] (setv _ast (import_file_to_ast fname ((. (. os path) basename) (get ((. (. os path) splitext) fname) 0)))) (setv code (ast_compile _ast fname "exec")) (write_code_as_pyc fname code))
(defn write_code_as_pyc [fname code] "Using a hacky implementation of `return`" (try (do (setv st ((. os stat) fname)) (setv timestamp (long_type (. st st_mtime))) (setv cfile (get_bytecode_path fname)) (try (do ((. os makedirs) ((. (. os path) dirname) cfile))) (except [e Py2HyReturnException] (raise e)) (except [[IOError OSError]] None)) (with [fc ((. builtins open) cfile "wb")] ((. fc write) MAGIC) (wr_long fc timestamp) (when PY3 (do (wr_long fc (. st st_size)))) ((. marshal dump) code fc))) (except [e Py2HyReturnException] e.retvalue)))
(defclass MetaLoader [object] (defn __init__ [self path] (setv (. self path) path)) (defn load_module [self fullname] "Using a hacky implementation of `return`" (try (do (when (in fullname (. sys modules)) (do (raise (Py2HyReturnException (get (. sys modules) fullname))))) (when (not (. self path)) (do (raise (Py2HyReturnException None)))) (raise (Py2HyReturnException (import_file_to_module fullname (. self path) self)))) (except [e Py2HyReturnException] e.retvalue))))
(defclass MetaImporter [object] (defn find_on_path [self fullname] "Using a hacky implementation of `return`" (try (do (setv fls ["%s/__init__.hyhy" "%s.hyhy"]) (setv dirpath ((. "/" join) ((. fullname split) "."))) (for [pth (. sys path)] (setv pth ((. (. os path) abspath) pth)) (for [fp fls] (setv composed_path (% fp (% "%s/%s" (, pth dirpath)))) (when ((. (. os path) exists) composed_path) (do (raise (Py2HyReturnException composed_path))))))) (except [e Py2HyReturnException] e.retvalue))) (defn find_module [self fullname &optional [path None]] "Using a hacky implementation of `return`" (try (do (setv path ((. self find_on_path) fullname)) (when path (do (raise (Py2HyReturnException (MetaLoader path)))))) (except [e Py2HyReturnException] e.retvalue))))
((. (. sys meta_path) insert) 0 (MetaImporter))
((. (. sys path) insert) 0 "")
(defn is_package [module_name] "Using a hacky implementation of `return`" (try (do (setv mpath ((. (. os path) join) (unpack_iterable ((. module_name split) ".")))) (for [path (map (. (. os path) abspath) (. sys path))] (when ((. (. os path) exists) ((. (. os path) join) path mpath "__init__.hyhy")) (do (raise (Py2HyReturnException True))))) (raise (Py2HyReturnException False))) (except [e Py2HyReturnException] e.retvalue)))
(defn get_bytecode_path [source_path] "Using a hacky implementation of `return`" (try (do (if PY34 (do (import [importlib.util]) (raise (Py2HyReturnException ((. (. importlib util) cache_from_source) source_path)))) (do (if (hasattr imp "cache_from_source") (do (raise (Py2HyReturnException ((. imp cache_from_source) source_path)))) (do (do (setv _py2hy_anon_var_G_1237 ((. (. os path) split) source_path)) (setv d (nth _py2hy_anon_var_G_1237 0) f (nth _py2hy_anon_var_G_1237 1))) (raise (Py2HyReturnException ((. (. os path) join) d ((. re sub) "(?:\\.[^.]+)?\\Z" ".pyc" f))))))))) (except [e Py2HyReturnException] e.retvalue)))
