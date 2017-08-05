(import [argparse])
(defclass Py2HyReturnException [Exception] (defn __init__ [self retvalue] (setv self.retvalue retvalue)))
(import [code])
(import [ast])
(import [sys])
(import [os])
(import [importlib])
(import [astor.codegen])
(import [hy])
(import [hyhy])
(import [hyhy.lex [LexException PrematureEndOfInput]])
(import [hyhy.lex.parser [hy_symbol_mangle]])
(import [hyhy.compiler [HyTypeError]])
(import [hyhy.importer [hy_eval import_buffer_to_module import_file_to_ast import_file_to_hst import_buffer_to_ast import_buffer_to_hst]])
(import [hyhy.completer [completion]])
(import [hyhy.completer [Completer]])
(import [hyhy.errors [HyIOError]])
(import [hyhy.macros [macro require]])
(import [hyhy.models [HyExpression HyString HySymbol]])
(import [hyhy._compat [builtins PY3]])
(defclass HyQuitter [object] (defn __init__ [self name] (setv (. self name) name)) (defn __repr__ [self] "Using a hacky implementation of `return`" (try (do (raise (Py2HyReturnException (mod "Use (%s) or Ctrl-D (i.e. EOF) to exit" (. self name))))) (except [e Py2HyReturnException] e.retvalue))) (setv __str__ __repr__) (defn __call__ [self &optional [code None]] (try (do ((. (. sys stdin) close))) (except [] None)) (raise (SystemExit code))))
(setv (. builtins quit) (HyQuitter "quit"))
(setv (. builtins exit) (HyQuitter "exit"))
(defclass HyREPL [(. code InteractiveConsole)] (defn __init__ [self &optional [spy False] [output_fn None] [locals None] [filename "<input>"]] (setv (. self spy) spy) (if (is output_fn None) (do (setv (. self output_fn) repr)) (do (if (callable output_fn) (do (setv (. self output_fn) output_fn)) (do (setv f (hy_symbol_mangle output_fn)) (if (in "." output_fn) (do (setv (, module f) ((. f rsplit) "." 1)) (setv (. self output_fn) (getattr ((. importlib import_module) module) f))) (do (setv (. self output_fn) (get __builtins__ f)))))))) ((. (. code InteractiveConsole) __init__) self :locals locals :filename filename)) (defn runsource [self source &optional [filename "<input>"] [symbol "single"]] "Using a hacky implementation of `return`" (try (do (global SIMPLE_TRACEBACKS) (try (do (try (do (setv do (import_buffer_to_hst source))) (except [PrematureEndOfInput] (raise (Py2HyReturnException True))))) (except [e LexException] (when (is (. e source) None) (do (setv (. e source) source) (setv (. e filename) filename))) (print e :file (. sys stderr)) (raise (Py2HyReturnException False)))) (try (do (defn ast_callback [main_ast expr_ast] (when (. self spy) (do (setv new_ast ((. ast Module) (+ (. main_ast body) [((. ast Expr) (. expr_ast body))]))) (print ((. astor to_source) new_ast))))) (setv value (hy_eval do (. self locals) "__console__" ast_callback))) (except [e HyTypeError] (when (is (. e source) None) (do (setv (. e source) source) (setv (. e filename) filename))) (if SIMPLE_TRACEBACKS (do (print e :file (. sys stderr))) (do ((. self showtraceback)))) (raise (Py2HyReturnException False))) (except [Exception] ((. self showtraceback)) (raise (Py2HyReturnException False)))) (when (is_not value None) (do (setv (get (. self locals) "_") value) (print ((. self output_fn) value)))) (raise (Py2HyReturnException False))) (except [e Py2HyReturnException] e.retvalue))))
(with_decorator (macro "koan") (defn koan_macro [] "Using a hacky implementation of `return`" (try (do (raise (Py2HyReturnException (HyExpression [(HySymbol "print") (HyString "
  Ummon asked the head monk, \"What sutra are you lecturing on?\"
  \"The Nirvana Sutra.\"
  \"The Nirvana Sutra has the Four Virtues, hasn't it?\"
  \"It has.\"
  Ummon asked, picking up a cup, \"How many virtues has this?\"
  \"None at all,\" said the monk.
  \"But ancient people said it had, didn't they?\" said Ummon.
  \"What do you think of what they said?\"
  Ummon struck the cup and asked, \"You understand?\"
  \"No,\" said the monk.
  \"Then,\" said Ummon, \"You'd better go on with your lectures on the sutra.\"
")])))) (except [e Py2HyReturnException] e.retvalue))))
(with_decorator (macro "ideas") (defn ideas_macro [] "Using a hacky implementation of `return`" (try (do (raise (Py2HyReturnException (HyExpression [(HySymbol "print") (HyString "

    => (import [sh [figlet]])
    => (figlet \"Hi, Hy!\")
     _   _ _     _   _       _
    | | | (_)   | | | |_   _| |
    | |_| | |   | |_| | | | | |
    |  _  | |_  |  _  | |_| |_|
    |_| |_|_( ) |_| |_|\__, (_)
            |/         |___/


;;; string things
(.join \", \" [\"what\" \"the\" \"heck\"])


;;; this one plays with command line bits
(import [sh [cat grep]])
(-> (cat \"/usr/share/dict/words\") (grep \"-E\" \"bro$\"))


;;; filtering a list w/ a lambda
(filter (fn [x] (= (% x 2) 0)) (range 0 10))


;;; swaggin' functional bits (Python rulez)
(max (map (fn [x] (len x)) [\"hi\" \"my\" \"name\" \"is\" \"paul\"]))

")])))) (except [e Py2HyReturnException] e.retvalue))))
((. (. hyhy macros) require) "hyhy.cmdline" "__console__" :all_macros True)
((. (. hyhy macros) require) "hyhy.cmdline" "__main__" :all_macros True)
(setv SIMPLE_TRACEBACKS True)
(defn pretty_error [func &kwargs kw &rest args] "Using a hacky implementation of `return`" (try (do (try (do (raise (Py2HyReturnException (func (unpack_iterable args) (unpack_mapping kw))))) (except [e (, HyTypeError LexException)] (when SIMPLE_TRACEBACKS (do (print e :file (. sys stderr)) ((. sys exit) 1))) (raise None)))) (except [e Py2HyReturnException] e.retvalue)))
(defn run_command [source] "Using a hacky implementation of `return`" (try (do (pretty_error import_buffer_to_module "__main__" source) (raise (Py2HyReturnException 0))) (except [e Py2HyReturnException] e.retvalue)))
(defn run_module [mod_name] "Using a hacky implementation of `return`" (try (do (import [hyhy.importer [MetaImporter]]) (setv pth ((. (MetaImporter) find_on_path) mod_name)) (when (is_not pth None) (do (setv (. sys argv) (+ [pth] (. sys argv))) (raise (Py2HyReturnException (run_file pth))))) (print ((. "{0}: module '{1}' not found.
" format) (. hyhy __appname__) mod_name) :file (. sys stderr)) (raise (Py2HyReturnException 1))) (except [e Py2HyReturnException] e.retvalue)))
(defn run_file [filename] "Using a hacky implementation of `return`" (try (do (import [hyhy.importer [import_file_to_module]]) (pretty_error import_file_to_module "__main__" filename) (raise (Py2HyReturnException 0))) (except [e Py2HyReturnException] e.retvalue)))
(defn run_repl [&optional [hr None] &kwargs kwargs] "Using a hacky implementation of `return`" (try (do (import [platform]) (setv (. sys ps1) "=> ") (setv (. sys ps2) "... ") (setv namespace {"__name__" "__console__" "__doc__" ""}) (with [(completion (Completer namespace))] (when (not hr) (do (setv hr (HyREPL (unpack_mapping kwargs))))) ((. hr interact) ((. "{appname} {version} using {py}({build}) {pyversion} on {os}" format) :appname (. hyhy __appname__) :version (. hyhy __version__) :py ((. platform python_implementation)) :build (get ((. platform python_build)) 0) :pyversion ((. platform python_version)) :os ((. platform system))))) (raise (Py2HyReturnException 0))) (except [e Py2HyReturnException] e.retvalue)))
(defn run_icommand [source &kwargs kwargs] "Using a hacky implementation of `return`" (try (do (setv hr (HyREPL (unpack_mapping kwargs))) (if ((. (. os path) exists) source) (do (with [f (open source "r")] (setv source ((. f read)))) (setv filename source)) (do (setv filename "<input>"))) ((. hr runsource) source :filename filename :symbol "single") (raise (Py2HyReturnException (run_repl hr)))) (except [e Py2HyReturnException] e.retvalue)))
(setv USAGE "%(prog)s [-h | -i cmd | -c cmd | -m module | file | -] [arg] ...")
(setv VERSION (+ "%(prog)s " (. hyhy __version__)))
(setv EPILOG "  file         program read from script
  module       module to execute as main
  -            program read from stdin
  [arg] ...    arguments passed to program in sys.argv[1:]
")
(defn cmdline_handler [scriptname argv] "Using a hacky implementation of `return`" (try (do (setv parser ((. argparse ArgumentParser) :prog "hyhy" :usage USAGE :formatter_class (. argparse RawDescriptionHelpFormatter) :epilog EPILOG)) ((. parser add_argument) "-c" :dest "command" :help "program passed in as a string") ((. parser add_argument) "-m" :dest "mod" :help "module to run, passed in as a string") ((. parser add_argument) "-i" :dest "icommand" :help "program passed in as a string, then stay in REPL") ((. parser add_argument) "--spy" :action "store_true" :help "print equivalent Python code before executing") ((. parser add_argument) "--repl-output-fn" :help "function for printing REPL output (e.g., hyhy.contrib.hy-repr.hy-repr)") ((. parser add_argument) "-v" "--version" :action "version" :version VERSION) ((. parser add_argument) "--show-tracebacks" :action "store_true" :help "show complete tracebacks for Hy exceptions") ((. parser add_argument) "args" :nargs (. argparse REMAINDER) :help (. argparse SUPPRESS)) (setv (. hyhy executable) (get argv 0)) (setv module_args []) (when (in "-m" argv) (do (setv mloc ((. argv index) "-m")) (when (> (len argv) (+ mloc 2)) (do (setv module_args (get argv (slice (+ mloc 2) None None))) (setv argv (get argv (slice None (+ mloc 2) None))))))) (setv options ((. parser parse_args) (get argv (slice 1 None None)))) (when (. options show_tracebacks) (do (global SIMPLE_TRACEBACKS) (setv SIMPLE_TRACEBACKS False))) (setv (. sys argv) (or (+ (. options args) module_args) [""])) (when (. options command) (do (raise (Py2HyReturnException (run_command (. options command)))))) (when (. options mod) (do (raise (Py2HyReturnException (run_module (. options mod)))))) (when (. options icommand) (do (raise (Py2HyReturnException (run_icommand (. options icommand) :spy (. options spy) :output_fn (. options repl_output_fn)))))) (when (. options args) (do (if (= (get (. options args) 0) "-") (do (raise (Py2HyReturnException (run_command ((. (. sys stdin) read)))))) (do (try (do (raise (Py2HyReturnException (run_file (get (. options args) 0))))) (except [e HyIOError] (print ((. "hyhy: Can't open file '{0}': [Errno {1}] {2}
" format) (. e filename) (. e errno) (. e strerror)) :file (. sys stderr)) ((. sys exit) (. e errno)))))))) (raise (Py2HyReturnException (run_repl :spy (. options spy) :output_fn (. options repl_output_fn))))) (except [e Py2HyReturnException] e.retvalue)))
(defn hy_main [] ((. sys exit) (cmdline_handler "hyhy" (. sys argv))))
(defn hyc_main [] (import [hyhy.importer [write_hy_as_pyc]]) (setv parser ((. argparse ArgumentParser) :prog "hyc")) ((. parser add_argument) "files" :metavar "FILE" :nargs "+" :help "file to compile") ((. parser add_argument) "-v" :action "version" :version VERSION) (setv options ((. parser parse_args) (get (. sys argv) (slice 1 None None)))) (for [file (. options files)] (try (do (print (mod "Compiling %s" file)) (pretty_error write_hy_as_pyc file)) (except [x IOError] (print ((. "hyc: Can't open file '{0}': [Errno {1}] {2}
" format) (. x filename) (. x errno) (. x strerror)) :file (. sys stderr)) ((. sys exit) (. x errno))))))
(defn hy2py_main [] (import [platform]) (setv module_name "<STDIN>") (setv options (dict :prog "hy2py" :usage "%(prog)s [options] [FILE]" :formatter_class (. argparse RawDescriptionHelpFormatter))) (setv parser ((. argparse ArgumentParser) (unpack_mapping options))) ((. parser add_argument) "FILE" :type str :nargs "?" :help "Input Hy code (use STDIN if \"-\" or not provided)") ((. parser add_argument) "--with-source" "-s" :action "store_true" :help "Show the parsed source structure") ((. parser add_argument) "--with-ast" "-a" :action "store_true" :help "Show the generated AST") ((. parser add_argument) "--without-python" "-np" :action "store_true" :help "Do not show the Python code generated from the AST") (setv options ((. parser parse_args) (get (. sys argv) (slice 1 None None)))) (setv stdin_text None) (when (or (is (. options FILE) None) (= (. options FILE) "-")) (do (setv stdin_text ((. (. sys stdin) read))))) (when (. options with_source) (do (setv hst (if (is stdin_text None) (pretty_error import_file_to_hst (. options FILE)) (pretty_error import_buffer_to_hst stdin_text))) (if (and PY3 (= ((. platform system)) "Windows")) (do (for [h hst] (try (do (print h)) (except [] (print ((. (str h) encode) "utf-8")))))) (do (print hst))) (print) (print))) (setv _ast (if (is stdin_text None) (pretty_error import_file_to_ast (. options FILE) module_name) (pretty_error import_buffer_to_ast stdin_text module_name))) (when (. options with_ast) (do (if (and PY3 (= ((. platform system)) "Windows")) (do (_print_for_windows ((. astor dump) _ast))) (do (print ((. astor dump) _ast)))) (print) (print))) (when (not (. options without_python)) (do (if (and PY3 (= ((. platform system)) "Windows")) (do (_print_for_windows ((. (. astor codegen) to_source) _ast))) (do (print ((. (. astor codegen) to_source) _ast)))))) ((. parser exit) 0))
(defn _print_for_windows [src] (for [line ((. src split) "
")] (try (do (print line)) (except [] (print ((. line encode) "utf-8"))))))