(import [contextlib])
(defclass Py2HyReturnException [Exception] 
 (defn __init__ [self retvalue] 
 (setv self.retvalue retvalue)))
(import [os])
(import [re])
(import [sys])
(import [hyhy.macros])
(import [hyhy.compiler])
(import [hyhy._compat [builtins string_types]])
(import [builtins])
(setv docomplete True)
(try 
 [(import [readline])] 
 (except [e Py2HyReturnException] 
 (raise e)) 
 (except [ImportError] 
 (try 
 (do 
 (import [pyreadline.rlmain]) 
 (import [pyreadline.unicode_helper]) 
 (import [readline])) 
 (except [e Py2HyReturnException] 
 (raise e)) 
 (except [ImportError] 
 (setv docomplete False)))))
(if (and (= sys.platform "darwin") (in "libedit" readline.__doc__)) 
 (do 
 (setv readline_bind "bind ^I rl_complete")) 
 (do 
 (setv readline_bind "tab: complete")))
(defclass Completer [object] 
 (defn __init__ [self &optional [namespace {}]] 
 (when (not (isinstance namespace dict)) 
 (raise (TypeError "namespace must be a dictionary"))) 
 (setv self.namespace namespace) 
 (setv self.path [hyhy.compiler._compile_table builtins.__dict__ (get hyhy.macros._hy_macros None) namespace]) 
 (setv self.tag_path [(get hyhy.macros._hy_tag None)]) 
 (when (in "__name__" namespace) 
 (setv module_name (get namespace "__name__")) 
 (self.path.append (get hyhy.macros._hy_macros module_name)) 
 (self.tag_path.append (get hyhy.macros._hy_tag module_name)))) 
 (defn attr_matches [self text] 
 (try 
 (do 
 (setv m (re.match "(\\S+(\\.[\\w-]+)*)\\.([\\w-]*)$" text)) 
 (if m 
 (do 
 (do 
 (setv _py2hy_anon_var_G_1235 (m.group 1 3)) 
 (setv expr (nth _py2hy_anon_var_G_1235 0)) 
 (setv attr (nth _py2hy_anon_var_G_1235 1))) 
 (setv attr (attr.replace "-" "_")) 
 (setv expr (expr.replace "-" "_"))) 
 (do 
 (raise (Py2HyReturnException [])))) 
 (try 
 (do 
 (setv obj (builtins.eval expr self.namespace)) 
 (setv words (dir obj))) 
 (except [e Py2HyReturnException] 
 (raise e)) 
 (except [Exception] 
 (raise (Py2HyReturnException [])))) 
 (setv n (len attr)) 
 (setv matches []) 
 (for [w words] 
 (when (= (get w (slice None n None)) attr) 
 (matches.append ((. "{}.{}" format) (expr.replace "_" "-") (w.replace "_" "-"))))) 
 (raise (Py2HyReturnException matches))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (defn global_matches [self text] 
 (try 
 (do 
 (setv matches []) 
 (for [p self.path] 
 (for [k (p.keys)] 
 (when (isinstance k string_types) 
 (setv k (k.replace "_" "-")) 
 (when (k.startswith text) 
 (matches.append k))))) 
 (raise (Py2HyReturnException matches))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (defn tag_matches [self text] 
 (try 
 (do 
 (setv text (get text (slice 1 None None))) 
 (setv matches []) 
 (for [p self.tag_path] 
 (for [k (p.keys)] 
 (when (isinstance k string_types) 
 (when (k.startswith text) 
 (matches.append ((. "#{}" format) k)))))) 
 (raise (Py2HyReturnException matches))) 
 (except [e Py2HyReturnException] 
 e.retvalue))) 
 (defn complete [self text state] 
 (try 
 (do 
 (if (text.startswith "#") 
 (do 
 (setv matches (self.tag_matches text))) 
 (do 
 (if (in "." text) 
 (do 
 (setv matches (self.attr_matches text))) 
 (do 
 (setv matches (self.global_matches text)))))) 
 (try 
 [(raise (Py2HyReturnException (get matches state)))] 
 (except [e Py2HyReturnException] 
 (raise e)) 
 (except [IndexError] 
 (raise (Py2HyReturnException None))))) 
 (except [e Py2HyReturnException] 
 e.retvalue))))
(with_decorator 
 contextlib.contextmanager 
 (defn completion [&optional [completer None]] 
 (try 
 (do 
 (setv delims "()[]{} ") 
 (when (not completer) 
 (setv completer (Completer))) 
 (when docomplete 
 (readline.set_completer completer.complete) 
 (readline.set_completer_delims delims) 
 (setv history (os.path.expanduser "~/.hyhy-history")) 
 (readline.parse_and_bind "set blink-matching-paren on") 
 (try 
 [(readline.read_history_file history)] 
 (except [e Py2HyReturnException] 
 (raise e)) 
 (except [IOError] 
 ((. (open history "a") close)))) 
 (readline.parse_and_bind readline_bind)) 
 (try 
 [(yield)] 
 (except [e Py2HyReturnException] 
 (raise e)) 
 (finally (when docomplete 
 (readline.write_history_file history))))) 
 (except [e Py2HyReturnException] 
 e.retvalue))))