(import [contextlib])
(defclass Py2HyReturnException [Exception] (defn __init__ [self retvalue] (setv self.retvalue retvalue)))
(import [os])
(import [re])
(import [sys])
(import [hyhy.macros])
(import [hyhy.compiler])
(import [hyhy._compat [builtins string_types]])
(do (setv docomplete True))
(try (do (import [readline])) (except [e Py2HyReturnException] (raise e)) (except [ImportError] (try (do (import [pyreadline.rlmain]) 
 (import [pyreadline.unicode_helper]) 
 (import [readline])) (except [e Py2HyReturnException] (raise e)) (except [ImportError] (do (setv docomplete False))))))
(if (and (= sys.platform "darwin") (in "libedit" readline.__doc__)) (do (do (setv readline_bind "bind ^I rl_complete"))) (do (do (setv readline_bind "tab: complete"))))
(defclass Completer [object] (defn __init__ [self &optional [namespace {}]] 
 (when (not (isinstance namespace dict)) (do (raise (TypeError "namespace must be a dictionary")))) 
 (do (setv self.namespace namespace)) 
 (do (setv self.path [hyhy.compiler._compile_table builtins.__dict__ (get hyhy.macros._hy_macros None) namespace])) 
 (do (setv self.tag_path [(get hyhy.macros._hy_tag None)])) 
 (when (in "__name__" namespace) (do (do (setv module_name (get namespace "__name__"))) 
 (self.path.append (get hyhy.macros._hy_macros module_name)) 
 (self.tag_path.append (get hyhy.macros._hy_tag module_name))))) 
 (defn attr_matches [self text] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv m (re.match "(\\S+(\\.[\\w-]+)*)\\.([\\w-]*)$" text))) 
 (if m (do (do (do (setv expr (nth (m.group 1 3) 0)) (setv attr (nth (m.group 1 3) 1)))) (do (setv attr (attr.replace "-" "_"))) (do (setv expr (expr.replace "-" "_")))) (do (raise (Py2HyReturnException [])))) 
 (try (do (do (setv obj (eval expr self.namespace))) 
 (do (setv words (dir obj)))) (except [e Py2HyReturnException] (raise e)) (except [Exception] (raise (Py2HyReturnException [])))) 
 (do (setv n (len attr))) 
 (do (setv matches [])) 
 (for [w words] 
 (when (= (get w (slice None n None)) attr) (do (matches.append ((. "{}.{}" format) (expr.replace "_" "-") (w.replace "_" "-")))))) 
 (raise (Py2HyReturnException matches))) (except [e Py2HyReturnException] e.retvalue))) 
 (defn global_matches [self text] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv matches [])) 
 (for [p self.path] 
 (for [k (p.keys)] 
 (when (isinstance k string_types) (do (do (setv k (k.replace "_" "-"))) 
 (when (k.startswith text) (do (matches.append k))))))) 
 (raise (Py2HyReturnException matches))) (except [e Py2HyReturnException] e.retvalue))) 
 (defn tag_matches [self text] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv text (get text (slice 1 None None)))) 
 (do (setv matches [])) 
 (for [p self.tag_path] 
 (for [k (p.keys)] 
 (when (isinstance k string_types) (do (when (k.startswith text) (do (matches.append ((. "#{}" format) k)))))))) 
 (raise (Py2HyReturnException matches))) (except [e Py2HyReturnException] e.retvalue))) 
 (defn complete [self text state] 
 "Using a hacky implementation of `return`" 
 (try (do (if (text.startswith "#") (do (do (setv matches (self.tag_matches text)))) (do (if (in "." text) (do (do (setv matches (self.attr_matches text)))) (do (do (setv matches (self.global_matches text))))))) 
 (try (do (raise (Py2HyReturnException (get matches state)))) (except [e Py2HyReturnException] (raise e)) (except [IndexError] (raise (Py2HyReturnException None))))) (except [e Py2HyReturnException] e.retvalue))))
(with_decorator 
 contextlib.contextmanager 
 (defn completion [&optional [completer None]] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv delims "()[]{} ")) 
 (when (not completer) (do (do (setv completer (Completer))))) 
 (when docomplete (do (readline.set_completer completer.complete) 
 (readline.set_completer_delims delims) 
 (do (setv history (os.path.expanduser "~/.hyhy-history"))) 
 (readline.parse_and_bind "set blink-matching-paren on") 
 (try (do (readline.read_history_file history)) (except [e Py2HyReturnException] (raise e)) (except [IOError] ((. (open history "a") close)))) 
 (readline.parse_and_bind readline_bind))) 
 (try (do (yield)) (except [e Py2HyReturnException] (raise e)) (finally (when docomplete (do (readline.write_history_file history)))))) (except [e Py2HyReturnException] e.retvalue))))
