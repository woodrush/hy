(import [os])
(defclass Py2HyReturnException [Exception] 
 (defn __init__ [self retvalue] 
 (setv self.retvalue retvalue)))
(import [imp])
(import [tempfile])
(import [hy])
(import [hyhy.importer [write_hy_as_pyc get_bytecode_path]])
(defn test_pyc [] 
 "Test pyc compilation." 
 (setv f (tempfile.NamedTemporaryFile :suffix ".hyhy" :delete False)) 
 (f.write (hy.models.HyBytes [40 100 101 102 110 32 112 121 99 116 101 115 116 32 91 115 93 32 40 43 32 34 88 34 32 115 32 34 89 34 41 41])) 
 (f.close) 
 (write_hy_as_pyc f.name) 
 (os.remove f.name) 
 (setv cfile (get_bytecode_path f.name)) 
 (setv mod (imp.load_compiled "pyc" cfile)) 
 (os.remove cfile) 
 (assert (= (mod.pyctest "Foo") "XFooY")))