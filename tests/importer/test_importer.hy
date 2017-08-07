(import [hy])
(defclass Py2HyReturnException [Exception] (defn __init__ [self retvalue] (setv self.retvalue retvalue)))
(import [hyhy.importer [import_file_to_module import_buffer_to_ast MetaLoader get_bytecode_path]])
(import [hyhy.errors [HyTypeError]])
(import [os])
(import [ast])
(import [tempfile])
(defn test_basics [] 
 "Make sure the basics of the importer work" 
 (import_file_to_module "basic" "tests/resources/importer/basic.hyhy"))
(defn test_stringer [] 
 (do (setv _ast (import_buffer_to_ast "(defn square [x] (* x x))" ""))) 
 (assert (= (type (get _ast.body 0)) ast.FunctionDef)))
(defn test_imports [] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv path (+ (os.getcwd) "/tests/resources/importer/a.hyhy"))) 
 (do (setv testLoader (MetaLoader path))) 
 (defn _import_test [] 
 "Using a hacky implementation of `return`" 
 (try (do (try (do (raise (Py2HyReturnException (testLoader.load_module "tests.resources.importer.a")))) (except [e Py2HyReturnException] (raise e)) (except [] (raise (Py2HyReturnException "Error"))))) (except [e Py2HyReturnException] e.retvalue))) 
 (assert (= (_import_test) "Error")) 
 (assert (is_not (_import_test) None))) (except [e Py2HyReturnException] e.retvalue)))
(defn test_import_error_reporting [] 
 "Make sure that (import) reports errors correctly." 
 "Using a hacky implementation of `return`" 
 (try (do (defn _import_error_test [] 
 "Using a hacky implementation of `return`" 
 (try (do (try (do (import_buffer_to_ast "(import \"sys\")" "")) (except [e Py2HyReturnException] (raise e)) (except [HyTypeError] (raise (Py2HyReturnException "Error reported"))))) (except [e Py2HyReturnException] e.retvalue))) 
 (assert (= (_import_error_test) "Error reported")) 
 (assert (is_not (_import_error_test) None))) (except [e Py2HyReturnException] e.retvalue)))
(defn test_import_autocompiles [] 
 "Test that (import) byte-compiles the module." 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv f (tempfile.NamedTemporaryFile :suffix ".hy" :delete False))) 
 (f.write (hy.models.HyBytes [40 100 101 102 110 32 112 121 99 116 101 115 116 32 91 115 93 32 40 43 32 34 88 34 32 115 32 34 89 34 41 41])) 
 (f.close) 
 (try (do (os.remove (get_bytecode_path f.name))) (except [e Py2HyReturnException] (raise e)) (except [[IOError OSError]] (do))) 
 (import_file_to_module "mymodule" f.name) 
 (assert (os.path.exists (get_bytecode_path f.name))) 
 (os.remove f.name) 
 (os.remove (get_bytecode_path f.name))) (except [e Py2HyReturnException] e.retvalue)))
