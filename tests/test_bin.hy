(import [os])
(defclass Py2HyReturnException [Exception] (defn __init__ [self retvalue] (setv self.retvalue retvalue)))
(import [subprocess])
(import [re])
(import [hy])
(import [hyhy._compat [PY3 PY35]])
(import [hyhy.importer [get_bytecode_path]])
(import [pytest])
(do (setv hy_dir (os.environ.get "HY_DIR" "")))
(defn hr [&optional [s ""]] 
 "Using a hacky implementation of `return`" 
 (try (do (raise (Py2HyReturnException (+ "hyhy --repl-output-fn=hyhy.contrib.hy-repr.hy-repr " s)))) (except [e Py2HyReturnException] e.retvalue)))
(defn run_cmd [cmd &optional [stdin_data None] [expect 0] [dontwritebytecode False]] 
 "Using a hacky implementation of `return`" 
 (try (do (do (setv env None)) 
 (when dontwritebytecode (do (do (setv env (dict os.environ))) 
 (do (assoc env "PYTHONDONTWRITEBYTECODE" "1")))) 
 (do (setv p (subprocess.Popen (os.path.join hy_dir cmd) :stdin subprocess.PIPE :stdout subprocess.PIPE :stderr subprocess.PIPE :universal_newlines True :shell True :env env))) 
 (when (is_not stdin_data None) (do (p.stdin.write stdin_data) 
 (p.stdin.flush) 
 (p.stdin.close))) 
 (do (setv stdout "")) 
 (do (setv stderr "")) 
 (while (is (p.poll) None) (setv stdout (+ stdout (p.stdout.read))) (setv stderr (+ stderr (p.stderr.read)))) 
 (assert (= p.returncode expect)) 
 (raise (Py2HyReturnException (, stdout stderr)))) (except [e Py2HyReturnException] e.retvalue)))
(defn rm [fpath] 
 "Using a hacky implementation of `return`" 
 (try (do (try (do (os.remove fpath)) (except [e Py2HyReturnException] (raise e)) (except [[IOError OSError]] (try (do (os.rmdir fpath)) (except [e Py2HyReturnException] (raise e)) (except [[IOError OSError]] (do)))))) (except [e Py2HyReturnException] e.retvalue)))
(defn test_bin_hy [] 
 (run_cmd "hyhy" ""))
(defn test_bin_hy_stdin [] 
 (do (setv _py2hy_anon_var_G_1235 (run_cmd "hyhy" "(koan)")) (do (setv output (nth _py2hy_anon_var_G_1235 0)) (do))) 
 (assert (in "monk" output)) 
 (do (setv _py2hy_anon_var_G_1236 (run_cmd "hyhy --spy" "(koan)")) (do (setv output (nth _py2hy_anon_var_G_1236 0)) (do))) 
 (assert (in "monk" output)) 
 (assert (in "\\n  Ummon" output)) 
 (do (setv _py2hy_anon_var_G_1237 (run_cmd "hyhy --spy" "(foof)")) (do (setv output (nth _py2hy_anon_var_G_1237 0)) (do))) 
 (assert (in "foof()" output)))
(defn test_bin_hy_stdin_multiline [] 
 (do (setv _py2hy_anon_var_G_1238 (run_cmd "hyhy" "(+ \"a\" \"b\"
\"c\" \"d\")")) (do (setv output (nth _py2hy_anon_var_G_1238 0)) (do))) 
 (assert (in "'abcd'" output)))
(defn test_bin_hy_stdin_comments [] 
 (do (setv _py2hy_anon_var_G_1239 (run_cmd "hyhy" "")) (do (do) (setv err_empty (nth _py2hy_anon_var_G_1239 1)))) 
 (do (setv _py2hy_anon_var_G_1240 (run_cmd "hyhy" "(+ \"a\" \"b\") ; \"c\"")) (do (setv output (nth _py2hy_anon_var_G_1240 0)) (setv err (nth _py2hy_anon_var_G_1240 1)))) 
 (assert (in "'ab'" output)) 
 (assert (= err err_empty)) 
 (do (setv _py2hy_anon_var_G_1241 (run_cmd "hyhy" "; 1")) (do (do) (setv err (nth _py2hy_anon_var_G_1241 1)))) 
 (assert (= err err_empty)))
(defn test_bin_hy_stdin_assignment [] 
 (do (setv _py2hy_anon_var_G_1242 (run_cmd "hyhy" "(setv x (+ \"A\" \"Z\"))")) (do (setv output (nth _py2hy_anon_var_G_1242 0)) (do))) 
 (assert (not_in "AZ" output)) 
 (do (setv _py2hy_anon_var_G_1243 (run_cmd "hyhy" "(setv x (+ \"A\" \"Z\")) (+ \"B\" \"Y\")")) (do (setv output (nth _py2hy_anon_var_G_1243 0)) (do))) 
 (assert (not_in "AZ" output)) 
 (assert (in "BY" output)) 
 (do (setv _py2hy_anon_var_G_1244 (run_cmd "hyhy" "(+ \"B\" \"Y\") (setv x (+ \"A\" \"Z\"))")) (do (setv output (nth _py2hy_anon_var_G_1244 0)) (do))) 
 (assert (not_in "AZ" output)) 
 (assert (not_in "BY" output)))
(defn test_bin_hy_stdin_as_arrow [] 
 (do (setv _py2hy_anon_var_G_1245 (run_cmd "hyhy" "(as-> 0 it (inc it) (inc it))")) (do (setv output (nth _py2hy_anon_var_G_1245 0)) (do))) 
 (assert (re.match "=>\\s+2L?\\s+=>" output)))
(defn test_bin_hy_stdin_error_underline_alignment [] 
 (do (setv _py2hy_anon_var_G_1246 (run_cmd "hy" "(defmacro mabcdefghi [x] x)
(mabcdefghi)")) (do (do) (setv err (nth _py2hy_anon_var_G_1246 1)))) 
 (assert (in "
  (mabcdefghi)
  ^----------^" err)))
(defn test_bin_hy_stdin_except_do [] 
 (do (setv _py2hy_anon_var_G_1247 (run_cmd "hy" "(try (/ 1 0) (except [ZeroDivisionError] \"hello\"))")) (do (setv output (nth _py2hy_anon_var_G_1247 0)) (do))) 
 (assert (in "hello" output)) 
 (do (setv _py2hy_anon_var_G_1248 (run_cmd "hy" "(try (/ 1 0) (except [ZeroDivisionError] \"aaa\" \"bbb\" \"ccc\"))")) (do (setv output (nth _py2hy_anon_var_G_1248 0)) (do))) 
 (assert (not_in "aaa" output)) 
 (assert (not_in "bbb" output)) 
 (assert (in "ccc" output)) 
 (do (setv _py2hy_anon_var_G_1249 (run_cmd "hy" "(if True (do \"xxx\" \"yyy\" \"zzz\"))")) (do (setv output (nth _py2hy_anon_var_G_1249 0)) (do))) 
 (assert (not_in "xxx" output)) 
 (assert (not_in "yyy" output)) 
 (assert (in "zzz" output)))
(defn test_bin_hy_stdin_hy_repr [] 
 (do (setv _py2hy_anon_var_G_1250 (run_cmd "hy" "(+ [1] [2])")) (do (setv output (nth _py2hy_anon_var_G_1250 0)) (do))) 
 (assert (in "[1, 2]" (output.replace "L" ""))) 
 (do (setv _py2hy_anon_var_G_1251 (run_cmd (hr) "(+ [1] [2])")) (do (setv output (nth _py2hy_anon_var_G_1251 0)) (do))) 
 (assert (in "[1 2]" output)) 
 (do (setv _py2hy_anon_var_G_1252 (run_cmd (hr "--spy") "(+ [1] [2])")) (do (setv output (nth _py2hy_anon_var_G_1252 0)) (do))) 
 (assert (in "[1]+[2]" ((. (output.replace "L" "") replace) " " ""))) 
 (assert (in "[1 2]" output)) 
 (do (setv _py2hy_anon_var_G_1253 (run_cmd (hr "--spy") "(+ [1] [2] (foof))")) (do (setv output (nth _py2hy_anon_var_G_1253 0)) (do))) 
 (assert (in "[1]+[2]" ((. (output.replace "L" "") replace) " " ""))))
(defn test_bin_hy_cmd [] 
 (do (setv _py2hy_anon_var_G_1254 (run_cmd "hy -c \"(koan)\"")) (do (setv output (nth _py2hy_anon_var_G_1254 0)) (do))) 
 (assert (in "monk" output)) 
 (do (setv _py2hy_anon_var_G_1255 (run_cmd "hy -c \"(koan\"" :expect 1)) (do (do) (setv err (nth _py2hy_anon_var_G_1255 1)))) 
 (assert (in "Premature end of input" err)))
(defn test_bin_hy_icmd [] 
 (do (setv _py2hy_anon_var_G_1256 (run_cmd "hy -i \"(koan)\"" "(ideas)")) (do (setv output (nth _py2hy_anon_var_G_1256 0)) (do))) 
 (assert (in "monk" output)) 
 (assert (in "figlet" output)))
(defn test_bin_hy_icmd_file [] 
 (do (setv _py2hy_anon_var_G_1257 (run_cmd "hy -i resources/icmd_test_file.hy" "(ideas)")) (do (setv output (nth _py2hy_anon_var_G_1257 0)) (do))) 
 (assert (in "Hy!" output)))
(defn test_bin_hy_icmd_and_spy [] 
 (do (setv _py2hy_anon_var_G_1258 (run_cmd "hy -i \"(+ [] [])\" --spy" "(+ 1 1)")) (do (setv output (nth _py2hy_anon_var_G_1258 0)) (do))) 
 (assert (in "([] + [])" output)))
(defn test_bin_hy_missing_file [] 
 (do (setv _py2hy_anon_var_G_1259 (run_cmd "hy foobarbaz" :expect 2)) (do (do) (setv err (nth _py2hy_anon_var_G_1259 1)))) 
 (assert (in "No such file" err)))
(defn test_bin_hy_file_with_args [] 
 (assert (in "usage" (get (run_cmd "hy tests/resources/argparse_ex.hy -h") 0))) 
 (assert (in "got c" (get (run_cmd "hy tests/resources/argparse_ex.hy -c bar") 0))) 
 (assert (in "foo" (get (run_cmd "hy tests/resources/argparse_ex.hy -i foo") 0))) 
 (assert (in "foo" (get (run_cmd "hy tests/resources/argparse_ex.hy -i foo -c bar") 0))))
(defn test_bin_hyc [] 
 (do (setv _py2hy_anon_var_G_1260 (run_cmd "hyc" :expect 2)) (do (do) (setv err (nth _py2hy_anon_var_G_1260 1)))) 
 (assert (in "usage" err)) 
 (do (setv _py2hy_anon_var_G_1261 (run_cmd "hyc -h")) (do (setv output (nth _py2hy_anon_var_G_1261 0)) (do))) 
 (assert (in "usage" output)) 
 (do (setv path "tests/resources/argparse_ex.hy")) 
 (do (setv _py2hy_anon_var_G_1262 (run_cmd (+ "hyc " path))) (do (setv output (nth _py2hy_anon_var_G_1262 0)) (do))) 
 (assert (in "Compiling" output)) 
 (assert (os.path.exists (get_bytecode_path path))) 
 (rm (get_bytecode_path path)))
(defn test_bin_hyc_missing_file [] 
 (do (setv _py2hy_anon_var_G_1263 (run_cmd "hyc foobarbaz" :expect 2)) (do (do) (setv err (nth _py2hy_anon_var_G_1263 1)))) 
 (assert (in "[Errno 2]" err)))
(defn test_hy2py [] 
 (do (setv i 0)) 
 (for [[dirpath dirnames filenames] (os.walk "tests/native_tests")] 
 (for [f filenames] 
 (when (f.endswith ".hy") (do (when (and (= f "py3_only_tests.hy") (not PY3)) (do (continue))) 
 (when (and (= f "py35_only_tests.hy") (not PY35)) (do (continue))) 
 (setv i (+ i 1)) 
 (do (setv _py2hy_anon_var_G_1264 (run_cmd (+ "hy2py -s -a " (os.path.join dirpath f)))) (do (setv output (nth _py2hy_anon_var_G_1264 0)) (setv err (nth _py2hy_anon_var_G_1264 1)))) 
 (assert (> (len output) 1) f) 
 (assert (= (len err) 0) f))))) 
 (assert i))
(defn test_bin_hy_builtins [] 
 (import [hyhy.cmdline]) 
 (assert (= (str exit) "Use (exit) or Ctrl-D (i.e. EOF) to exit")) 
 (assert (= (str quit) "Use (quit) or Ctrl-D (i.e. EOF) to exit")))
(defn test_bin_hy_main [] 
 (do (setv _py2hy_anon_var_G_1265 (run_cmd "hy tests/resources/bin/main.hy")) (do (setv output (nth _py2hy_anon_var_G_1265 0)) (do))) 
 (assert (in "Hello World" output)))
(defn test_bin_hy_main_args [] 
 (do (setv _py2hy_anon_var_G_1266 (run_cmd "hy tests/resources/bin/main.hy test 123")) (do (setv output (nth _py2hy_anon_var_G_1266 0)) (do))) 
 (assert (in "test" output)) 
 (assert (in "123" output)))
(defn test_bin_hy_main_exitvalue [] 
 (run_cmd "hy tests/resources/bin/main.hy exit1" :expect 1))
(defn test_bin_hy_no_main [] 
 (do (setv _py2hy_anon_var_G_1267 (run_cmd "hy tests/resources/bin/nomain.hy")) (do (setv output (nth _py2hy_anon_var_G_1267 0)) (do))) 
 (assert (in "This Should Still Work" output)))
(with_decorator 
 (pytest.mark.parametrize "scenario" ["normal" "prevent_by_force" "prevent_by_env"]) (pytest.mark.parametrize "cmd_fmt" ["hy {fpath}" "hy -m {modname}" "hy -c '(import {modname})'"]) 
 (defn test_bin_hy_byte_compile [scenario cmd_fmt] 
 (do (setv modname "tests.resources.bin.bytecompile")) 
 (do (setv fpath (+ (modname.replace "." "/") ".hy"))) 
 (do (setv cmd (cmd_fmt.format (unpack_mapping (locals))))) 
 (rm (get_bytecode_path fpath)) 
 (when (= scenario "prevent_by_force") (do (os.mkdir (get_bytecode_path fpath)))) 
 (do (setv _py2hy_anon_var_G_1268 (run_cmd cmd :dontwritebytecode (= scenario "prevent_by_env"))) (do (setv output (nth _py2hy_anon_var_G_1268 0)) (do))) 
 (assert (in "Hello from macro" output)) 
 (assert (in "The macro returned: boink" output)) 
 (if (= scenario "normal") (do (assert (os.path.exists (get_bytecode_path fpath)))) (do (when (= scenario "prevent_by_env") (do (assert (not (os.path.exists (get_bytecode_path fpath)))))))) 
 (do (setv _py2hy_anon_var_G_1269 (run_cmd cmd)) (do (setv output (nth _py2hy_anon_var_G_1269 0)) (do))) 
 (assert (^ (in "Hello from macro" output) (= scenario "normal"))) 
 (assert (in "The macro returned: boink" output))))
(defn test_bin_hy_module_main [] 
 (do (setv _py2hy_anon_var_G_1270 (run_cmd "hy -m tests.resources.bin.main")) (do (setv output (nth _py2hy_anon_var_G_1270 0)) (do))) 
 (assert (in "Hello World" output)))
(defn test_bin_hy_module_main_args [] 
 (do (setv _py2hy_anon_var_G_1271 (run_cmd "hy -m tests.resources.bin.main test 123")) (do (setv output (nth _py2hy_anon_var_G_1271 0)) (do))) 
 (assert (in "test" output)) 
 (assert (in "123" output)))
(defn test_bin_hy_module_main_exitvalue [] 
 (run_cmd "hy -m tests.resources.bin.main exit1" :expect 1))
(defn test_bin_hy_module_no_main [] 
 (do (setv _py2hy_anon_var_G_1272 (run_cmd "hy -m tests.resources.bin.nomain")) (do (setv output (nth _py2hy_anon_var_G_1272 0)) (do))) 
 (assert (in "This Should Still Work" output)))
