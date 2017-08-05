(import [traceback])
(defclass Py2HyReturnException [Exception] (defn __init__ [self retvalue] (setv self.retvalue retvalue)))
(import [clint.textui [colored]])
(defclass HyError [Exception] "
    Generic Hy error. All internal Exceptions will be subclassed from this
    Exception.
    " None)
(defclass HyCompileError [HyError] (defn __init__ [self exception &optional [traceback None]] (setv (. self exception) exception) (setv (. self traceback) traceback)) (defn __str__ [self] "Using a hacky implementation of `return`" (try (do (when (isinstance (. self exception) HyTypeError) (do (raise (Py2HyReturnException (str (. self exception)))))) (if (. self traceback) (do (setv tb ((. ((. "" join) ((. traceback format_tb) (. self traceback))) strip)))) (do (setv tb "No traceback available. 😟"))) (raise (Py2HyReturnException (% "Internal Compiler Bug 😱
⤷ %s: %s
Compilation traceback:
%s" (, (. (. (. self exception) __class__) __name__) (. self exception) tb))))) (except [e Py2HyReturnException] e.retvalue))))
(defclass HyTypeError [TypeError] (defn __init__ [self expression message] ((. (super HyTypeError self) __init__) message) (setv (. self expression) expression) (setv (. self message) message) (setv (. self source) None) (setv (. self filename) None)) (defn __str__ [self] "Using a hacky implementation of `return`" (try (do (setv line (. (. self expression) start_line)) (setv start (. (. self expression) start_column)) (setv end (. (. self expression) end_column)) (setv source []) (when (is_not (. self source) None) (do (setv source (get ((. (. self source) split) "
") (slice (- line 1) (. (. self expression) end_line) None))) (if (= line (. (. self expression) end_line)) (do (setv length (- end start))) (do (setv length (- (len (get source 0)) start)))))) (setv result "") (setv result (+ result (% "  File \"%s\", line %d, column %d

" (, (. self filename) line start)))) (when (= (len source) 1) (do (setv result (+ result (% "  %s
" ((. colored red) (get source 0))))) (setv result (+ result (% "  %s%s
" (, (* " " (- start 1)) ((. colored green) (+ (+ "^" (* "-" (- length 1))) "^")))))))) (when (> (len source) 1) (do (setv result (+ result (% "  %s
" ((. colored red) (get source 0))))) (setv result (+ result (% "  %s%s
" (, (* " " (- start 1)) ((. colored green) (+ "^" (* "-" length))))))) (when (> (len source) 2) (do (for [line (get source (slice 1 (- 1) None))] (setv result (+ result (% "  %s
" ((. colored red) ((. "" join) line))))) (setv result (+ result (% "  %s
" ((. colored green) (* "-" (len line))))))))) (setv result (+ result (% "  %s
" ((. colored red) ((. "" join) (get source (- 1))))))) (setv result (+ result (% "  %s
" ((. colored green) (+ (* "-" (- end 1)) "^"))))))) (setv result (+ result ((. colored yellow) (% "%s: %s

" (, (. (. self __class__) __name__) ((. (. self message) encode) "utf-8")))))) (raise (Py2HyReturnException result))) (except [e Py2HyReturnException] e.retvalue))))
(defclass HyMacroExpansionError [HyTypeError] None)
(defclass HyIOError [HyError IOError] "
    Trivial subclass of IOError and HyError, to distinguish between
    IOErrors raised by Hy itself as opposed to Hy programs.
    " None)