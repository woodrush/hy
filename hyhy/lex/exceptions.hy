(import [hyhy.errors [HyError]])
(defclass Py2HyReturnException [Exception] (defn __init__ [self retvalue] (setv self.retvalue retvalue)))
(defclass LexException [HyError] "Error during the Lexing of a Hython expression." (defn __init__ [self message lineno colno &optional [source None]] ((. (super LexException self) __init__) message) (setv (. self message) message) (setv (. self lineno) lineno) (setv (. self colno) colno) (setv (. self source) source) (setv (. self filename) "<stdin>")) (defn __str__ [self] "Using a hacky implementation of `return`" (try (do (import [hyhy.errors [colored]]) (setv line (. self lineno)) (setv start (. self colno)) (setv result "") (setv source ((. (. self source) split) "
")) (when (and (> line 0) (> start 0)) (do (setv result (+ result (% "  File \"%s\", line %d, column %d

" (, (. self filename) line start)))) (if (> (len (. self source)) 0) (do (setv source_line (get source (- line 1)))) (do (setv source_line ""))) (setv result (+ result (% "  %s
" ((. colored red) source_line)))) (setv result (+ result (% "  %s%s
" (, (* " " (- start 1)) ((. colored green) "^"))))))) (setv result (+ result ((. colored yellow) (% "LexException: %s

" (. self message))))) (raise (Py2HyReturnException result))) (except [e Py2HyReturnException] e.retvalue))))
(defclass PrematureEndOfInput [LexException] "We got a premature end of input" (defn __init__ [self message] ((. (super PrematureEndOfInput self) __init__) message (- 1) (- 1))))
