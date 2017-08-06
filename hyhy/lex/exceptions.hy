(import [hyhy.errors [HyError]])
(defclass Py2HyReturnException [Exception] (defn __init__ [self retvalue] (setv self.retvalue retvalue)))
(defclass LexException [HyError] "Error during the Lexing of a Hython expression." 
 (defn __init__ [self message lineno colno &optional [source None]] 
 ((. (super LexException self) __init__) message) 
 (do (setv self.message message)) 
 (do (setv self.lineno lineno)) 
 (do (setv self.colno colno)) 
 (do (setv self.source source)) 
 (do (setv self.filename "<stdin>"))) 
 (defn __str__ [self] 
 "Using a hacky implementation of `return`" 
 (try (do (import [hyhy.errors [colored]]) 
 (do (setv line self.lineno)) 
 (do (setv start self.colno)) 
 (do (setv result "")) 
 (do (setv source (self.source.split "
"))) 
 (when (and (> line 0) (> start 0)) (do (setv result (+ result (% "  File \"%s\", line %d, column %d

" (, self.filename line start)))) 
 (if (> (len self.source) 0) (do (do (setv source_line (get source (- line 1))))) (do (do (setv source_line "")))) 
 (setv result (+ result (% "  %s
" (colored.red source_line)))) 
 (setv result (+ result (% "  %s%s
" (, (* " " (- start 1)) (colored.green "^"))))))) 
 (setv result (+ result (colored.yellow (% "LexException: %s

" self.message)))) 
 (raise (Py2HyReturnException result))) (except [e Py2HyReturnException] e.retvalue))))
(defclass PrematureEndOfInput [LexException] "We got a premature end of input" 
 (defn __init__ [self message] 
 ((. (super PrematureEndOfInput self) __init__) message (- 1) (- 1))))
