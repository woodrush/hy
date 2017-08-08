(import [functools [wraps]])
(defclass Py2HyReturnException [Exception]
  (defn __init__ [self retvalue]
    (setv self.retvalue retvalue)))
(import [ast [literal_eval]])
(import [rply [ParserGenerator]])
(import [hyhy._compat [PY3 str_type]])
(import [hyhy.models [HyBytes HyComplex HyCons HyDict HyExpression HyFloat HyInteger HyKeyword HyList HySet HyString HySymbol]])
(import [hyhy.lex.lexer [lexer]])
(import [hyhy.lex.exceptions [LexException PrematureEndOfInput]])
(setv pg (ParserGenerator (+ (list_comp rule.name [rule lexer.rules]) ["$end"]) :cache_id "hy_parser"))
(defn hy_symbol_mangle [p]
  (when (and (p.startswith "*") (p.endswith "*") (not_in p (, "*" "**")))
    (setv p ((. (get p (slice 1 (- 1) None)) upper))))
  (when (and (in "-" p) (!= p "-"))
    (setv p (p.replace "-" "_")))
  (when (and (p.endswith "?") (!= p "?"))
    (setv p (% "is_%s" (get p (slice None (- 1) None)))))
  (when (and (p.endswith "!") (!= p "!"))
    (setv p (% "%s_bang" (get p (slice None (- 1) None)))))
  p)
(defn hy_symbol_unmangle [p]
  (setv p (str_type p))
  (when (and (p.endswith "_bang") (!= p "_bang"))
    (setv p (+ (get p (slice None (- (len "_bang")) None)) "!")))
  (when (and (p.startswith "is_") (!= p "is_"))
    (setv p (+ (get p (slice (len "is_") None None)) "?")))
  (when (and (in "_" p) (!= p "_"))
    (setv p (p.replace "_" "-")))
  (when (and (all (list_comp (or (and (c.isalpha) (c.isupper)) (= c "_")) [c p])) (any (list_comp (c.isalpha) [c p])))
    (setv p (+ (+ "*" (p.lower)) "*")))
  p)
(defn set_boundaries [fun]
  (with_decorator
    (wraps fun)
    (defn wrapped [p]
      (setv start (. (get p 0) source_pos))
      (setv end (. (get p (- 1)) source_pos))
      (setv ret (fun p))
      (setv ret.start_line start.lineno)
      (setv ret.start_column start.colno)
      (if (is_not start end)
        (do
          (setv ret.end_line end.lineno)
          (setv ret.end_column end.colno))
        (do
          (setv ret.end_line start.lineno)
          (setv ret.end_column (+ start.colno (len (. (get p 0) value))))))
      ret))
  wrapped)
(defn set_quote_boundaries [fun]
  (with_decorator
    (wraps fun)
    (defn wrapped [p]
      (setv start (. (get p 0) source_pos))
      (setv ret (fun p))
      (setv ret.start_line start.lineno)
      (setv ret.start_column start.colno)
      (setv ret.end_line (. (get p (- 1)) end_line))
      (setv ret.end_column (. (get p (- 1)) end_column))
      ret))
  wrapped)
(with_decorator
  (pg.production "main : list_contents")
  (defn main [p]
    (get p 0)))
(with_decorator
  (pg.production "main : $end")
  (defn main_empty [p]
    []))
(defn reject_spurious_dots [&rest items]
  "Reject the spurious dots from items"
  (for [list items]
    (for [tok list]
      (when (and (= tok ".") (= (type tok) HySymbol))
        (raise (LexException "Malformed dotted list" tok.start_line tok.start_column))))))
(with_decorator
  (pg.production "paren : LPAREN list_contents RPAREN")
  set_boundaries
  (defn paren [p]
    (try
      (do
        (setv cont (get p 1))
        (when (and (>= (len cont) 3) (isinstance (get cont (- 2)) HySymbol) (= (get cont (- 2)) "."))
          (reject_spurious_dots (get cont (slice None (- 2) None)) (get cont (slice (- 1) None None)))
          (if (= (len cont) 3)
            (do
              (raise (Py2HyReturnException (HyCons (get cont 0) (get cont 2)))))
            (do
              (raise (Py2HyReturnException (HyCons (get cont 0) (paren [(get p 0) (get cont (slice 1 None None)) (get p 2)])))))))
        (reject_spurious_dots (get cont (slice 1 None None)))
        (raise (Py2HyReturnException (HyExpression (get p 1)))))
      (except [e Py2HyReturnException]
        e.retvalue))))
(with_decorator
  (pg.production "paren : LPAREN RPAREN")
  set_boundaries
  (defn empty_paren [p]
    (HyExpression [])))
(with_decorator
  (pg.production "list_contents : term list_contents")
  (defn list_contents [p]
    (+ [(get p 0)] (get p 1))))
(with_decorator
  (pg.production "list_contents : term")
  (defn list_contents_single [p]
    [(get p 0)]))
(with_decorator
  (pg.production "term : identifier")
  (pg.production "term : paren")
  (pg.production "term : dict")
  (pg.production "term : list")
  (pg.production "term : set")
  (pg.production "term : string")
  (defn term [p]
    (get p 0)))
(with_decorator
  (pg.production "term : QUOTE term")
  set_quote_boundaries
  (defn term_quote [p]
    (HyExpression [(HySymbol "quote") (get p 1)])))
(with_decorator
  (pg.production "term : QUASIQUOTE term")
  set_quote_boundaries
  (defn term_quasiquote [p]
    (HyExpression [(HySymbol "quasiquote") (get p 1)])))
(with_decorator
  (pg.production "term : UNQUOTE term")
  set_quote_boundaries
  (defn term_unquote [p]
    (HyExpression [(HySymbol "unquote") (get p 1)])))
(with_decorator
  (pg.production "term : UNQUOTESPLICE term")
  set_quote_boundaries
  (defn term_unquote_splice [p]
    (HyExpression [(HySymbol "unquote_splice") (get p 1)])))
(with_decorator
  (pg.production "term : HASHSTARS term")
  set_quote_boundaries
  (defn term_hashstars [p]
    (setv n_stars (len (get ((. (get p 0) getstr)) (slice 1 None None))))
    (cond
      [(= n_stars 1)
       (setv sym "unpack_iterable")]
      [(= n_stars 2)
       (setv sym "unpack_mapping")]
      [True
       (raise (LexException "Too many stars in `#*` construct (if you want to unpack a symbol beginning with a star, separate it with whitespace)" (. (. (get p 0) source_pos) lineno) (. (. (get p 0) source_pos) colno)))])
    (HyExpression [(HySymbol sym) (get p 1)])))
(with_decorator
  (pg.production "term : HASHOTHER term")
  set_quote_boundaries
  (defn hash_other [p]
    (setv st (get ((. (get p 0) getstr)) (slice 1 None None)))
    (setv str_object (HyString st))
    (setv expr (get p 1))
    (HyExpression [(HySymbol "dispatch_tag_macro") str_object expr])))
(with_decorator
  (pg.production "set : HLCURLY list_contents RCURLY")
  set_boundaries
  (defn t_set [p]
    (HySet (get p 1))))
(with_decorator
  (pg.production "set : HLCURLY RCURLY")
  set_boundaries
  (defn empty_set [p]
    (HySet [])))
(with_decorator
  (pg.production "dict : LCURLY list_contents RCURLY")
  set_boundaries
  (defn t_dict [p]
    (HyDict (get p 1))))
(with_decorator
  (pg.production "dict : LCURLY RCURLY")
  set_boundaries
  (defn empty_dict [p]
    (HyDict [])))
(with_decorator
  (pg.production "list : LBRACKET list_contents RBRACKET")
  set_boundaries
  (defn t_list [p]
    (HyList (get p 1))))
(with_decorator
  (pg.production "list : LBRACKET RBRACKET")
  set_boundaries
  (defn t_empty_list [p]
    (HyList [])))
(if PY3
  (do
    (defn uni_hystring [s]
      (HyString (literal_eval s)))
    (defn hybytes [s]
      (HyBytes (literal_eval (+ "b" s)))))
  (do
    (defn uni_hystring [s]
      (HyString (literal_eval (+ "u" s))))
    (defn hybytes [s]
      (HyBytes (literal_eval s)))))
(with_decorator
  (pg.production "string : STRING")
  set_boundaries
  (defn t_string [p]
    (setv s (get (. (get p 0) value) (slice None (- 1) None)))
    (do
      (setv _py2hy_anon_var_G_1236 (s.split "\"" 1))
      (setv header (nth _py2hy_anon_var_G_1236 0))
      (setv s (nth _py2hy_anon_var_G_1236 1)))
    (setv header (header.replace "u" ""))
    (setv is_bytestring (in "b" header))
    (setv header (header.replace "b" ""))
    (setv s (+ (+ (+ header "\"\"\"") s) "\"\"\""))
    ((if is_bytestring
       hybytes
       uni_hystring) s)))
(with_decorator
  (pg.production "string : PARTIAL_STRING")
  (defn t_partial_string [p]
    (raise (PrematureEndOfInput "Premature end of input"))))
(with_decorator
  (pg.production "identifier : IDENTIFIER")
  set_boundaries
  (defn t_identifier [p]
    (try
      (do
        (setv obj (. (get p 0) value))
        (setv val (symbol_like obj))
        (when (is_not val None)
          (raise (Py2HyReturnException val)))
        (when (and (in "." obj) (is_not (symbol_like (get (obj.split "." 1) 0)) None))
          (raise (LexException "Cannot access attribute on anything other than a name (in order to get attributes of expressions, use `(. <expression> <attr>)` or `(.<attr> <expression>)`)" (. (. (get p 0) source_pos) lineno) (. (. (get p 0) source_pos) colno))))
        (raise (Py2HyReturnException (HySymbol ((. "." join) (genexpr (hy_symbol_mangle x) [x (obj.split ".")]))))))
      (except [e Py2HyReturnException]
        e.retvalue))))
(defn symbol_like [obj]
  "Try to interpret `obj` as a number or keyword."
  (try
    (do
      (try
        (raise (Py2HyReturnException (HyInteger obj)))
        (except [e Py2HyReturnException]
          (raise e))
        (except [ValueError]
          (do)))
      (when (in "/" obj)
        (try
          (do
            (do
              (setv _py2hy_anon_var_G_1238 (obj.split "/"))
              (setv lhs (nth _py2hy_anon_var_G_1238 0))
              (setv rhs (nth _py2hy_anon_var_G_1238 1)))
            (raise (Py2HyReturnException (HyExpression [(HySymbol "fraction") (HyInteger lhs) (HyInteger rhs)]))))
          (except [e Py2HyReturnException]
            (raise e))
          (except [ValueError]
            (do))))
      (try
        (raise (Py2HyReturnException (HyFloat obj)))
        (except [e Py2HyReturnException]
          (raise e))
        (except [ValueError]
          (do)))
      (when (!= obj "j")
        (try
          (raise (Py2HyReturnException (HyComplex obj)))
          (except [e Py2HyReturnException]
            (raise e))
          (except [ValueError]
            (do))))
      (when (and (obj.startswith ":") (not_in "." obj))
        (raise (Py2HyReturnException (HyKeyword obj)))))
    (except [e Py2HyReturnException]
      e.retvalue)))
(with_decorator
  pg.error
  (defn error_handler [token]
    (setv tokentype (token.gettokentype))
    (if (= tokentype "$end")
      (do
        (raise (PrematureEndOfInput "Premature end of input")))
      (do
        (raise (LexException (% "Ran into a %s where it wasn't expected." tokentype) token.source_pos.lineno token.source_pos.colno))))))
(setv parser (pg.build))
