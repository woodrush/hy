(import [math [isnan]])
(defclass Py2HyReturnException [Exception]
  (defn __init__ [self retvalue]
    (setv self.retvalue retvalue)))
(import [hyhy.models [HyExpression HyInteger HyFloat HyComplex HySymbol HyString HyDict HyList HySet HyCons]])
(import [hyhy.lex [LexException PrematureEndOfInput tokenize]])
(import [pytest])
(defn peoi []
  (pytest.raises PrematureEndOfInput))
(defn lexe []
  (pytest.raises LexException))
(defn test_lex_exception []
  " Ensure tokenize throws a fit on a partial input "
  (with [(peoi)] (tokenize "(foo"))
  (with [(peoi)] (tokenize "{foo bar"))
  (with [(peoi)] (tokenize "(defn foo [bar]"))
  (with [(peoi)] (tokenize "(foo \"bar"))
  None)
(defn test_unbalanced_exception []
  "Ensure the tokenization fails on unbalanced expressions"
  (with [(lexe)] (tokenize "(bar))"))
  (with [(lexe)] (tokenize "(baz [quux]])"))
  None)
(defn test_lex_single_quote_err []
  "Ensure tokenizing \"' \" throws a LexException that can be stringified"
  (with [e (lexe)] (tokenize "' "))
  (assert (in "Could not identify the next token" (str e.value))))
(defn test_lex_expression_symbols []
  " Make sure that expressions produce symbols "
  (setv objs (tokenize "(foo bar)"))
  (assert (= objs [(HyExpression [(HySymbol "foo") (HySymbol "bar")])])))
(defn test_lex_expression_strings []
  " Test that expressions can produce strings "
  (setv objs (tokenize "(foo \"bar\")"))
  (assert (= objs [(HyExpression [(HySymbol "foo") (HyString "bar")])])))
(defn test_lex_expression_integer []
  " Make sure expressions can produce integers "
  (setv objs (tokenize "(foo 2)"))
  (assert (= objs [(HyExpression [(HySymbol "foo") (HyInteger 2)])])))
(defn test_lex_symbols []
  " Make sure that symbols are valid expressions"
  (setv objs (tokenize "foo "))
  (assert (= objs [(HySymbol "foo")])))
(defn test_lex_strings []
  " Make sure that strings are valid expressions"
  (setv objs (tokenize "\"foo\""))
  (assert (= objs [(HyString "foo")]))
  (setv objs (tokenize "
\"a\\
bc\"
"))
  (assert (= objs [(HyString "abc")])))
(defn test_lex_integers []
  " Make sure that integers are valid expressions"
  (setv objs (tokenize "42 "))
  (assert (= objs [(HyInteger 42)])))
(defn test_lex_fractions []
  " Make sure that fractions are valid expressions"
  (setv objs (tokenize "1/2"))
  (assert (= objs [(HyExpression [(HySymbol "fraction") (HyInteger 1) (HyInteger 2)])])))
(defn test_lex_expression_float []
  " Make sure expressions can produce floats "
  (setv objs (tokenize "(foo 2.)"))
  (assert (= objs [(HyExpression [(HySymbol "foo") (HyFloat 2.0)])]))
  (setv objs (tokenize "(foo -0.5)"))
  (assert (= objs [(HyExpression [(HySymbol "foo") (HyFloat (- 0.5))])]))
  (setv objs (tokenize "(foo 1.e7)"))
  (assert (= objs [(HyExpression [(HySymbol "foo") (HyFloat 10000000.0)])])))
(defn test_lex_nan_and_inf []
  (assert (isnan (get (tokenize "NaN") 0)))
  (assert (= (tokenize "Nan") [(HySymbol "Nan")]))
  (assert (= (tokenize "nan") [(HySymbol "nan")]))
  (assert (= (tokenize "NAN") [(HySymbol "NAN")]))
  (assert (= (tokenize "Inf") [(HyFloat (float "inf"))]))
  (assert (= (tokenize "inf") [(HySymbol "inf")]))
  (assert (= (tokenize "INF") [(HySymbol "INF")]))
  (assert (= (tokenize "-Inf") [(HyFloat (float "-inf"))]))
  (assert (= (tokenize "-inf") [(HySymbol "_inf")]))
  (assert (= (tokenize "-INF") [(HySymbol "_INF")])))
(defn test_lex_expression_complex []
  " Make sure expressions can produce complex "
  (defn t [x]
    (tokenize ((. "(foo {})" format) x)))
  (defn f [x]
    [(HyExpression [(HySymbol "foo") x])])
  (assert (= (t "2.j") (f (HyComplex 2j))))
  (assert (= (t "-0.5j") (f (HyComplex (- 0.5j)))))
  (assert (= (t "1.e7j") (f (HyComplex 10000000j))))
  (assert (= (t "j") (f (HySymbol "j"))))
  (assert (isnan (. (get (get (t "NaNj") 0) 1) imag)))
  (assert (= (t "nanj") (f (HySymbol "nanj"))))
  (assert (= (t "Inf+Infj") (f (HyComplex (complex (float "inf") (float "inf"))))))
  (assert (= (t "Inf-Infj") (f (HyComplex (complex (float "inf") (float "-inf"))))))
  (assert (= (t "Inf-INFj") (f (HySymbol "Inf_INFj")))))
(defn test_lex_digit_separators []
  (assert (= (tokenize "1_000_000") [(HyInteger 1000000)]))
  (assert (= (tokenize "1,000,000") [(HyInteger 1000000)]))
  (assert (= (tokenize "1,000_000") [(HyInteger 1000000)]))
  (assert (= (tokenize "1_000,000") [(HyInteger 1000000)]))
  (assert (= (tokenize "0x_af") [(HyInteger 175)]))
  (assert (= (tokenize "0x,af") [(HyInteger 175)]))
  (assert (= (tokenize "0b_010") [(HyInteger 2)]))
  (assert (= (tokenize "0b,010") [(HyInteger 2)]))
  (assert (= (tokenize "0o_373") [(HyInteger 251)]))
  (assert (= (tokenize "0o,373") [(HyInteger 251)]))
  (assert (= (tokenize "1_2.3,4") [(HyFloat 12.34)]))
  (assert (= (tokenize "1_2e3,4") [(HyFloat 1.2e+35)]))
  (assert (= (tokenize "1,2/3_4") [(HyExpression [(HySymbol "fraction") (HyInteger 12) (HyInteger 34)])]))
  (assert (= (tokenize "1,0_00j") [(HyComplex 1000j)]))
  (assert (= (tokenize ",,,,___,__1__,,__,,2__,,,__") [(HyInteger 12)]))
  (assert (= (tokenize ",,,,___,__1__,,__,,2__,q,__") [(HySymbol ",,,,___,__1__,,__,,2__,q,__")])))
(defn test_lex_bad_attrs []
  (with [(lexe)] (tokenize "1.foo"))
  (with [(lexe)] (tokenize "0.foo"))
  (with [(lexe)] (tokenize "1.5.foo"))
  (with [(lexe)] (tokenize "1e3.foo"))
  (with [(lexe)] (tokenize "5j.foo"))
  (with [(lexe)] (tokenize "3+5j.foo"))
  (with [(lexe)] (tokenize "3.1+5.1j.foo"))
  (assert (tokenize "j.foo"))
  (with [(lexe)] (tokenize "3/4.foo"))
  (assert (tokenize "a/1.foo"))
  (assert (tokenize "1/a.foo"))
  (with [(lexe)] (tokenize ":hello.foo"))
  None)
(defn test_lex_line_counting []
  " Make sure we can count lines / columns "
  (setv entry (get (tokenize "(foo (one two))") 0))
  (assert (= entry.start_line 1))
  (assert (= entry.start_column 1))
  (assert (= entry.end_line 1))
  (assert (= entry.end_column 15))
  (setv entry (get entry 1))
  (assert (= entry.start_line 1))
  (assert (= entry.start_column 6))
  (assert (= entry.end_line 1))
  (assert (= entry.end_column 14)))
(defn test_lex_line_counting_multi []
  " Make sure we can do multi-line tokenization "
  (setv entries (tokenize "
(foo (one two))
(foo bar)
"))
  (setv entry (get entries 0))
  (assert (= entry.start_line 2))
  (assert (= entry.start_column 1))
  (assert (= entry.end_line 2))
  (assert (= entry.end_column 15))
  (setv entry (get entries 1))
  (assert (= entry.start_line 3))
  (assert (= entry.start_column 1))
  (assert (= entry.end_line 3))
  (assert (= entry.end_column 9)))
(defn test_lex_line_counting_multi_inner []
  " Make sure we can do multi-line tokenization (inner) "
  (setv entry (get (tokenize "(foo
    bar)") 0))
  (setv inner (get entry 0))
  (assert (= inner.start_line 1))
  (assert (= inner.start_column 2))
  (setv inner (get entry 1))
  (assert (= inner.start_line 2))
  (assert (= inner.start_column 5)))
(defn test_dicts []
  " Ensure that we can tokenize a dict. "
  (setv objs (tokenize "{foo bar bar baz}"))
  (assert (= objs [(HyDict ["foo" "bar" "bar" "baz"])]))
  (setv objs (tokenize "(bar {foo bar bar baz})"))
  (assert (= objs [(HyExpression [(HySymbol "bar") (HyDict ["foo" "bar" "bar" "baz"])])]))
  (setv objs (tokenize "{(foo bar) (baz quux)}"))
  (assert (= objs [(HyDict [(HyExpression [(HySymbol "foo") (HySymbol "bar")]) (HyExpression [(HySymbol "baz") (HySymbol "quux")])])])))
(defn test_sets []
  " Ensure that we can tokenize a set. "
  (setv objs (tokenize "#{1 2}"))
  (assert (= objs [(HySet [(HyInteger 1) (HyInteger 2)])]))
  (setv objs (tokenize "(bar #{foo bar baz})"))
  (assert (= objs [(HyExpression [(HySymbol "bar") (HySet ["foo" "bar" "baz"])])]))
  (setv objs (tokenize "#{(foo bar) (baz quux)}"))
  (assert (= objs [(HySet [(HyExpression [(HySymbol "foo") (HySymbol "bar")]) (HyExpression [(HySymbol "baz") (HySymbol "quux")])])]))
  (setv objs (tokenize "#{1 2 1 1 2 1}"))
  (assert (= objs [(HySet (list_comp (HyInteger n) [n [1 2 1 1 2 1]]))]))
  (assert (= (len (get objs 0)) 6))
  (setv objs (tokenize "#{a 1}"))
  (assert (= objs [(HySet [(HySymbol "a") (HyInteger 1)])])))
(defn test_nospace []
  " Ensure we can tokenize without spaces if we have to "
  (setv entry (get (tokenize "(foo(one two))") 0))
  (assert (= entry.start_line 1))
  (assert (= entry.start_column 1))
  (assert (= entry.end_line 1))
  (assert (= entry.end_column 14))
  (setv entry (get entry 1))
  (assert (= entry.start_line 1))
  (assert (= entry.start_column 5))
  (assert (= entry.end_line 1))
  (assert (= entry.end_column 13)))
(defn test_escapes []
  " Ensure we can escape things "
  (setv entry (get (tokenize "(foo \"foo\\n\")") 0))
  (assert (= (get entry 1) "foo
"))
  (setv entry (get (tokenize "(foo \"foo\\s\")") 0))
  (assert (= (get entry 1) "foo\\s")))
(defn test_unicode_escapes []
  "Ensure unicode escapes are handled correctly"
  (setv s "\"a\\xac\\u1234\\u20ac\\U00008000\"")
  (assert (= (len s) 29))
  (setv entry (get (tokenize s) 0))
  (assert (= (len entry) 5))
  (assert (= (list_comp (ord x) [x entry]) [97 172 4660 8364 32768])))
(defn test_complex []
  "Ensure we tokenize complex numbers properly"
  (setv entry (get (get (tokenize "(1j)") 0) 0))
  (assert (= entry (HyComplex "1.0j")))
  (setv entry (get (get (tokenize "(j)") 0) 0))
  (assert (= entry (HySymbol "j"))))
(defn test_tag_macro []
  "Ensure tag macros are handled properly"
  (setv entry (tokenize "#^()"))
  (assert (= (get (get entry 0) 0) (HySymbol "dispatch_tag_macro")))
  (assert (= (get (get entry 0) 1) (HyString "^")))
  (assert (= (len (get entry 0)) 3)))
(defn test_lex_comment_382 []
  "Ensure that we can tokenize sources with a comment at the end"
  (setv entry (tokenize "foo ;bar
;baz"))
  (assert (= entry [(HySymbol "foo")])))
(defn test_lex_mangling_star []
  "Ensure that mangling starred identifiers works according to plan"
  (setv entry (tokenize "*foo*"))
  (assert (= entry [(HySymbol "FOO")]))
  (setv entry (tokenize "*"))
  (assert (= entry [(HySymbol "*")]))
  (setv entry (tokenize "*foo"))
  (assert (= entry [(HySymbol "*foo")])))
(defn test_lex_mangling_hyphen []
  "Ensure that hyphens get translated to underscores during mangling"
  (setv entry (tokenize "foo-bar"))
  (assert (= entry [(HySymbol "foo_bar")]))
  (setv entry (tokenize "-"))
  (assert (= entry [(HySymbol "-")])))
(defn test_lex_mangling_qmark []
  "Ensure that identifiers ending with a question mark get mangled ok"
  (setv entry (tokenize "foo?"))
  (assert (= entry [(HySymbol "is_foo")]))
  (setv entry (tokenize "?"))
  (assert (= entry [(HySymbol "?")]))
  (setv entry (tokenize "im?foo"))
  (assert (= entry [(HySymbol "im?foo")]))
  (setv entry (tokenize ".foo?"))
  (assert (= entry [(HySymbol ".is_foo")]))
  (setv entry (tokenize "foo.bar?"))
  (assert (= entry [(HySymbol "foo.is_bar")]))
  (setv entry (tokenize "foo?.bar"))
  (assert (= entry [(HySymbol "is_foo.bar")]))
  (setv entry (tokenize ".foo?.bar.baz?"))
  (assert (= entry [(HySymbol ".is_foo.bar.is_baz")])))
(defn test_lex_mangling_bang []
  "Ensure that identifiers ending with a bang get mangled ok"
  (setv entry (tokenize "foo!"))
  (assert (= entry [(HySymbol "foo_bang")]))
  (setv entry (tokenize "!"))
  (assert (= entry [(HySymbol "!")]))
  (setv entry (tokenize "im!foo"))
  (assert (= entry [(HySymbol "im!foo")]))
  (setv entry (tokenize ".foo!"))
  (assert (= entry [(HySymbol ".foo_bang")]))
  (setv entry (tokenize "foo.bar!"))
  (assert (= entry [(HySymbol "foo.bar_bang")]))
  (setv entry (tokenize "foo!.bar"))
  (assert (= entry [(HySymbol "foo_bang.bar")]))
  (setv entry (tokenize ".foo!.bar.baz!"))
  (assert (= entry [(HySymbol ".foo_bang.bar.baz_bang")])))
(defn test_unmangle []
  (import [sys])
  (setv f (. (get sys.modules "hyhy.lex.parser") hy_symbol_unmangle))
  (assert (= (f "FOO") "*foo*"))
  (assert (= (f "<") "<"))
  (assert (= (f "FOOa") "FOOa"))
  (assert (= (f "foo_bar") "foo-bar"))
  (assert (= (f "_") "_"))
  (assert (= (f "is_foo") "foo?"))
  (assert (= (f "is_") "is-"))
  (assert (= (f "foo_bang") "foo!"))
  (assert (= (f "_bang") "-bang")))
(defn test_simple_cons []
  "Check that cons gets tokenized correctly"
  (setv entry (get (tokenize "(a . b)") 0))
  (assert (= entry (HyCons (HySymbol "a") (HySymbol "b")))))
(defn test_dotted_list []
  "Check that dotted lists get tokenized correctly"
  (setv entry (get (tokenize "(a b c . (d . e))") 0))
  (assert (= entry (HyCons (HySymbol "a") (HyCons (HySymbol "b") (HyCons (HySymbol "c") (HyCons (HySymbol "d") (HySymbol "e"))))))))
(defn test_cons_list []
  "Check that cons of something and a list gets tokenized as a list"
  (setv entry (get (tokenize "(a . [])") 0))
  (assert (= entry (HyList [(HySymbol "a")])))
  (assert (= (type entry) HyList))
  (setv entry (get (tokenize "(a . ())") 0))
  (assert (= entry (HyExpression [(HySymbol "a")])))
  (assert (= (type entry) HyExpression))
  (setv entry (get (tokenize "(a b . {})") 0))
  (assert (= entry (HyDict [(HySymbol "a") (HySymbol "b")])))
  (assert (= (type entry) HyDict)))
