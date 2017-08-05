(import [math [isnan isinf]])
(defclass Py2HyReturnException [Exception] (defn __init__ [self retvalue] (setv self.retvalue retvalue)))
(import [hyhy._compat [PY3 str_type bytes_type long_type string_types]])
(import [fractions [Fraction]])
(defclass HyObject [object] "
    Generic Hy Object model. This is helpful to inject things into all the
    Hy lexing Objects at once.
    " (defn replace [self other] "Using a hacky implementation of `return`" (try (do (if (isinstance other HyObject) (do (for [attr ["start_line" "end_line" "start_column" "end_column"]] (when (and (not (hasattr self attr)) (hasattr other attr)) (do (setattr self attr (getattr other attr)))))) (do (raise (TypeError "Can't replace a non Hy object with a Hy object")))) (raise (Py2HyReturnException self))) (except [e Py2HyReturnException] e.retvalue))))
(setv _wrappers {})
(defn wrap_value [x] "Using a hacky implementation of `return`" (try (do "Wrap `x` into the corresponding Hy type.

    This allows replace_hy_obj to convert a non Hy object to a Hy object.

    This also allows a macro to return an unquoted expression transparently.

    " (setv wrapper ((. _wrappers get) (type x))) (if (is wrapper None) (do (raise (Py2HyReturnException x))) (do (raise (Py2HyReturnException (wrapper x)))))) (except [e Py2HyReturnException] e.retvalue)))
(defn replace_hy_obj [obj other] "Using a hacky implementation of `return`" (try (do (when (isinstance obj HyObject) (do (raise (Py2HyReturnException ((. obj replace) other))))) (setv wrapped_obj (wrap_value obj)) (if (isinstance wrapped_obj HyObject) (do (raise (Py2HyReturnException ((. wrapped_obj replace) other)))) (do (raise (TypeError (% "Don't know how to wrap a %s object to a HyObject" (type obj))))))) (except [e Py2HyReturnException] e.retvalue)))
(defclass HyString [HyObject str_type] "
    Generic Hy String object. Helpful to store string literals from Hy
    scripts. It's either a ``str`` or a ``unicode``, depending on the
    Python version.
    " None)
(setv (get _wrappers str_type) HyString)
(defclass HyBytes [HyObject bytes_type] "
    Generic Hy Bytes object. It's either a ``bytes`` or a ``str``, depending
    on the Python version.
    " None)
(setv (get _wrappers bytes_type) HyBytes)
(defclass HySymbol [HyString] "
    Hy Symbol. Basically a String.
    " (defn __init__ [self string] (setv self (+ self string))))
(setv (get _wrappers bool) (fn [x] (if x (HySymbol "True") (HySymbol "False"))))
(setv (get _wrappers (type None)) (fn [foo] (HySymbol "None")))
(defclass HyKeyword [HyObject str_type] "Generic Hy Keyword object. It's either a ``str`` or a ``unicode``,
    depending on the Python version.
    " (setv PREFIX "﷐") (defn __new__ [cls value] "Using a hacky implementation of `return`" (try (do (when (not ((. value startswith) (. cls PREFIX))) (do (setv value (+ (. cls PREFIX) value)))) (setv obj ((. str_type __new__) cls value)) (raise (Py2HyReturnException obj))) (except [e Py2HyReturnException] e.retvalue))))
(defn strip_digit_separators [number] "Using a hacky implementation of `return`" (try (do (raise (Py2HyReturnException (if (isinstance number string_types) ((. ((. number replace) "_" "") replace) "," "") number)))) (except [e Py2HyReturnException] e.retvalue)))
(defclass HyInteger [HyObject long_type] "
    Internal representation of a Hy Integer. May raise a ValueError as if
    int(foo) was called, given HyInteger(foo). On python 2.x long will
    be used instead
    " (defn __new__ [cls number &kwargs kwargs &rest args] "Using a hacky implementation of `return`" (try (do (if (isinstance number string_types) (do (setv number (strip_digit_separators number)) (setv bases {"0x" 16 "0o" 8 "0b" 2}) (for [(, leader base) ((. bases items))] (when ((. number startswith) leader) (do (setv number (long_type number :base base)) (break))))) (do (setv number (long_type number)))) (raise (Py2HyReturnException ((. (super HyInteger cls) __new__) cls number)))) (except [e Py2HyReturnException] e.retvalue))))
(setv (get _wrappers int) HyInteger)
(when (not PY3) (do (setv (get _wrappers long_type) HyInteger)))
(defn check_inf_nan_cap [arg value] (when (isinstance arg string_types) (do (when (and (isinf value) (not_in "Inf" arg)) (do (raise (ValueError "Inf must be capitalized as \"Inf\"")))) (when (and (isnan value) (not_in "NaN" arg)) (do (raise (ValueError "NaN must be capitalized as \"NaN\"")))))))
(defclass HyFloat [HyObject float] "
    Internal representation of a Hy Float. May raise a ValueError as if
    float(foo) was called, given HyFloat(foo).
    " (defn __new__ [cls num &kwargs kwargs &rest args] "Using a hacky implementation of `return`" (try (do (setv value ((. (super HyFloat cls) __new__) cls (strip_digit_separators num))) (check_inf_nan_cap num value) (raise (Py2HyReturnException value))) (except [e Py2HyReturnException] e.retvalue))))
(setv (get _wrappers float) HyFloat)
(defclass HyComplex [HyObject complex] "
    Internal representation of a Hy Complex. May raise a ValueError as if
    complex(foo) was called, given HyComplex(foo).
    " (defn __new__ [cls num &kwargs kwargs &rest args] "Using a hacky implementation of `return`" (try (do (setv value ((. (super HyComplex cls) __new__) cls (strip_digit_separators num))) (when (isinstance num string_types) (do (setv (, p1 _ p2) ((. ((. ((. num lstrip) "+-") replace) "-" "+") partition) "+")) (if p2 (do (check_inf_nan_cap p1 (. value real)) (check_inf_nan_cap p2 (. value imag))) (do (if (in "j" p1) (do (check_inf_nan_cap p1 (. value imag))) (do (check_inf_nan_cap p1 (. value real)))))))) (raise (Py2HyReturnException value))) (except [e Py2HyReturnException] e.retvalue))))
(setv (get _wrappers complex) HyComplex)
(defclass HyList [HyObject list] "
    Hy List. Basically just a list.
    " (defn replace [self other] "Using a hacky implementation of `return`" (try (do (for [x self] (replace_hy_obj x other)) ((. HyObject replace) self other) (raise (Py2HyReturnException self))) (except [e Py2HyReturnException] e.retvalue))) (defn __add__ [self other] "Using a hacky implementation of `return`" (try (do (raise (Py2HyReturnException ((. self __class__) ((. (super HyList self) __add__) other))))) (except [e Py2HyReturnException] e.retvalue))) (defn __getslice__ [self start end] "Using a hacky implementation of `return`" (try (do (raise (Py2HyReturnException ((. self __class__) ((. (super HyList self) __getslice__) start end))))) (except [e Py2HyReturnException] e.retvalue))) (defn __getitem__ [self item] "Using a hacky implementation of `return`" (try (do (setv ret ((. (super HyList self) __getitem__) item)) (when (isinstance item slice) (do (raise (Py2HyReturnException ((. self __class__) ret))))) (raise (Py2HyReturnException ret))) (except [e Py2HyReturnException] e.retvalue))) (defn __repr__ [self] "Using a hacky implementation of `return`" (try (do (raise (Py2HyReturnException (% "[%s]" ((. " " join) (list_comp (repr x) [x self])))))) (except [e Py2HyReturnException] e.retvalue))))
(setv (get _wrappers list) (fn [l] (HyList (genexpr (wrap_value x) [x l]))))
(setv (get _wrappers tuple) (fn [t] (HyList (genexpr (wrap_value x) [x t]))))
(defclass HyDict [HyList] "
    HyDict (just a representation of a dict)
    " (defn __repr__ [self] "Using a hacky implementation of `return`" (try (do (raise (Py2HyReturnException (% "{%s}" ((. " " join) (list_comp (repr x) [x self])))))) (except [e Py2HyReturnException] e.retvalue))) (defn keys [self] "Using a hacky implementation of `return`" (try (do (raise (Py2HyReturnException (get self (slice 0 None 2))))) (except [e Py2HyReturnException] e.retvalue))) (defn values [self] "Using a hacky implementation of `return`" (try (do (raise (Py2HyReturnException (get self (slice 1 None 2))))) (except [e Py2HyReturnException] e.retvalue))) (defn items [self] "Using a hacky implementation of `return`" (try (do (raise (Py2HyReturnException (list (zip ((. self keys)) ((. self values))))))) (except [e Py2HyReturnException] e.retvalue))))
(setv (get _wrappers dict) (fn [d] (HyDict (genexpr (wrap_value x) [x (sum ((. d items)) (,))]))))
(defclass HyExpression [HyList] "
    Hy S-Expression. Basically just a list.
    " (defn __repr__ [self] "Using a hacky implementation of `return`" (try (do (raise (Py2HyReturnException (% "(%s)" ((. " " join) (list_comp (repr x) [x self])))))) (except [e Py2HyReturnException] e.retvalue))))
(setv (get _wrappers HyExpression) (fn [e] (HyExpression (genexpr (wrap_value x) [x e]))))
(setv (get _wrappers Fraction) (fn [e] (HyExpression [(HySymbol "fraction") (wrap_value (. e numerator)) (wrap_value (. e denominator))])))
(defclass HySet [HyList] "
    Hy set (just a representation of a set)
    " (defn __repr__ [self] "Using a hacky implementation of `return`" (try (do (raise (Py2HyReturnException (% "#{%s}" ((. " " join) (list_comp (repr x) [x self])))))) (except [e Py2HyReturnException] e.retvalue))))
(setv (get _wrappers set) (fn [s] (HySet (genexpr (wrap_value x) [x s]))))
(defclass HyCons [HyObject] "
    HyCons: a cons object.

    Building a HyCons of something and a HyList really builds a HyList
    " (setv __slots__ ["car" "cdr"]) (defn __new__ [cls car cdr] "Using a hacky implementation of `return`" (try (do (if (isinstance cdr list) (do (when (= (type cdr) HyExpression) (do (when (and (> (len cdr) 0) (= (type (get cdr 0)) HySymbol)) (do (when (in (get cdr 0) (, "unquote" "unquote_splice")) (do (raise (Py2HyReturnException ((. (super HyCons cls) __new__) cls))))))))) (raise (Py2HyReturnException ((. cdr __class__) (+ [(wrap_value car)] cdr))))) (do (if (is cdr None) (do (raise (Py2HyReturnException (HyExpression [(wrap_value car)])))) (do (raise (Py2HyReturnException ((. (super HyCons cls) __new__) cls)))))))) (except [e Py2HyReturnException] e.retvalue))) (defn __init__ [self car cdr] (setv (. self car) (wrap_value car)) (setv (. self cdr) (wrap_value cdr))) (defn __getitem__ [self n] "Using a hacky implementation of `return`" (try (do (when (= n 0) (do (raise (Py2HyReturnException (. self car))))) (when (= n (slice 1 None)) (do (raise (Py2HyReturnException (. self cdr))))) (raise (IndexError "Can only get the car ([0]) or the cdr ([1:]) of a HyCons"))) (except [e Py2HyReturnException] e.retvalue))) (defn __setitem__ [self n new] "Using a hacky implementation of `return`" (try (do (when (= n 0) (do (setv (. self car) new) (raise (Py2HyReturnException None)))) (when (= n (slice 1 None)) (do (setv (. self cdr) new) (raise (Py2HyReturnException None)))) (raise (IndexError "Can only set the car ([0]) or the cdr ([1:]) of a HyCons"))) (except [e Py2HyReturnException] e.retvalue))) (defn __iter__ [self] (yield (. self car)) (try (do (setv iterator (genexpr i [i (. self cdr)]))) (except [TypeError] (when (is_not (. self cdr) None) (do (yield (. self cdr)) (raise (TypeError "Iteration on malformed cons"))))) (else (for [i iterator] (yield i))))) (defn replace [self other] (when (is_not (. self car) None) (do (replace_hy_obj (. self car) other))) (when (is_not (. self cdr) None) (do (replace_hy_obj (. self cdr) other))) ((. HyObject replace) self other)) (defn __repr__ [self] "Using a hacky implementation of `return`" (try (do (if (isinstance (. self cdr) (. self __class__)) (do (raise (Py2HyReturnException (% "(%s %s)" (, (repr (. self car)) (get (repr (. self cdr)) (slice 1 (- 1) None))))))) (do (raise (Py2HyReturnException (% "(%s . %s)" (, (repr (. self car)) (repr (. self cdr))))))))) (except [e Py2HyReturnException] e.retvalue))) (defn __eq__ [self other] "Using a hacky implementation of `return`" (try (do (raise (Py2HyReturnException (and (isinstance other (. self __class__)) (= (. self car) (. other car)) (= (. self cdr) (. other cdr)))))) (except [e Py2HyReturnException] e.retvalue))))
