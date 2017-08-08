(import [hyhy._compat [long_type str_type]])
(defclass Py2HyReturnException [Exception]
  (defn __init__ [self retvalue]
    (setv self.retvalue retvalue)))
(import [hyhy.models [wrap_value replace_hy_obj HyString HyInteger HyList HyDict HySet HyExpression HyCons]])
(defn test_wrap_long_type []
  " Test conversion of integers."
  (setv wrapped (wrap_value (long_type 0)))
  (assert (= (type wrapped) HyInteger)))
(defn test_wrap_tuple []
  " Test conversion of tuples."
  (setv wrapped (wrap_value (, (HyInteger 0))))
  (assert (= (type wrapped) HyList))
  (assert (= (type (get wrapped 0)) HyInteger))
  (assert (= wrapped (HyList [(HyInteger 0)]))))
(defn test_wrap_nested_expr []
  " Test conversion of HyExpressions with embedded non-HyObjects."
  (setv wrapped (wrap_value (HyExpression [(long_type 0)])))
  (assert (= (type wrapped) HyExpression))
  (assert (= (type (get wrapped 0)) HyInteger))
  (assert (= wrapped (HyExpression [(HyInteger 0)]))))
(defn test_replace_long_type []
  " Test replacing integers."
  (setv replaced (replace_hy_obj (long_type 0) (HyInteger 13)))
  (assert (= replaced (HyInteger 0))))
(defn test_replace_string_type []
  "Test replacing python string"
  (setv replaced (replace_hy_obj (str_type "foo") (HyString "bar")))
  (assert (= replaced (HyString "foo"))))
(defn test_replace_tuple []
  " Test replacing tuples."
  (setv replaced (replace_hy_obj (, (long_type 0)) (HyInteger 13)))
  (assert (= (type replaced) HyList))
  (assert (= (type (get replaced 0)) HyInteger))
  (assert (= replaced (HyList [(HyInteger 0)]))))
(defn test_list_add []
  "Check that adding two HyLists generates a HyList"
  (setv a (HyList [1 2 3]))
  (setv b (HyList [3 4 5]))
  (setv c (+ a b))
  (assert (= c [1 2 3 3 4 5]))
  (assert (= c.__class__ HyList)))
(defn test_list_slice []
  "Check that slicing a HyList produces a HyList"
  (setv a (HyList [1 2 3 4]))
  (setv sl1 (get a (slice 1 None None)))
  (setv sl5 (get a (slice 5 None None)))
  (assert (= (type sl1) HyList))
  (assert (= sl1 (HyList [2 3 4])))
  (assert (= (type sl5) HyList))
  (assert (= sl5 (HyList []))))
(setv hydict (HyDict ["a" 1 "b" 2 "c" 3]))
(defn test_dict_items []
  (assert (= (hydict.items) [(, "a" 1) (, "b" 2) (, "c" 3)])))
(defn test_dict_keys []
  (assert (= (hydict.keys) ["a" "b" "c"])))
(defn test_dict_values []
  (assert (= (hydict.values) [1 2 3])))
(setv hyset (HySet [3 1 2 2]))
(defn test_set []
  (assert (= hyset [3 1 2 2])))
(defn test_cons_slicing []
  "Check that cons slicing works as expected"
  (try
    (do
      (setv cons (HyCons "car" "cdr"))
      (assert (= (get cons 0) "car"))
      (assert (= (get cons (slice 1 None None)) "cdr"))
      (try
        (do
          (get cons (slice None None None))
          (assert (is True False)))
        (except [e Py2HyReturnException]
          (raise e))
        (except [IndexError]
          (do)))
      (try
        (do
          (get cons 1)
          (assert (is True False)))
        (except [e Py2HyReturnException]
          (raise e))
        (except [IndexError]
          (do))))
    (except [e Py2HyReturnException]
      e.retvalue)))
(defn test_cons_replacing []
  "Check that assigning to a cons works as expected"
  (try
    (do
      (setv cons (HyCons "foo" "bar"))
      (assoc cons 0 "car")
      (assert (= cons (HyCons "car" "bar")))
      (assoc cons (slice 1 None None) "cdr")
      (assert (= cons (HyCons "car" "cdr")))
      (try
        (do
          (assoc cons (slice None None None) "foo")
          (assert (is True False)))
        (except [e Py2HyReturnException]
          (raise e))
        (except [IndexError]
          (do))))
    (except [e Py2HyReturnException]
      e.retvalue)))
