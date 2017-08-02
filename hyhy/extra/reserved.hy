;;; Get a frozenset of Hy reserved words
;; Copyright 2017 the authors.
;; This file is part of Hy, which is free software licensed under the Expat
;; license. See the LICENSE.

(import hyhy sys keyword)

(setv _cache None)

(defn names []
  "Return a frozenset of reserved symbol names.

  The result of the first call is cached."
  (global _cache)
  (if (is _cache None) (do
    (setv unmangle (. sys.modules ["hyhy.lex.parser"] hy_symbol_unmangle))
    (setv _cache (frozenset (map unmangle (+
      hyhy.core.language.*exports*
      hyhy.core.shadow.*exports*
      (list (.keys (get hyhy.macros._hy_macros None)))
      keyword.kwlist
      (list-comp k [k (.keys hyhy.compiler.-compile-table)]
        (isinstance k hyhy._compat.string-types))))))))
  _cache)
