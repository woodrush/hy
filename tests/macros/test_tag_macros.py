# Copyright 2017 the authors.
# This file is part of Hy, which is free software licensed under the Expat
# license. See the LICENSE.

from hyhy.macros import macroexpand
from hyhy.compiler import HyTypeError, HyASTCompiler
from hyhy.lex import tokenize


def test_tag_macro_error():
    """Check if we get correct error with wrong dispatch character"""
    try:
        macroexpand(tokenize("(dispatch_tag_macro '- '())")[0],
                    HyASTCompiler(__name__))
    except HyTypeError as e:
        assert "with the character `-`" in str(e)
