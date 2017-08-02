# Copyright 2017 the authors.
# This file is part of Hy, which is free software licensed under the Expat
# license. See the LICENSE.

from rply.errors import LexingError

from hyhy.lex.exceptions import LexException, PrematureEndOfInput  # NOQA
from hyhy.lex.lexer import lexer
from hyhy.lex.parser import parser


def tokenize(buf):
    """
    Tokenize a Lisp file or string buffer into internal Hy objects.
    """
    try:
        return parser.parse(lexer.lex(buf))
    except LexingError as e:
        pos = e.getsourcepos()
        raise LexException("Could not identify the next token.",
                           pos.lineno, pos.colno, buf)
    except LexException as e:
        if e.source is None:
            e.source = buf
        raise
