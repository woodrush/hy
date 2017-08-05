__appname__ = 'hyhy'
try:
    from hyhy.version import __version__
except ImportError:
    __version__ = 'unknown'


import hy
from hyhy.models import HyExpression, HyInteger, HyKeyword, HyComplex, HyString, HyBytes, HySymbol, HyFloat, HyDict, HyList, HySet, HyCons  # NOQA


import hyhy.importer  # NOQA
# we import for side-effects.
