#!/usr/bin/env python
# Copyright 2017 the authors.
# This file is part of Hy, which is free software licensed under the Expat
# license. See the LICENSE.

import sys, os

from setuptools import find_packages, setup
from setuptools.command.install import install

from get_version import __version__

os.chdir(os.path.split(os.path.abspath(__file__))[0])

PKG = "hyhy"

long_description = """HyHy is a Python <--> Lisp layer. It helps
make things work nicer, and lets Python and the Hy lisp variant play
nice together. """

class Install(install):
    def run(self):
        # Import each Hy module to ensure it's compiled.
        import os, importlib
        for dirpath, _, filenames in sorted(os.walk("hyhy")):
            for filename in sorted(filenames):
                if filename.endswith(".hyhy"):
                    importlib.import_module(
                        dirpath.replace("/", ".").replace("\\", ".") +
                        "." + filename[:-len(".hyhy")])
        install.run(self)

install_requires = ['rply>=0.7.5', 'astor>=0.5', 'clint>=0.4']
if os.name == 'nt':
    install_requires.append('pyreadline>=2.1')

ver = sys.version_info[0]

setup(
    name=PKG,
    version=__version__,
    install_requires=install_requires,
    cmdclass=dict(install=Install),
    entry_points={
        'console_scripts': [
            'hyhy = hyhy.cmdline:hy_main',
            'hyhy%d = hyhy.cmdline:hy_main' % ver,
            'hyhyc = hyhy.cmdline:hyc_main',
            'hyhyc%d = hyhy.cmdline:hyc_main' % ver,
            'hyhy2py = hyhy.cmdline:hy2py_main',
            'hyhy2py%d = hyhy.cmdline:hy2py_main' % ver,
        ]
    },
    packages=find_packages(exclude=['tests*']),
    package_data={
        'hyhy.contrib': ['*.hyhy', '__pycache__/*'],
        'hyhy.core': ['*.hyhy', '__pycache__/*'],
        'hyhy.extra': ['*.hyhy', '__pycache__/*'],
    },
    data_files=[
        ('get_version', ['get_version.py'])
    ],
    author="Paul Tagliamonte",
    author_email="tag@pault.ag",
    long_description=long_description,
    description='Lisp and Python love each other.',
    license="Expat",
    url="http://hylang.org/",
    platforms=['any'],
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Developers",
        "License :: DFSG approved",
        "License :: OSI Approved :: MIT License",  # Really "Expat". Ugh.
        "Operating System :: OS Independent",
        "Programming Language :: Lisp",
        "Programming Language :: Python",
        "Programming Language :: Python :: 2",
        "Programming Language :: Python :: 2.7",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.3",
        "Programming Language :: Python :: 3.4",
        "Programming Language :: Python :: 3.5",
        "Programming Language :: Python :: 3.6",
        "Topic :: Software Development :: Code Generators",
        "Topic :: Software Development :: Compilers",
        "Topic :: Software Development :: Libraries",
    ]
)
