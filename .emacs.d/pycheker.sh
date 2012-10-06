#!/bin/bash
~/.emacs.d/vendor/pyflakes "$1"
~/.emacs.d/vendor/pep8.py --repeat "$1"
true
