#!/bin/sh
emacs -batch -l ~/.emacs -l ./test-harness.el -l ./airwave-func.el -f batch-test-emacs ./airwave-func-tests.el
