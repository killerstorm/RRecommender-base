#!/bin/bash

sbcl --load common.lisp --load $1.lisp --eval "(progn $2 (quit))"