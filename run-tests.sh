#!/bin/sh
exec ros run \
     --eval "(ql:quickload '(:rove :scsd/tests) :silent t)" \
     --eval "(rove:run :scsd/tests)"