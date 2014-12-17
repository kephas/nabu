#!/bin/sh

exec sbcl --eval '(ql:quickload "nabu")' --eval "(in-package :nabu)" --eval "(clackup $1)"
