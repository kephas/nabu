#!/bin/sh

exec sbcl --eval '(ql:quickload "nabu")' --eval "(nabu::web-start $1)"
