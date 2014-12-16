#!/bin/sh

exec sbcl --eval '(ql:quickload "nabu")' --eval "(nabu::clackup $1)"
