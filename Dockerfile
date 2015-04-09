FROM debian:testing
MAINTAINER Pierre Thierry <pierre@nothos.net>

RUN apt-get update && apt-get install -y build-essential
RUN apt-get install -y wget \
    sbcl \
    git \
    libpq-dev
RUN wget http://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(let ((ql-util::*do-not-prompt* t))(ql:add-to-init-file))'
RUN sbcl --eval '(ql-dist:install-dist "http://beta.quicklisp.org/dist/quicklisp/2015-01-13/distinfo.txt" :prompt nil :replace t)'

# Ironclad takes time and MUST be cached
RUN sbcl --eval '(ql:quickload "ironclad")'

# Pre-load common dependencies/tools for caching purposes
RUN git clone https://github.com/kephas/cl-scheme.git /root/quicklisp/local-projects/cl-scheme
RUN git clone https://github.com/kephas/cl-docker-tools.git /root/quicklisp/local-projects/cl-docker-tools
RUN sbcl --eval '(ql:register-local-projects)' \
         --eval '(ql:quickload (list "scheme" "cl-docker-tools" "alexandria" "split-sequence" "metabang-bind" "hu.dwim.stefil" \
                                     "caveman2" "cl-who" "drakma" "elephant" "clsql-postgresql" "clack-handler-hunchentoot"))'


# Pre-load remaining dependencies before COPY
RUN sbcl --eval '(ql:quickload (list "cl-match" "cl-base64" "uuid" "do-urlencode"))'

COPY ./ /root/quicklisp/local-projects/nabu/
RUN sbcl --eval '(ql:register-local-projects)' --eval '(ql:quickload "nabu")' # Pre-compile the project

CMD sbcl --eval '(ql:quickload (list "cl-docker-tools" "nabu"))' --eval "(in-package :nabu)" \
         --eval '(docker-tools:swank 4005)' --eval "(clackup 80)"
EXPOSE 4005
EXPOSE 80
