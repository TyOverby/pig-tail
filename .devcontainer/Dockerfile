FROM ocaml/opam:alpine-ocaml-4.12

USER root
RUN sed -i 's-/home/opam:/sbin/nologin-/home/opam:/bin/bash-' /etc/passwd
RUN apk add git inotify-tools openssh openssl-dev libffi-dev zlib-dev pcre neovim gmp-dev pcre-dev postgresql-dev postgresql

USER opam
RUN opam update
RUN opam install dune core async base merlin ocamlformat ppx_jane ocaml-lsp-server postgresql

RUN mkdir -p ~/.cache/nvim