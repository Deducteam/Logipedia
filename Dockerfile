FROM ubuntu:18.04

LABEL maintainer="hande.gozukan@inria.fr"
LABEL maintainer="romain.primet@inria.fr"

ARG OCAML_VERSION=4.05.0
ARG APP_NAME=logipedia_lib
ARG USER_NAME=logipedia

# opam installation -> https://opam.ocaml.org/doc/Install.html
RUN apt update \
	# necessary to fix "add-apt-repository: command not found”
	&& apt install -y software-properties-common \
	&& add-apt-repository ppa:avsm/ppa \
	&& apt update \
	&& apt install -y gcc \
	make \
	m4 \
	mongodb \
        time \
	opam \
	# switch to OCAML_VERSION for ocaml compiler
	# -a -> Automatically do a full setup, including adding a line to your shell init files
        # --disable-sandboxing -> to solve "bwrap: Creating new namespace failed: Operation not permitted" error in container
	&& opam init -a --compiler=$OCAML_VERSION --disable-sandboxing \
        # install dedukti dependencies
	&& opam install -y ocamlfind \
	ocamlbuild \
	menhir \
	# install proof generator dependencies
	dedukti \
	mongo \
# todo : install type checker for each relevant system
	&& rm -rf /var/lib/apt/lists/* \
	&& rm -rf /tmp/*

WORKDIR /$APP_NAME

COPY ./ ./

CMD ["sh", "-c", "eval $(opam env) && make && make web"]
