FROM ubuntu:18.04

LABEL maintainer="hande.gozukan@inria.fr"
LABEL maintainer="romain.primet@inria.fr"

ARG OCAML_VERSION=4.05.0
ARG OPAM_VERSION="2.0.4"
ARG APP_NAME=logipedia_lib
ARG USER_NAME=logipedia

# opam installation -> https://opam.ocaml.org/doc/Install.html
RUN apt-get update \
	&& apt-get install -y gcc \
        git \
	make \
	m4 \
	mongodb \
        time \
        unzip \
        wget \
        zip \
        && wget https://github.com/ocaml/opam/releases/download/$OPAM_VERSION/opam-$OPAM_VERSION-x86_64-linux \
        && cp opam-$OPAM_VERSION-x86_64-linux /usr/local/bin/opam \
        && chmod a+x /usr/local/bin/opam \
	# switch to OCAML_VERSION for ocaml compiler
	# -a -> Automatically do a full setup, including adding a line to your shell init files
        # --disable-sandboxing -> to solve "bwrap: Creating new namespace failed: Operation not permitted" error in container
	&& opam init -a --compiler=$OCAML_VERSION --disable-sandboxing 
RUN eval $(opam env) \
        # dedukti 0.26 is too old to build Logipedia; pin to github repo
        # until 0.27 is released
        && opam pin add dedukti git+https://github.com/Deducteam/Dedukti.git -qy \
	&& opam install mongo -qy \
# todo : install type checker for each relevant system
	&& rm -rf /var/lib/apt/lists/* \
	&& rm -rf /tmp/*

WORKDIR /$APP_NAME

COPY ./ ./

CMD ["sh", "-c", "eval $(opam env) && service mongodb start && make && make web"]
