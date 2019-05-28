FROM ubuntu:18.04 AS logipedia_base

LABEL maintainer="hande.gozukan@inria.fr"
LABEL maintainer="romain.primet@inria.fr"

ARG OCAML_VERSION=4.05.0
ARG OPAM_VERSION=2.0.4
ARG APP_NAME=logipedia

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
        # --disable-sandboxing -> to solve "bwrap: Creating new namespace failed: Operation not permitted" error in container, otherwise container needs to be started in priviledged mode https://github.com/ocaml/opam/issues/3498
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

RUN eval $(opam env) \
	&& service mongodb start \
	&& make \
	&& make web \
	&& mongodump -o ./website/web/theorems/download/mongodump 

#########################################################################
FROM ubuntu:18.04

LABEL maintainer="hande.gozukan@inria.fr"
LABEL maintainer="romain.primet@inria.fr"

ARG PHP_VERSION=7.2
ARG APP_NAME=logipedia

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update \
	&& apt-get install -y composer \
	mongodb \
	php${PHP_VERSION}-zip \
	php${PHP_VERSION}-dev \
#	php-mongodb \ # this does not work because driver version is 1.3.4 and does not satisfy dependency when running composer
	php-pear \
	&& pecl install mongodb \
        # below line copied from https://packagist.org/packages/mongodb/mongodb
	&& echo "extension=mongodb.so" >> `php --ini | grep "Loaded Configuration" | sed -e "s|.*:\s*||"` \
	&& rm -rf /var/lib/apt/lists/* \
	&& rm -rf /tmp/*


WORKDIR /$APP_NAME

COPY --from=logipedia_base /$APP_NAME/website/ ./

RUN  composer update \
	&& service mongodb start \
	&& mongorestore --dir  ./web/theorems/download/mongodump/

WORKDIR /$APP_NAME/web

EXPOSE 8000

CMD ["sh", "-c", "service mongodb start && php -S 0.0.0.0:8000"]
