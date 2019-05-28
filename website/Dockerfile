FROM ubuntu:18.04

LABEL maintainer="hande.gozukan@inria.fr"
LABEL maintainer="romain.primet@inria.fr"

ARG PHP_VERSION=7.2
ARG APP_NAME=logipedia_web

WORKDIR /$APP_NAME

COPY ./ ./

RUN apt update \
        # DEBIAN_FRONTEND=noninteractive -> to avoid interactive installation for tzdata
	&& DEBIAN_FRONTEND=noninteractive apt install -y php${PHP_VERSION}-zip \
	php${PHP_VERSION}-dev \
#	php-mongodb \ # this does not work because driver version is 1.3.4 and does not satisfy dependency when running composer
	php-pear \
	composer \
	&& pecl install mongodb \
        # below line copied from https://packagist.org/packages/mongodb/mongodb
	&& echo "extension=mongodb.so" >> `php --ini | grep "Loaded Configuration" | sed -e "s|.*:\s*||"` \
	&& rm -rf /var/lib/apt/lists/* \
	&& rm -rf /tmp/* \
	&& composer update


WORKDIR /$APP_NAME/web

EXPOSE 8000

CMD ["php", "-S", "0.0.0.0:8000"]
