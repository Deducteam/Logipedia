
# Requirements

The website needs the system packages

- mongodb (>=v4.0.2)
- php-zip
- pecl

It also requires the php package mongo that you can install with the utilitary 'pecl': `pecl install mongodb`. Don't forget to have the `php-<version>-dev` package installed.

# Docker

**Dockerfile** creates a container image for the web application. 

## Build container image

To build the docker image, [docker](https://docs.docker.com/install/) should be installed.

Container image can be generated either using

```
docker build -t logipedia:web .
```

or 

```
./container_build.sh
```


## Run container

Container image should be generated before running.

Container can be run either using
```
docker run  -p 8000:8000 --network host logipedia:web
```

or

```
./container_run.sh
```

This will start the container and display the website on [localhost:8000](localhost:8000).

The container uses PHP built in web server which runs only one single-threaded process, and should not be used in production environment.

It assumes an existing MongoDB that contains **logipedia** database. 
