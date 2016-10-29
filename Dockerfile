FROM ubuntu:16.04

ARG exe=~/.local/bin/hauth-exe

RUN apt-get update -qq \
    && apt-get install -y libpq-dev \
    && rm -rf /var/lib/apt/lists/*

ADD snaplets /snaplets
ADD static /static
ADD $exe /usr/local/bin/

ENV HAUTH_ENV=docker

EXPOSE 8080

CMD hauth-exe -b 0.0.0.0 -p 8080 -e $HAUTH_ENV --access-log=/dev/stderr --error-log=/dev/stderr
