FROM alpine:3.3
MAINTAINER Gregoire Lejeune <gregoire.lejeune@free.fr>

ENV OTP_VERSION 18.2.1
ENV ELIXIR_VERSION v1.2.1

RUN apk --update add git autoconf build-base perl-dev zlib-dev ncurses-dev openssl-dev unixodbc-dev wget grep

# Install Erlang
RUN curl -O https://raw.githubusercontent.com/yrashk/kerl/master/kerl
RUN chmod +x kerl 
ENV KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac --enable-shared-zlib --enable-dynamic-ssl-lib --enable-hipe --enable-smp-support --enable-threads --enable-kernel-poll"
RUN ./kerl update releases && ./kerl build ${OTP_VERSION} ${OTP_VERSION}-bu && ./kerl install ${OTP_VERSION}-bu ~/.kerl/installs/${OTP_VERSION}-bu

# Install Elixir
RUN curl -O https://raw.githubusercontent.com/glejeune/kex/master/kex
RUN chmod +x kex
RUN . ~/.kerl/installs/${OTP_VERSION}-bu/activate && ./kex update && ./kex install ${ELIXIR_VERSION}

# Create Entrypoint
RUN echo "#!/bin/sh" > /entrypoint.sh
RUN echo ". ~/.kerl/installs/${OTP_VERSION}-bu/activate" >> /entrypoint.sh
RUN echo ". ~/.kex/installs/${ELIXIR_VERSION}/activate" >> /entrypoint.sh
RUN echo "exec \"\$@\"" >> /entrypoint.sh
RUN chmod 755 /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]

