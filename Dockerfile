FROM ubuntu:bionic

RUN apt-get update && apt-get install -y curl
RUN curl -sSL https://get.haskellstack.org/ | sh

RUN apt-get install -y libghc-postgresql-libpq-dev

COPY ./ haskell-starter-kit/

WORKDIR /haskell-starter-kit/
RUN stack build

CMD [ "stack", "exec", "haskell-starter-kit-exe" ]
