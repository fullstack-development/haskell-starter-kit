# starterkit/project:latest
FROM starterkit/dependencies:latest

WORKDIR /haskell-starter-kit/
RUN stack build

EXPOSE 8080
CMD [ "stack", "exec", "haskell-starter-kit-exe" ]
