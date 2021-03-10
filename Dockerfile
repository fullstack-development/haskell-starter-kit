# starterkit/project:latest
FROM starterkit/dependencies:latest

COPY ./ new/
RUN cp -rf new/* /haskell-starter-kit/ && rm -rf new/

WORKDIR /haskell-starter-kit/
RUN stack build

EXPOSE 8080
CMD [ "stack", "exec", "haskell-starter-kit-exe" ]
