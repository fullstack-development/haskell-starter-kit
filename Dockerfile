# starterkit/project:latest
FROM starterkit/dependencies:latest

# Clean up files to avoid problems with files deleted in the project,
# but remaining in the image, that would confuse 'stack build'.
RUN rm -rf /haskell-starter-kit
COPY ./ /haskell-starter-kit/

WORKDIR /haskell-starter-kit/
RUN stack build

EXPOSE 8080
CMD [ "stack", "exec", "haskell-starter-kit-exe" ]
