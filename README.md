# haskell-starter-kit

A simple backend server using PostgreSQL database.

## Run with docker

You need to have docker installed in your system. Then run:

`make run`

## Develop with stack

To build project:

`stack build`

To run database:

`docker-compose up -d db`

To run server:

`stack exec haskell-starter-kit-exe`
