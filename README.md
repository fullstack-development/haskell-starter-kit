# haskell-starter-kit

A simple backend server using PostgreSQL database.

## Run with docker

You need to have docker installed in your system. Then run:

```
make deps
make run
```

## Develop with stack

To build project:

`stack build`

To configure env for project:

Run `source local.env` or create file with desired variables at `./config/local.conf`.

To run database:

`docker-compose up -d db`

To connect to database:

`psql -U myuser -d appnamedb -p 5431 -h 127.0.0.1 -W`

To run server:

`stack exec haskell-starter-kit-exe`
