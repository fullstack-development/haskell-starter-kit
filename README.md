# haskell-starter-kit

A simple backend server using PostgreSQL database.

## Configure project

To configure env for project:

1. Configure file at `./config/local.conf` using template at `./config/template.conf`:

`cp ./config/template.conf ./config/local.conf`

2. Configure docker env variables at `./.env` using template at `./.env.template`.

`cp ./.env.template ./.env`

## Run with docker

You need to have docker installed in your system. Then run:

```
make deps
make run
```

## Develop with stack

To build project:

`stack build`

To run database:

`docker-compose up -d db`

To connect to database:

`psql -U myuser -d appnamedb -p 5431 -h 127.0.0.1 -W`

To run server:

`stack exec haskell-starter-kit-exe`
