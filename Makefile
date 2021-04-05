default: run

deps:
	docker build -f Dockerfile.deps -t starterkit/dependencies:latest .

image:
	docker-compose build

pg:
	docker-compose up -d db

start:
	docker-compose up backend

run: image pg start

stop:
	docker-compose down

prune: stop
	docker volume rm haskellstarterkit_pg-data

ORMOLU_VERSION = 0.1.4.1
ORMOLU_OPTIONS = --mode inplace --check-idempotence

style:
	@ROOT=$$(git rev-parse --show-toplevel) && \
		cd "$$ROOT" && \
		FMT=$$(stack exec --package ormolu-'$(ORMOLU_VERSION)' which ormolu) && \
		find src app test -type f -name \*.hs -exec \
			"$$FMT" $(ORMOLU_OPTIONS) '{}' +
