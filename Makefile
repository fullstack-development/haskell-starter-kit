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
	docker volume rm haskell-starter-kit_pg-data

style:
	@cd "$$(git rev-parse --show-toplevel)" && ./scripts/format-code
