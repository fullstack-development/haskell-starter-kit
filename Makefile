default: run

image:
	docker-compose build

pg:
	docker-compose up -d db

start:
	docker-compose up

run: image pg start

stop:
	docker-compose down
	