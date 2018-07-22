.PHONY: run test docker-build docker-run

run:
	sbt run

test:
	sbt test

docker-build:
	docker build -t plippe/suprnation .

docker-run:
	docker run --rm -i plippe/suprnation
