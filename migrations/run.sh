#!/bin/sh
docker run \
    --network="host" \
    -v `pwd`/migrations:/workspace/ \
    kilna/liquibase-postgres \
    liquibase update
