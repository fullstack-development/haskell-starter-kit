#!/bin/bash
docker run \
    --network="host" \
    -v `pwd`/migrations:/workspace/ \
    kilna/liquibase-postgres \
    liquibase update
