#!/bin/sh

docker build . -t oak_phenotype

docker-compose up
