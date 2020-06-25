#!/bin/sh

docker build . -t oak_pdi

docker-compose up
