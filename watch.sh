#!/bin/bash

docker compose run --rm --service-ports blog --host 0.0.0.0 --port 9999
