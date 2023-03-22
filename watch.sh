#!/bin/bash

docker compose run -d --rm --service-ports blog --host 0.0.0.0 --port 9999
echo "The blog is running on http://localhost:9999"
open 'http://localhost:9999'
