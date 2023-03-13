#!/bin/bash

docker compose run --rm --entrypoint 'bash -c "site clean && site rebuild && site deploy"' blog
