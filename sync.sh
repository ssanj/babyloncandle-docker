#!/bin/bash

docker compose run --rm --entrypoint 'bash -c "site clean && site rebuild"' blog

echo "Copying dist files into $BLOG_WEBSITE_DIR"

rsync -av \
      --checksum \
      --delete \
      --progress \
      data/dist/_site/* $BLOG_WEBSITE_DIR
