#!/bin/bash

POST_NAME=$(echo $@ | tr ' ' '-' | tr '[:upper:]' '[:lower:]')
CURRENT_DATE=$(date '+%Y-%m-%d')
touch "data/posts/$CURRENT_DATE-$POST_NAME.md"
