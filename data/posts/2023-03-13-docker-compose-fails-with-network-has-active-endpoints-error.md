---
title: Docker Compose Fails with Network Has Active Endpoints Error
author: sanjiv sahayam
description: How to stop a docker compose stack when it fails with 'Network ID Has Active Endpoints'
tags: docker, docker-compose
comments: true
---

While running `docker compose down` on a docker compose stack, I came across this error:

> network <ID> has active endpoints

Now if I listed the docker networks with:


```{.terminal .scrollx}
docker network ls
```

I could see the offending network listed:

```{.terminal .scrollx}
NETWORK ID     NAME                           DRIVER    SCOPE
2222a222222a   <OFFENDING NETWORK NAME>       bridge    local
```

Now I could probably fix this by removing the offending network with:

```{.terminal .scrollx}
docker network rm <OFFENDING NETWORK NAME>
```

A quick [Google](https://stackoverflow.com/questions/42842277/docker-compose-down-default-network-error) pointed me in the right direction to get things working as expected. The network I was trying to remove was being used by an orphaned container.

Running the following fixed the issue:

```{.terminal .scrollx}
docker compose down --remove-orphans
```

This can occur if you run `docker run` or equivalent without the `--rm` parameter, where container is not removed after the command exits.

