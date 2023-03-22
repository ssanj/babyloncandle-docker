---
title: Change Cockpit Launch Port on Ubuntu Server
author: sanjiv sahayam
description: How to change Cockpit's port from the default of 9090 on Ubuntu Server
tags: ubuntu, cockpit
comments: true
---

[Cockpit](https://cockpit-project.org/) runs on the popular port 9090. Chances are that you are going to have a port conflict if you have a couple of other services running. This will lead to Cockpit failing to start.

If you suspect this is the case you can verify it by running:

```{.terminal .scrollx}
sudo systemctl status cockpit.socket
```

or by tailing the syslog:

```{.terminal .scrollx}
tail /var/log/syslog
```

If you do see any errors about Cockpit not starting due to a port conflict, it's quite easy to rectify. These instructions are for Ubuntu Server Jammy (22.04.2).

1. Create or edit the `/etc/systemd/system/cockpit.socket.d/listen.conf` file
2. Change the `ListenStream` option to match your required port

The default option will be similar to:

```{.config .scrollx}
[Socket]
ListenStream=
ListenStream=443
```

Change it to match your required port:

```{.config .scrollx}
[Socket]
ListenStream=
ListenStream=<YOUR_NEW_PORT>
```

Note: the empty `ListenStream=` directive is needed to reset the previous value of this field. You can find more information about this on the [Cockpit documentation](https://cockpit-project.org/guide/latest/listen)


3. Finally, restart the services to make the change take effect:

```{.terminal .scrollx}
sudo systemctl daemon-reload
sudo systemctl restart cockpit.socket
```
