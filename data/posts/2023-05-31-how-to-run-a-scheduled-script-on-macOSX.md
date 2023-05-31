---
title: How to run a Scheduled Script on macOSX
author: sanjiv sahayam
description: How to setup a scheduled script on macOSX without Cron
tags: macosx
comments: true
---

At the risk of stating the obvious: macOS has its own way of doing things. When I wanted to run a scheduled script using
Cron, the standard response seemed to be that [Crontab is deprecated](https://apple.stackexchange.com/questions/12819/why-is-cron-being-deprecated) and to use `launchd` instead.

![sigh](https://media.giphy.com/media/I2a5q9dyo9CaU9BtEY/giphy.gif)


There are two main types of `Job`s that can be run through `launchd`:

- `Agent`s - Which run as the currently logged in user
- `Daemon`s - Which run as any user specified (such as root)


Jobs are defined in terms of `plist` files. `plist` files are XML files which can be registered with the `launchd` system.
There's an excellent guide at [launchd.info](https://www.launchd.info/) on how to setup all things `launchd` if you want to know more.

Below are the steps required to setup a simple `launchd` `Agent` to run a script at a given time each day.


## Define the Job

`Agent` `Job` definitions are stored at `~/Library/LaunchAgents`. The file name of `Job` definition file is of the form:

`<NAMESPACE>.<SCRIPT>.plist`

The `NAMESPACE` can be something unique to your computer such as a reversed domain name.
`SCRIPT` is the name of your script.


**Note**: *This is the format I use. Feel free to change this up as you like.*

For example to create a `backup` script for a computer at `machinex.ssanj.net`, we would create the following `plist`:

```{.terminal .scrollx}
~/Library/LaunchAgents/net.ssanj.machinex.backup.plist
```

<details>
  <summary>Here is a simple `Job` definition for a script that should run daily at some time:</summary>
```{.xml .scrollx}
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
        <key>Label</key>
        <string>NAMESPACE.SCRIPT</string>
        <key>WorkingDirectory</key>
        <string>YOUR_WORKING_DIRECTORY</string>
        <key>StandardOutPath</key>
        <string>PATH_TO_YOUR_STDOUT_FILE</string>
        <key>StandardErrorPath</key>
        <string>PATH_TO_YOUR_STDERR_FILE</string>
        <key>ProgramArguments</key>
        <array>
            <string>PATH_TO_YOUR_SCRIPT</string>
        </array>
        <key>StartCalendarInterval</key>
        <dict>
            <key>Hour</key>
            <integer>HOUR_TO_RUN_AT</integer>
            <key>Minute</key>
            <integer>MINUTE_WIHIN_HOUR_TO_RUN_AT</integer>
        </dict>
</dict>
</plist>
```
</details>

<details>
  <summary>For example:</summary>
```{.xml .scrollx}
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
        <key>Label</key>
        <string>net.ssanj.machinex.backup</string>
        <key>WorkingDirectory</key>
        <string>/Users/sanj/backups</string>
        <key>StandardOutPath</key>
        <string>/Users/sanj/backups/logs/backup.stdout</string>
        <key>StandardErrorPath</key>
        <string>/Users/sanj/backups/logs/backup.stderr</string>
        <key>ProgramArguments</key>
        <array>
            <string>/Users/sanj/backups/backup.sh</string>
        </array>
        <key>StartCalendarInterval</key>
        <dict>
            <key>Hour</key>
            <integer>10</integer>
            <key>Minute</key>
            <integer>30</integer>
        </dict>
        <key>EnvironmentVariables</key>
        <dict>
             <key>PATH</key>
             <string>/opt/homebrew/bin:/opt/homebrew/sbin:/Users/sanj/bin:/bin:/usr/bin:/usr/local/bin</string>
        </dict>
</dict>
</plist>
```
</details>


There are many more parameters that can be customised to suite your needs. Have a look at [launchd.info](https://www.launchd.info/) for more.

## Job Manipulation

You can manipulate `Job` characteristics using the [launchctl](https://support.apple.com/en-au/guide/terminal/apdc6c1077b-5d5d-4d35-9c19-60f2397b2369/mac) command.

### Activate Job

To activate the `Job` use the `load` subcommand:


```{.terminal .scrollx}
launchctl load ~/Library/LaunchAgents/<NAMESPACE>.<SCRIPT>.plist
```

### Verify Job

To verify the `Job` is activated use the `list` subcommand:

```{.terminal .scrollx}
launchctl list <NAMESPACE>.<SCRIPT>
```

If this fails ensure you have loaded the correct `Job` and there are no issues with the `plist` file.

### Deactivate Job
To deactivate the `Job` use the `unload` subcommand:

```{.terminal .scrollx}
launchctl unload ~/Library/LaunchAgents/<NAMESPACE>.<SCRIPT>.plist
```

## Troubleshooting

Ensure to check your `StandardOutPath` and `StandardErrorPath` files for any errors should your script fail to run as expected. Also check that your script is correctly defined in the `plist` file and has appropriate execution rights etc.


![Godspeed](https://media.giphy.com/media/xTiTnirRhNvRJiSf96/giphy.gif)
