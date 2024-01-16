---
title: Authentication Failure On Samba Shares After Upgrading To Macos Sonoma
author: sanjiv sahayam
description: How to fix authentication failures on samba shares after upgrading to macos sonoma.
tags: macosx, sonoma, samba
comments: true
---

I recently upgraded to macOS Sonoma `(14.2.1 (23C71))`. Surprisingly, it seemed to go smoothly. Nothing was broken, as was the usual case
with a macOS upgrade. That was until I tried to access one of my Samba shares. Authentication to the share failed.
As I was using a password manager, I knew the password was correct. Oh dear. A lot of the files I use on a daily basis
lived on my Samba shares.

![Devastated](https://media.giphy.com/media/APcfVOIIFi4Pm/giphy.gif)

## What worked for me

> Try using all caps for your username when logging in.

## Investigation

Knowing the password was correct and that I had not changed anything on the machines running the Samba shares, I chose
to verify I could log into the shares from multiple Linux machines. I could access the Samba shares without any issues.
I could also list all the shared folders and they were all there as expected.

This had to be an upgrade issue. Urg.

After Googling for a while I didn't come up with anything concrete. I tried rebooting my Mac but the issue still persisted.
I next tried deleting the Keystore entries for the Samba logins and still no dice. This was starting to become a huge
time sink.

I turned on [debug-level](https://www.samba.org/samba/docs/current/man-html/smb.conf.5.html#LOGLEVEL) logging on the Samba server by updating my `/etc/smb.conf` with:

```{.terminal .scrollx}
   log level = 3
```

This would write out verbose logs into my `/var/logs/samba/*.log` files.

I notice that the server had issues authenticating my macOS user:

```{.terminal .scrollx}
Got user=[myshareuser] domain=[MYDOMAIN] workstation=[MYMAC] len1=24 len2=186
4146 [2024/01/13 18:00:31.784745,  3] ../../source3/auth/auth.c:201(auth_check_ntlm_password)
4147   check_ntlm_password:  Checking password for unmapped user [MYDOMAIN]\[myshareuser]@[MYMAC] wi
     th the new password interface
4148 [2024/01/13 18:00:31.784799,  3] ../../source3/auth/auth.c:204(auth_check_ntlm_password)
4149   check_ntlm_password:  mapped user is: [MYDOMAIN]\[myshareuser]@[MYMAC]
4150 [2024/01/13 18:00:31.785138,  3] ../../source3/passdb/lookup_sid.c:1720(get_primary_group_sid
     )
4151   Forcing Primary Group to 'Domain Users' for myshareuser
4152 [2024/01/13 18:00:31.785395,  3] ../../libcli/auth/ntlm_check.c:446(ntlm_password_check)
4153   ntlm_password_check: NTLMv2 password check failed
4154 [2024/01/13 18:00:31.785446,  3] ../../libcli/auth/ntlm_check.c:492(ntlm_password_check)
4155   ntlm_password_check: Lanman passwords NOT PERMITTED for user myshareuser
4156 [2024/01/13 18:00:31.785573,  3] ../../libcli/auth/ntlm_check.c:637(ntlm_password_check)
4157   ntlm_password_check: LM password, NT MD4 password in LM field and LMv2 failed for user smbm
     edia
4158 [2024/01/13 18:00:31.786072,  2] ../../source3/auth/auth.c:345(auth_check_ntlm_password)
4159   check_ntlm_password:  Authentication for user [myshareuser] -> [myshareuser] FAILED with error NT
     _STATUS_WRONG_PASSWORD, authoritative=1
```

I didn't get this issue when logging in from a Linux machine with the same user and password.

I also updated my minimum SMB protocol version to SMB2, as I read there maybe protocol issues if the Mac was trying
to authenticate with SMB1:

```{.terminal .scrollx}
   min protocol = SMB2
```

This also seemed to have no effect on solving this issue.

## Logging in with a guest user

I created a new Samba guest share; one that didn't require a password:

```{.terminal .scrollx}
[testshare]
  comment = test share
  path = /my_shares/testshare
  read only = no
  writeable = yes
  browsable = yes
  guest ok = yes
  guest only = yes
```

This worked when I tried to log into the share from macOS! I didn't want to go through and remove the passwords from all
my Samba shares though. This was a good fallback if I couldn't find a better workaround.


## Using uppercase user names

I finally ran across the [Weird samba password issue](https://www.reddit.com/r/debian/comments/18ak7n0/weird_samba_password_issue/) Reddit thread.
In that thread the user [az999999](https://www.reddit.com/user/az999999/) said he'd found a workaround. A few other users confirmed the workaround worked for
them as well.

I tried using the uppercased username with not much hope of it actually working. But guess what? It worked!!!

I am not sure why this was necessary. I've used all my Samba shared without uppercased usernames in macOS Venture and all the other macOSs before that.

*Sigh*


But in any case this workaround works for the moment.

Also `az999999` is a God-damned hero.

## A little more evidence

After getting this to work I wondered if I could spot anything of interest in the Samba logs.

I got the following JSON dump in the logs in a login that succeeded:

```json
{
       "timestamp": "2024-01-13T18:00:38.471589+1100",
       "type": "Authentication",
       "Authentication":
       {
           "version":
           {
               "major": 1,
               "minor": 2
           },
           "eventId": 4624,
           "logonId": "0",
           "logonType": 3,
           "status": "NT_STATUS_OK",
           "localAddress": "ipv4:SERVER_IP",
           "remoteAddress": "ipv4:CLIENT_IP",
           "serviceDescription": "SMB2",
           "authDescription": null,
           "clientDomain": "MYDOMAIN",
           "clientAccount": "MYSHAREUSER",
           "workstation": "MYMAC",
           "becameAccount": "myshareuser",
           "becameDomain": "MYDOMAIN",
           "becameSid": "MY-SID",
           "mappedAccount": "MYSHAREUSER",
           "mappedDomain": "MYDOMAIN",
           "netlogonComputer": null,
           "netlogonTrustAccount": null,
           "netlogonNegotiateFlags": "0x00000000",
           "netlogonSecureChannelType": 0,
           "netlogonTrustAccountSid": null,
           "passwordType": "NTLMv2",
           "duration": 19755
       }
   }
```

In the ones that failed I got the following JSON dump in the logs:


```json
{
    "timestamp": "2024-01-13T18:00:31.786365+1100",
    "type": "Authentication",
    "Authentication":
    {
        "version":
        {
            "major": 1,
            "minor": 2
        },
        "eventId": 4625,
        "logonId": "0",
        "logonType": 3,
        "status": "NT_STATUS_WRONG_PASSWORD",
        "localAddress": "ipv4:SERVER_IP",
        "remoteAddress": "ipv4:CLIENT_IP",
        "serviceDescription": "SMB2",
        "authDescription": null,
        "clientDomain": "MYDOMAIN",
        "clientAccount": "myshareuser",
        "workstation": "MYMAC",
        "becameAccount": null,
        "becameDomain": null,
        "becameSid": null,
        "mappedAccount": "myshareuser",
        "mappedDomain": "MYDOMAIN",
        "netlogonComputer": null,
        "netlogonTrustAccount": null,
        "netlogonNegotiateFlags": "0x00000000",
        "netlogonSecureChannelType": 0,
        "netlogonTrustAccountSid": null,
        "passwordType": "NTLMv2",
        "duration": 5016
    }
}
```

The fields to note are the `became*` fields:

- becameDomain
- becameAccount
- becameSid

In the logins that worked, these field were populated and in the failed logins they were not.

It seems like Sonoma only sends over some required parameters to the Samba login process when the username is uppercased. All very weird.

## Additional links and resources

I wondered if there was some documentation that required/recommended that you supply the Samba username in all caps. After hunting around for a while
I was only able to find one article that mentions it: [Samba Troubles With My Mac - Password Never Accepted](https://apple.stackexchange.com/questions/5702/samba-troubles-with-my-mac-password-never-accepted).

You can get all sorts of useful Samba information with the `smbutil` tool that ships with Sonoma:

```{.terminal .scrollx}
usage: smbutil [-hv] subcommand [args]
where subcommands are:
 help          display help on specified subcommand
 lookup        resolve NetBIOS name to IP address
 status        resolve IP address or DNS name to NetBIOS names
 view          list resources on specified host
 dfs           list DFS referrals
 identity      identity of the user as known by the specified host
 statshares    list the attributes of mounted share(s)
 multichannel  list the attributes of the channels of mounted share(s)
 snapshot      list snapshots for the mount path
 smbstat       list info about item at path
```

The [smb.conf](https://www.samba.org/samba/docs/current/man-html/smb.conf.5.html) manpage website is a great resource to find out about all the different configuration options for your Samba server.

I also wrote out a `/etc/nsmb.conf` file on my macOS machine:

```{.terminal .scrollx}
 [default]
  2 protocol_vers_map=6
  3 port445=no_netbios
  4 mc_prefer_wired=yes
  5 mc_on=yes
```

This seemed to have zero effect on this problem. I'm just listing it here in case it might solve a similar problem for some people.

I hope this at least helped some of you that have a similar issue. So far macOS Sonoma is turning out to be a terrible upgrade; mostly for nicer wallpapers.
