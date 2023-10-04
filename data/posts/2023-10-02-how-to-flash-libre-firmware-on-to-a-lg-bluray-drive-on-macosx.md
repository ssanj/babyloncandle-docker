---
title: How To Flash Libre Firmware on to an LG Bluray Drive on macOSX
author: sanjiv sahayam
description: how to flash libre firmware on to an LG drive on macOSX
tags: bluray, makemkv, libredrive, macosx
comments: true
---

I recently wanted to update the firmware of my [LG WH16NS40](https://www.amazon.com.au/gp/product/B00E7B08MS/) to the [LibreDrive](https://forum.makemkv.com/forum/viewtopic.php?t=18856) firmware. This would allow the drive to read discs from all regions and also allow for faster read access. It is also needed to read UHD discs.


My MakeMKV drive details before the update:

```{.terminal .scrollx}
Drive Information
OS device name: /dev/rdisk5
Current profile: BD-ROM
Manufacturer: HL-DT-ST
Product: BD-RE WH16NS40
Revision: 1.05
Serial number: M00M1PE1426
Firmware date: 2120-05-06 14:40
Bus encryption flags: 17
Highest AACS version: 78

LibreDrive Information
Status: Possible (with patched firmware)
Drive platform: MT1959
Harware support: Yes
Firmware support: No
Firmware type: Original (patched version available)
Firmware version: 1.05
DVD all regions: Possible (with patched firmware)
BD raw data read: Possible (with patched firmware)
BD raw metadata read: Possible (with patched firmware)
Unrestricted read speed: Possible (with patched firmware)

No disc inserted
```


To see if your drive is supported look for the `Status: Possible (with patched firmware)` comment. The [MakeMKV forums](https://forum.makemkv.com/forum/viewtopic.php?f=16&t=19634) have a full list of LibreDrive firmware supported drives, if you want to purchase a new one.

As always this information is for reading and encoding discs you own and not for any other purposes.

Also use at your own risk; these steps worked for me and it may not work for you even if you have the same drive. Check the MakeMKV forums for more information if you are unsure.

![](https://media.giphy.com/media/RHOwWKH5OY7buuGHNi/giphy.gif)


This is a summary of steps from various MakeMKV forums posts and the ["Ultra HAX0R" GUIDE V2 for encrypted firmware to Make your Drive UHD friendly](https://www.youtube.com/watch?v=jyQV1aPlbow) YouTube video.

## Steps

1. Download and extract the [all you need firmware pack](https://www.makemkv.com/download/mk-firmware-pack-20200720.zip)
2. Locate the firmware for your drive model in the pack. For my drive it was at `mk-firmware-pack-20200720/MK/HL-DT-ST/WH16NS40-NS50/HL-DT-ST-BD-RE_WH16NS40-NS50-1.05-NM00900-212005061440.bin`.

    The date on the firmware should roughly match that displayed in the MakeMKV drive details. In the MakeMKV details section the firmware date was listed as `Firmware date: 2120-05-06 14:40`, which maps to `212005061440`, which is the date at the end of the bin file: `-212005061440.bin`.

    Generally the format seems to be:

    `TYPE_OF_DRIVE`-`DRIVE_MODEL`-`FIRMWARE_VERSION`-`NOT_SURE_WHAT_THIS_IS`_`DATE`.bin

    For my drive is it:

    `HL-DT-ST-BD-RE-WH16NS40-NS50-1.05-NM00900-212005061440.bin`

    <br/>

3. The `Ultra HAX0R" GUIDE V2 for encrypted firmware to Make your Drive UHD friendly` mentions that some newer firmware needs to be flashed encrypted. A rudimentary list is given below. Please check the MakeMKV forums for a more updated list.

    Know Encrypted firmware:

    - WH16NS60 1.03
    - WH16NS40 1.05 <-- my firmware
    - WH14NS40 1.05
    - BH16NS55 1.05
    - BU40N 1.04

<br/>

4. Find location of MKV installation; usually it's under `/Applications/MakeMKV.app`
5. Look for an executable named `makemkvcon` under the MakeMVK installation folder above: `/Applications/MakeMKV.app/Contents/MacOS/makemkvcon`
6. Find the name of the drive you want flash with:



```{.terminal .scrollx}
makemkvcon f -l
```

On my machine it's `/IOBDServices/8A6BA6DF`:

```{.terminal .scrollx}
00: /IOBDServices/8A6BA6DF
  HL-DT-ST_BD-RE__WH16NS40_1.05_212005061440_M00M1PE1426
```

7. Verify the drive can be used with the tool using the following:

```{.terminal .scrollx}
makemkvcon f -d DRIVE help
```

For example:

```{.terminal .scrollx}
makemkvcon f -d /IOBDServices/8A6BA6DF help
```

8. Run the following command to flash the new firmware to your drive. Use the `enc` parameter if your drive is listed as having encrypted firmware as mentioned above. Check the forums if you're unsure about your drive.

![](https://media.giphy.com/media/MFIsOqzodLr7ewnkUb/giphy.gif)

```{.terminal .scrollx}
 makemkvcon f --all-yes -d /IOBDServices/8A6BA6DF rawflash enc -i LOCATION_OF_FIRMWARE/LG-BD-RE-WH16NS40/HL-DT-ST-BD-RE_WH16NS40-NS50-1.05-NM00900-212005061440.bin
```

Example output:

```{.terminal .scrollx}
Reading input file LG-BD-RE-WH16NS40/HL-DT-ST-BD-RE_WH16NS40-NS50-1.05-NM00900-212005061440.bin
Flashing flags = 0x0 : 0 0 0 0 : ---- ---- ---- ----
Sending image in encrypted format
Current Drive ID: HL-DT-ST_BD-RE__WH16NS40_1.05_212005061440_M00M1PE1426
Ready to write drive flash memory. (auto confirmed)
Operation started: Sending flash image to drive
 100% Operation finished
Operation started: Programming flash
 100% Operation finished

Program Flash NG 06/29/00

Command produced error code 0x82062900

```

It looks like it encountered an error:

```{.terminal .scrollx}
Command produced error code 0x82062900
```

![](https://media.giphy.com/media/l0Iy7ukGEfG2JUKTC/giphy.gif)


A quick look through the MakeMKV forums mentions that if you see  `Command produced error code 0x82062900` it can be [safely ignored](https://forum.makemkv.com/forum/viewtopic.php?f=16&t=22896&p=138110&hilit=Command+produced+error+code+0x82062900#p138110). Phew!

9. Launch MakeMKV and you should see LibreDrive support enabled:

![](/images/2023-10-02-how-to-flash-libre-firmware-on-to-a-lg-bluray-drive-on-macosx/libre-drive-support.png)

10. Use your drive through MakeMKV and ensure it's working as expected.

I managed to read a Bluray disc at faster speeds than usual, at least for some of the time. I have yet to try discs from different regions.

## References

- [Flashing-drives-on-macos](https://forum.makemkv.com/forum/viewtopic.php?t=19113)
- [All you need firmware pack](https://www.makemkv.com/download/mk-firmware-pack-20200720.zip)
- [Ripping Bluray Disc](https://blog.paco.to/2023/ripping-bluray-disks-free-easy/)
- [Reddit guide to flashing drives](https://www.reddit.com/r/makemkv/comments/mvz5h8/ultimate_uhd_drives_flashing_guide_updated_2021/)
- ["Ultra HAX0R" GUIDE V2 for encrypted firmware to Make your Drive UHD friendly](https://www.youtube.com/watch?v=jyQV1aPlbow)
- [LG WH16NS40](https://www.amazon.com.au/gp/product/B00E7B08MS/)
