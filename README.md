# STE_FullScreen_MOD_Play
 MOD Player routines for ATARI STE to be used in fullscreen demos

## Credits
https://github.com/Uko-TAL/STE_FullScreen_MOD_Play

Uko from The Artic Land (T.AL) 2019

Contact: uko.tal@gmail.com or uko at http://www.atari-forum.com

## Presentation
There are plenty of routines to play the Amiga MOD Music files on Atari ST, and when it comes to fast specific STE routines for demomaking, of course the Lance one (and especially the Hacking Lance by Paulo Simoes) is certainly one of the best.

But these routines have not fixed CPU time, and when you plan to integrate them in a fullscreen demo, this can become very tricky.
And finding fullscreen-friendly routines is not easy, even today.

The source code in this repository has been designed to fulfill 2 main objectives:
1. Allow me to code from scratch a replay routine (ok it has no interest for you :smile:, but I never took the time in the golden age to implement one)
2. Have a fast (hope so !) and above all fixed execution time routine that is easy to integrate in a STE fullscreen demo (static routine with NOPS replacement)

## Implementation Overview
This code allows to replay classical 4 voices (M.K.) modules. All effects are far from being managed.

It supports all the STE DMA Replay Frequencies.

And it provides several replay modes that have different usage constraints, going from slow (23% CPU @12.5 kHz, 46% @25 kHz) but using few memory to very fast (4% CPU @12.5 kHz, 9% @25 kHz) but using lot of memory and some replay constraints.

For a given replay frequency and a given replay mode, the main part of the replay routine has fixed execution CPU time.

Some examples of usage are provided (Fullscreen or not)

This has been developped using vasm and tested on the latest Hatari and Steem SSE versions.

Full explanation of the implementation is detailed at the beginning of the [replay source code](https://github.com/Uko-TAL/STE_FullScreen_MOD_Play/blob/master/Include/MODPlay.s) 

## Contents
- Asm: source code of examples
- Bin: compiled TOS files of the examples
- Include: source code of the MOD Replay routine and of other system includes
- Mod: some MOD files used for the examples
- Python: Python script to generate tables

## Testing
In order to easily test the replay, several source codes are provided in the **Asm** directory and already compiled in the **Bin** directory.

Some may require more than 1MB of memory, it is recommended to use 2MB if you want to test all replay modes.

When you launch them, the screen will turn green while some computing is done, then some information will be displayed. Press space to hear the music !
The red/green colours (not visible in fullscreen, they are before the top border) show the decoding of the song.
The blue color show the mixing part.

- MPFSCL.TOS: Fullscreen with classical replay mode + Volume
- MPFSOV.TOS: Fullscreen with oversampling replay mode + Volume
- MPFSEX.TOS: Fullscreen with extreme replay mode (no volume)
- MPL.TOS: No fullscreen, with extreme replay mode + Volume

## Usage 
I have coded these routines for my own purposes, but I thought that they could be useful for someone else, and so I have decided to share them.
You are free to use these routines for your own productions, or to modify them as you want.
There are certainly plenty of bugs, or improvements, feel free to contact me, or to update them if you need ! 

***Just be kind and mention the credits !***


# Enjoy !
