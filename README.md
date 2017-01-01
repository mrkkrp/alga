# ALGA

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Hackage](https://img.shields.io/hackage/v/alga.svg?style=flat)](https://hackage.haskell.org/package/alga)
[![Build Status](https://travis-ci.org/mrkkrp/alga.svg?branch=master)](https://travis-ci.org/mrkkrp/alga)

How to algorithmically control every aspect of music using familiar, robust
tools: plugins, DAWs, etc. that have not been built with this in mind? I've
partially solved the problem in [MIDA](https://github.com/mrkkrp/mida) —
program that generates MIDI files using simple, symmetric, declarative
language. However, even though MIDA is a fine tool to create scores, it's
not sufficient if you want to control everything. Initially I thought I
could find some plugins that can algorithmically control other plugins, but
there are no decent tools of this sort. How automation is handled in a
traditional DAW? Well, you draw it in a separate track. Wouldn't it be a
good idea to algorithmically perform exactly that — drawing of the
automation track? This is simpler than it may seem: many DAWs allow to
export tracks in XML format, and this is all we need to start our hacking.

## How does it work?

You export your tracks in XML format, write some scripts in
[MIDA](https://github.com/mrkkrp/mida)-like language and patch the XML
file. Then you can import the files back, they will (hopefully) have fancy
automation tracks in place.

## Installation

1. Install the [Haskell Tool Stack](http://haskellstack.org).

2. Add `~/.local/bin` directory to your `PATH`, like this:

   ```
   # in .bashrc or similar
   export PATH=$HOME/.local/bin:$PATH
   ```

3. Clone the repo, `cd` into it, and let `stack` do its thing:

   ```
   $ git clone https://github.com/mrkkrp/alga.git
   $ cd mida
   $ stack build --copy-bins
   ```

4. Check it out:

   ```
   $ alga --version
   ALGA 0.2.0
   ```

## Example

ALGA is a simple declarative language. Just to get feeling of it:

```
#
# Example of ALGA program
#

my_track.pan  = {0..16} / 16 $ [{2}]
my_track.pand = 1/8, 0
```

This thing changes position of stereo-panorama for track named
`my_track`. There are 16 possible values from left to right. Panorama is
changed instantly, every eighth note.

You can control volume, panorama, mute, input gain, all parameters of send
slots, insertion slots, and software synths. Changes can be applied to audio
tracks, group tracks, instrument tracks, and effect tracks.

See [ALGA Manual](https://mrkkrp.github.io/alga/) for more information.

## License

Copyright © 2015–2017 Mark Karpov

Distributed under GNU GPL, version 3.
