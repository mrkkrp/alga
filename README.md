# ALGA

*First of all, this stuff is under development. It's not usable yet. This
 repository is place for me to experiment indefinitely until the project can
 be used by other people.*

How to algorithmically control every aspect of music using familiar, robust
tools: plugins, DAWs, etc. that have not built with this in mind? I've
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

1. Install [Haskell Platform](https://www.haskell.org/platform/);
2. Install [Cabal](https://www.haskell.org/cabal/);
3. Download and untar git repository of MIDA, or clone it:

   ```
   $ git clone https://github.com/mrkkrp/alga.git master
   ```

4. Go to the root directory of the repository and execute:

   ```
   $ cabal update
   $ cabal configure
   $ cabal install --only-dependencies
   $ cabal build
   # sh install.sh
   ```

5. Done (you can use `uninstall.sh` to uninstall the program).

## License

Copyright © 2015 Mark Karpov

Distributed under GNU GPL, version 3.
