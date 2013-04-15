Underlying libraries criteria
=============================

* C
* fairly permissive license (BSD, MIT, ZLIB/LIBPNG)
  + No restrictions on static linking
  + Definitely not (L)GPL
* actively maintained

Windowing/input
===============

GLUT - just no

GLFW - tentative yes

* Pros
  + simple
  + quite portable (apart from the GHCi on OS X issue)
  + zlib/libpng license

* Cons
  + only one context
  + doesn't work in GHCi in OS X without some extra steps (probably
    not fundamentally due to the C lib)

SDL - rejected (due to LGPL)

* Pros
  + many features
  + 2.0 is under zlib (but not released yet)

* Cons
  + 1.2 is under LGPL
  + limited audio interface
  + tough to make work out of the box (in jmcarthur's experiences)

SFML

* Pros
  + Multiple contexts; does the managing for you
  + Many features
  + zlib/libpng license

* Cons
  + It's C++ and therefore hard to make self-contained in a binding

WX

* Pros
  + Works on Windows efortlessly
  + Lightweight
  + Immediate access to GUI stuff for applications that want it (most won't)

* Cons
  + Haskell bindings have platform-specific build instructions

GTK

* Pros
  + Lots of control
  + Immediate access to GUI stuff for applications that want it (most won't)

* Cons
  + More work to use
  + Haskell bindings are crazy to set up
  + Not the best of citizens on all platforms (I think)
  + Last time I tried to build GTK on Windows I failed royally.

Native OS

* Pros
  + Infinite flexibility

* Cons
  + Soooooooo much work

Audio
=====

portaudio (with some higher level C wrapper) - tentative yes

* Pros
  + MIT

* "Cons" (These aren't really cons because we're building a C wrapper)
  + existing bindings on hackage are bit-rotted
  + have to deal with callbacks to fill the audio buffer

* Cons
?

OpenAL - rejected (new versions are proprietary)

Graphics
========

OpenGL - yes

Q: What version do we target?

A: 2.1 + very common extensions such as FBOs. This does not rule out
   the possibility of an end user using features from later versions,
   either.

Q: Quad per sprite or one quad for entire window?

A: Quad per sprite. The best reason is that it is more conventional,
   but it also saves us a lot of work. There are still plenty of
   opportunities to do fancy things if we want.

Text Rendering
==============

TTF, bitmap, 2d/3d etc.

FreeType

* Pros
  + Popular, well-tested etc.
  + FTL license (BSD style, http://www.freetype.org/license.html)
* Cons

Image Loading
=============

DevIL

http://openil.sourceforge.net/

* Pros
  + Popular, well-tested etc
  + Used by Ogre3D iirc
* Cons
  + LGPL

FreeImage

http://freeimage.sourceforge.net/

* Pros
  + Popular (used by Unity and others: http://freeimage.sourceforge.net/users.html)
  + FIPL license, allows for commercial use apparently (http://freeimage.sourceforge.net/license.html)
* Cons
  + I personally find it a pita to use (Jeanne-Kamikaze)

stb_image

http://nothings.org/stb_image.c

* Pros
  + Extremely lightweight (single C file)
  + Public domain
  + I have personally used it, extremely easy to use ((Jeanne-Kamikaze)
* Cons
  + Does not support all types of images or compressions. Fails with some compressed PNGs and what not

Corona

http://corona.sourceforge.net/

* Pros
  + Mature
  + Easy to use
  + zlib license
  + It is actually active despite not having been updated in a long time. I personally couldn't get it building on Arch, mailed the author and got it fixed the next day (Jeanne-Kamikaze)
* Cons
  + Not very actively developed (latest news from 2003), but the author is still alive lurking in the shadows (see above)

SOIL

http://www.lonesock.net/soil.html

* Pros
  + Seems to be made specifically for OpenGL apps
  + Public domain
* Cons
  + Hasn't been updated in a long time (July 7, 2008)

MagickCore

http://www.imagemagick.org/script/magick-core.php

* Pros
  + It's part of the imagemagick suite, must be good
  + Apache license - I suppose this is good but I haven't read the details

GUI
===

An in-game GUI library would be a nice eventual addition, but not
immediately.

AntTweakBar: http://anttweakbar.sourceforge.net/doc/

Network
=======

enet? See details here: http://enet.bespin.org/Features.html.

* Pros
  + Most suitable for games and real-time apps
  + Implements a subset of TCP on top of UDP to allow for optional
    safe delivery
  + Pretty standard and battle tested
  + MIT license? http://enet.bespin.org/License.html

Q: Do we even want to provide any direct support for networking?
