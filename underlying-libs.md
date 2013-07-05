Underlying libraries criteria
=============================

* C
* fairly permissive license (BSD, MIT, ZLIB/LIBPNG)
  + No restrictions on static linking
  + Definitely not (L)GPL
* actively maintained

Windowing/input
===============

Allegro

* Pros
  + Allegro 5 has a BSD3 license
  + actively maintained
  + supports multiple contexts
* Cons
  + Existing Haskell bindings (BSD3) look unmaintained and incomplete (but not a bad start)
  + would be a very complicated build if we want to include it directly in a Haskell package
  + the addons have a lot of C dependencies (reasonable ones, at least)

GLUT - just no

GLFW - tentative yes

* Pros
  + simple
  + quite portable (apart from the GHCi on OS X issue)
  + zlib/libpng license
  + multiple contexts as of v3

* Cons
  + doesn't work in GHCi in OS X without some extra steps (probably
    not fundamentally due to the C lib)

SDL - rejected (due to LGPL)

* Pros
  + many features
  + 2.0 is under zlib

* Cons
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
  + LGPL license

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
  + Low-Level haskell binding already exists http://hackage.haskell.org/package/freetype2
* Cons

Image Loading
=============

GraphicsMagick (http://www.graphicsmagick.org/)

* Pros
  + MIT-licensed
  + Claims many improvements over ImageMagick including 100% memcheck/helgrind clean
* Cons

DevIL - rejected due to licensing

http://openil.sourceforge.net/

* Pros
  + Popular, well-tested etc (used by Ogre3D iirc)
* Cons
  + LGPL

FreeImage

http://freeimage.sourceforge.net/

* Pros
  + Popular (used by Unity and others: http://freeimage.sourceforge.net/users.html)
  + FIPL license, allows for commercial use apparently (http://freeimage.sourceforge.net/license.html)
* Cons
  + Jeanne-Kamikaze: I personally find it a pita to use
  + jmcarthur: Unusual licenses make me nervous

stb_image

http://nothings.org/stb_image.c

* Pros
  + Extremely lightweight (single C file)
  + Public domain
  + Jeanne-Kamikaze: I have personally used it, extremely easy to use
  + Existing Haskell bindings
* Cons
  + Does not support all types of images or compressions. Fails with some compressed PNGs and what not
  + Doesn't seem to be actively developed.

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
  + Also very lightweight, it's in fact an extension of stb_image with support for more formats.
  + Has a screenshot function.
  + Public domain
* Cons
  + Hasn't been updated in a long time (July 7, 2008)

MagickCore

http://www.imagemagick.org/script/magick-core.php

* Pros
  + It's part of the imagemagick suite, must be good
  + Apache license - I suppose this is good but I haven't read the details

JuicyPixels - yes

* Pros
  + 100% Haskell, so likely to Just Work(tm) anywhere GHC does
  + Also saves images
  + Appears to be actively developed
  + BSD license
  + Schedule for inclusion in some future version of the Haskell Platform (eg., it should continue to gain popularity)
* Cons
  + Relatively limited API
  + Only supports a handful of common image formats
  + Relatively young library, not as time-tested as some of the C libs above

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

Sound and Music Loading
=======================

PortAudio has little or no support for loading of formatted audio data. Therefore, we will need a library to
load at least a few formats.

HCodecs?

* Pros
  + BSD3
  + Pure Haskell (no external deps)
* Cons
  + Not very many codecs supported
  + Last update is Nov. 2011
