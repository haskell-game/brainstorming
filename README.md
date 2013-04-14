The Haskell Game Library
========================

This initiative aims to improve the *status quo* of developing games
and other multimedia applications using
[Haskell](http://haskell.org). We will create a set of modular
libraries with the intent that they be good enough to become the *de
facto* standards for starting new games in Haskell.

This project is currently in its early stages. This is the best time
  to get involved if you have any grand visions or objections to what
  you know about it so far. **If you are interested in contributing in
  any way, feel free to create tickets in the bug tracker and/or drop
  into the #haskell-game channel on Freenode.**

Goals and Philosophy
--------------------

The functionality of our libraries is intended to be in competition
with libraries like [pygame](http://www.pygame.org/). As a bare
minimum, we will support common game-relevant data types, windowing,
event handling, sprite-based graphics, audio, and various
game-relevant file formats.

The interfaces must be simple, high level, and consistent. The process
of creating a game should be straightforward. The dependencies and
language extensions we use will be minimized. Even if we believe the
interfaces are quite discoverable, there must also be a ridiculous
amount of accessible documentation and tutorials. We should keep in
mind that, if we meet the goal of becoming the *de facto* standard for
games in Haskell, our efforts will set precedence for any work built
on top; it is important to get this right as early as possible.

At the same time, we must take care to make sure that any higher level
interfaces we create must not sabotage efforts by game developers to
write custom, lower-level things, either. For example, while we will
provide a fairly nice 2D graphics API, it should be reasonably
possible to augment it with custom 3D graphics (using raw OpenGL or
some other library built on OpenGL which also plays nicely with
others).

The libraries we create will be fully supported on at least the most
major desktop operating systems: Windows, Mac OS X, and a some common
Linux distributions (the other distributions should easily benefit
from this support, even if only second class). Support for other
operating systems or any web browsers are not primary goals, but we
would be willing to try to make the job easy for a contributor,
especially if you get involved early. This end may already be somewhat
approachable due to the fact that we are minimizing dependencies and
language extensions.

Any barrier to entry should be as low as possible. While getting this
into the [Haskell Platform](http://www.haskell.org/platform/) is not a
primary goal, we desire the same criteria that would be necessary,
such as limiting our dependencies and avoiding conflicts with other
Haskell Platform packages.

We intend for these to be practical libraries, not research
projects. We have nothing against functional reactive programming or
shader generating domain specific languages, but our intiative does
not implement or directly support such technologies. However, a user
should be free to mix our libraries with more experimental ones.
