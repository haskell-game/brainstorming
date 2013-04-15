Graphics API
============

I propose a 2D graphics API using a similar denotation to that of
[graphics-drawingcombinators](http://hackage.haskell.org/package/graphics-drawingcombinators),
just without the extra value. Colors are actually alpha blending
functions. Images are just colors over 2D space.

    [[ RGB   ]] = R * R * R
    [[ Color ]] = {f : RGB -> RGB | exists (x : RGB) (a : [0,1]). forall c. f(c) = ax + (1-a)c}
    [[ Image ]] = R*R -> Color

One might object to hardcoding the blend function. With some extra
complexity and a little more junk in the abstraction (due to the
implementation), we could probably make it more general. We should
strongly consider this. This is only a first proposal.

Most of the API falls out of these denotations. `Color` are `Image`
are both instances of `Monoid`. Affine transformations can be applied
to `Image`.

It might also be reasonable to provide some post-processing effects.

Another thing to keep in mind is the eventuality that we will want to
allow custom shaders and such.
