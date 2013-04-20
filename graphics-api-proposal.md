Graphics API
============

I propose a 2D graphics API using a similar denotation to that of
[graphics-drawingcombinators](http://hackage.haskell.org/package/graphics-drawingcombinators),
just without the extra value. Colors are actually alpha blending
functions. Images are just colors over 2D space.

    [[ RGB     ]] = R * R * R
    [[ Color   ]] = {f : RGB -> RGB | exists (x : RGB) (a : [0,1]). forall c. f(c) = ax + (1-a)c}
    [[ Image c ]] = c*c -> Color

One might object to hardcoding the blend function. With some extra
complexity and a little more junk in the abstraction (due to the
implementation), we could probably make it more general. We should
strongly consider this generalization. This is only a first proposal.

Most of the API falls out of these denotations. `Color` and `Image`
are both instances of `Monoid`. Affine transformations can be applied
to `Image`.

It might also be reasonable to provide some post-processing effects.

Another thing to keep in mind is the eventuality that we will want to
allow custom shaders and such.

Adding Blending Modes to `Color`
--------------------------------

Here's a first pass at generalizing the semantics of `Color` for more
interesting blending modes. It looks a little complicated, but I think
most of these complicated bits can be hidden in the common case.

    data Equation = Add | Subtract | ReverseSubtract | Min | Max
    
    [[ Add             ]]e ts st td dt = ts*st + td*dt
    [[ Subtract        ]]e ts st td dt = ts*st - td*dt
    [[ ReverseSubtract ]]e ts st td dt = td*dt - ts*st
    [[ Min             ]]e ts st td dt = min ts td
    [[ Max             ]]e ts st td dt = max ts td
    
    data Function = Zero
                  | One
                  | SrcColor
                  | OneMinusSrcColor
                  | DstColor
                  | OneMinusDstColor
                  | SrcAlpha
                  | OneMinusSrcAlpha
                  | DstAlpha
                  | OneMinusDstAlpha
    
    [[ Zero               ]]f ts td as ad = 0
    [[ One                ]]f ts td as ad = 1
    [[ SrcColor           ]]f ts td as ad = ts
    [[ OneMinusSrcColor   ]]f ts td as ad = 1 - ts
    [[ DstColor           ]]f ts td as ad = td
    [[ OneMinusDstColor   ]]f ts td as ad = 1 - td
    [[ SrcAlpha           ]]f ts td as ad = ta
    [[ OneMinusSrcAlpha   ]]f ts td as ad = 1 - ta
    [[ DstAlpha           ]]f ts td as ad = ta
    [[ OneMinusDstAlpha   ]]f ts td as ad = 1 - ta

    [[ Color rgbEquation rgbSrcFun rgbDstFun alphaEquation alphaSrcFun alphaDstFun ]]c =
       exists rgbs as.
       \rgbd ad -> [[ rgbEquation   ]]e rgbs ([[ rgbSrcFun   ]]f rgbs rgbd as ad) rgbd ([[ rgbDstFun   ]]f rgbs rgbd as ad)
                ,  [[ alphaEquation ]]e as   ([[ alphaSrcFun ]]f as   ad   as ad) ad   ([[ alphaDstFun ]]f as   ad   as ad)
