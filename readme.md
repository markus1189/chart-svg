chart-svg
=========

[![Build
Status](https://travis-ci.org/tonyday567/chart-svg.svg)](https://travis-ci.org/tonyday567/chart-svg)
[![Hackage](https://img.shields.io/hackage/v/chart-svg.svg)](https://hackage.haskell.org/package/chart-svg)
[![lts](https://www.stackage.org/package/chart-svg/badge/lts)](http://stackage.org/lts/package/chart-svg)
[![nightly](https://www.stackage.org/package/chart-svg/badge/nightly)](http://stackage.org/nightly/package/chart-svg)

test svgs
=========

rects
-----

zero

![](other/zero.svg)

one'

![](other/one'.svg)

rotate

![](other/rotateOne.svg)

translate

![](other/translateOne.svg)

rectChart\_

![](other/rectChart_Example.svg)

rectMulti\_

![](other/rectMulti_Example.svg)

pixelChart\_

![](other/pixelExample.svg)

text
----

text

![](other/textExample.svg)

textChart\_

![](other/textChart_Example.svg)

boundText

![](other/boundText.svg)

label + bounding box

![](other/labelExample.svg)

glyphs
------

circle

![](other/circleExample.svg)

glyphs

![](other/glyphsExample.svg)

smiley

![](other/smileyExample.svg)

glyph

![](other/glyphExample.svg)

line charts
-----------

line

![](other/lineExample.svg)

gline

![](other/glineExample.svg)

labelled glyph
--------------

lglyph

![](other/lglyphExample.svg)

putting it all together

![](other/compoundExample.svg)

recipe
------

    stack build --test --exec "$(stack path --local-install-root)/bin/chart-svg" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/readme_.md -t html -o index.html --filter pandoc-include --mathjax" --file-watch --ghc-options -freverse-errors

reference
---------

-   [ghc
    options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
-   [pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
-   [libraries](https://www.stackage.org/)
-   [hoogle](https://www.stackage.org/package/hoogle)
-   [doctest](https://www.stackage.org/package/doctest)
-   [MDN svg](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial)
-   [SVG2](https://www.w3.org/TR/SVG2/text.html#TextAnchoringProperties)
-   [blaze-svg](http://hackage.haskell.org/package/blaze-svg-0.3.6.1)
-   [blaze-markup](http://hackage.haskell.org/package/blaze-markup-0.8.2.1/docs/Text-Blaze-Internal.html#t:Attributable)
-   [plot-light](https://hackage.haskell.org/package/plot-light-0.4.3/docs/src/Graphics.Rendering.Plot.Light.Internal.html#text)
-   [svg-tree](http://hackage.haskell.org/package/svg-tree-0.6.2.2/docs/Graphics-Svg-Types.html#v:documentLocation)
-   [JuicyPixels](http://hackage.haskell.org/package/JuicyPixels-3.2.9.5/docs/Codec-Picture-Types.html#t:PixelRGBA8)

