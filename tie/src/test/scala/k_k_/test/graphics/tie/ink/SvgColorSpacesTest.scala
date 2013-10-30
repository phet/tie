/*
   file: k_k_/test/graphics/tie/ink/Svg_Color_Spaces_Test.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2013 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.test.graphics.tie 

package ink {

import org.junit._
import Assert._

import k_k_.graphics.tie._
import k_k_.graphics.tie.ink.{NamedColors => C, _}
import k_k_.graphics.tie.ink.palette._
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.text._


@Test
class SvgColorSpacesGreenRedTest extends SvgColorSpacesTestBase {

  val filename = "test_color_spaces[green-red].svg"

  val title = "Color Spaces: RGB lerp v. HSL transition v. HSL hue transition"


  protected def createCanvas() = {
    val (colorA, colorB) = (C.green, C.red)
    val (colorAName, colorBName) = (colorA.name, colorB.name)

    def labeledSwatch(c: NamedColor): Shape = {
      val swatch = swatchCtor(swatchW, swatchH) -~ Pen.fill(c)
      labelShape(swatch, c.name)
    }

    val labeledColorA = labeledSwatch(colorA)
    val labeledColorB = labeledSwatch(colorB)

    val transitionName = colorAName + " ... " + colorBName
    def labelTransition(c: NamedColor): String = {
      transitionName +
          " [HSL transition hue (constant lightness of '%s')]".format(c.name)
    }

    val const_L_cmp = drawSwatchCmp(
        colorA.transitionHuesBy(colorB, 1.0 / nStops),
        labelTransition(colorA),
        colorB.transitionHuesBy(colorA, 1.0 / nStops).reverse,
        labelTransition(colorB),
        labeledColorA,
        labeledColorB
      )

    val rgbHslCmp = drawRgbHslCmp(colorA, colorB)

    // NOTE: size of both layouts expected to be identical
    val Rectangular(_, layoutH) = rgbHslCmp.boundingBox

    new Canvas(
        CanvasProps(1000, 400, title = title),
        (rgbHslCmp   -+ (0, -(layoutH/2 + 30))) -&
        (const_L_cmp -+ (0,  (layoutH/2 + 30)))
      )
  }
}


@Test
class SvgColorSpacesBlueOrangeTest extends SvgColorSpacesTestBase {

  val filename = "test_color_spaces[blue-orange].svg"

  val title =
        "Color Spaces: RGB lerp v. HSL transition v. HSL saturation, lightness"


  protected def createCanvas() = {
    val (colorA, colorB) = (C.blue, C.orange)
    val (colorAName, colorBName) = (colorA.name, colorB.name)

    val (colorASat, colorADesat) = (colorA.saturate(1.0),colorA.desaturate(1.0))
    val (colorBSat, colorBDesat) = (colorB.saturate(1.0),colorB.desaturate(1.0))

    val desatCmp = drawSwatchCmp(
        colorASat.transitionSaturationsBy(colorADesat, 1.0 / nStops),
        colorAName + " desaturation",
        colorBSat.transitionSaturationsBy(colorBDesat, 1.0 / nStops),
        colorBName + " desaturation",
        NullShape, NullShape
      )

    val (colorALight, colorADark) = (colorA.lighten(1.0), colorA.darken(1.0))
    val (colorBLight, colorBDark) = (colorB.lighten(1.0), colorB.darken(1.0))
    val lightnessCmp = drawSwatchCmp(
        colorALight.transitionLightnessesBy(colorADark, 1.0 / nStops),
        colorAName + " lightness",
        colorBLight.transitionLightnessesBy(colorBDark, 1.0 / nStops),
        colorBName + " lightness",
        NullShape, NullShape
      )

    val rgbHslCmp = drawRgbHslCmp(colorA, colorB)

    // NOTE: size of both layouts expected to be identical
    val Rectangular(_, layoutH) = rgbHslCmp.boundingBox

    new Canvas(
        CanvasProps(1000, 540, title = title),
        (rgbHslCmp    -+ (0, -(layoutH + 30))) -&
        (desatCmp     -+ (0,  0))              -&
        (lightnessCmp -+ (0,  (layoutH + 30)))
      )
  }
}


abstract class SvgColorSpacesTestBase extends SvgTestBase {

  val nStops = 16
  val (swatchW, swatchH) = (50, 50)
  val eachSwatchPadX = 5

  final val (rowW, rowH) = (nStops*(eachSwatchPadX + swatchW), swatchH)

  val swatchCtor = Rectangle(_, _)
  // val swatchCtor = (w: Double, h: Double) => Hexagon(.6*w, w, h)


  @Test
  def testComplementIsInverse() = {
    for (color <- RainbowPalette.colors) {
      // NOTE: due to impl., additionally tests RGB -> HSL -> RGB (re)conversion
      assertEquals(color, color.complement.complement)
    }
  }


  def drawRgbHslCmp(colorA: NamedColor, colorB: NamedColor): Shape = {
    val (colorAName, colorBName) = (colorA.name, colorB.name)

    val colorASwatch = swatchCtor(swatchW, swatchH) -~ Pen.fill(colorA)
    val colorBSwatch = swatchCtor(swatchW, swatchH) -~ Pen.fill(colorB)

    val labeledColorA = labelShape(colorASwatch, colorAName)
    val labeledColorB = labelShape(colorBSwatch, colorBName)

    val transitionName = colorAName + " ... " + colorBName
    drawSwatchCmp(
        colorA.lerpsBy      (colorB, 1.0 / nStops),
        transitionName + " [RGB lerps]",
        colorA.transitionsBy(colorB, 1.0 / nStops),
        transitionName + " [HSL transitions]",
        labeledColorA, labeledColorB
      )
  }

  // horizontally aligned color swatches
  def drawColorSwatchRow(
      S: (Double, Double) => Shape
    )(
      rowW: Double, rowH: Double, horizPad: Double
    )(
      colors: Seq[Color]
    ): Shape = {
    val eachColW = rowW / colors.length
    val eachSwatchW = eachColW - horizPad
    val swatches = colors map { S(eachSwatchW, rowH) -~ Pen.fill(_) }
    val translatedSwatches = swatches.zipWithIndex.map { x =>
      x._1 -+ (x._2 * eachColW, 0)
    }
    (NullShape /: translatedSwatches) ( _ -& _ ) -+ (-rowW/2 + eachColW/2, 0)
  }

  def drawSwatchCmp(
      swatchXColors: Seq[Color], swatchXLabel: String,
      swatchYColors: Seq[Color], swatchYLabel: String,
      openShape: Shape, closeShape: Shape
    ): Shape = {

    val swatchColors =
        drawColorSwatchRow(swatchCtor)(rowW, rowH, eachSwatchPadX) _
                                                           
    val layoutCmp = layoutSwatchRowCmp(rowW, rowH, eachSwatchPadX) _


    val xSwatches = swatchColors(swatchXColors)
    val ySwatches = swatchColors(swatchYColors)

    val labeledXSwatches = labelShape(xSwatches, swatchXLabel)
    val labeledYSwatches = labelShape(ySwatches, swatchYLabel)

    layoutCmp(openShape, closeShape, labeledXSwatches, labeledYSwatches)
  }

  def layoutSwatchRowCmp(
      rowW: Double, rowH: Double, horizPad: Double
    )(
      swatchA: Shape, swatchB: Shape, rowX: Shape, rowY: Shape
    ): Shape = {
    // NOTE: size of both swatches expected to be identical
    val Rectangular(swatchW, _) = swatchA.boundingBox
    ((swatchA -+ (-(rowW + swatchW + horizPad)/2, 0)) -&
     (swatchB -+ ( (rowW + swatchW + horizPad)/2, 0)) -&

     (rowX    -+ (0, -(rowH/2 + 15))) -&
     (rowY    -+ (0,  (rowH/2 + 15))))
  }
}

}
