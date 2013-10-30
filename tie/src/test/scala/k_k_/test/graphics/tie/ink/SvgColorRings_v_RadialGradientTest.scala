/*
   file: k_k_/test/graphics/tie/ink/Svg_Color_Rings_v_Radial_Gradient_Test.scala

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

import k_k_.graphics.tie._
import k_k_.graphics.tie.ink._
import k_k_.graphics.tie.ink.Color._
import k_k_.graphics.tie.ink.palette._
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.text._


@Test
class SvgColorRings_v_RadialGradientTest extends SvgTestBase {

  val filename = "test_color_rings__v__radial_gradient.svg"

  val title = "Color Rings v. (Radial) Gradient"


  val colors = RainbowPalette.colors


  protected def createCanvas() = {
    val ellipseCtor = DiamEllipse(_, _)
    val hexCtor = (w: Double, h: Double) => Hexagon(.6*w, w, h)

    val (eachW, eachH) = (200, 160)

    val stripes1 = drawColorRings(ellipseCtor)(eachW, eachH)(colors)
    val gradient1 = drawGradient(ellipseCtor)(eachW, eachH)(colors)

    val gradient2 = drawGradient(Rectangle(_, _))(eachW, eachH)(colors)
    val gradient3 = drawGradient(hexCtor)(eachW, eachH)(colors)

    new Canvas(
        CanvasProps(450, 450, title = title),
        ((labelShape(stripes1,  "color rings")       -+ (-(eachW/2 + 15), 0)) -&
         (labelShape(gradient1, "(radial) gradient") -+ (  eachW/2 + 15,  0))
             -+ (0, -(eachH/2 + 20))) -&

        ((labelShape(
              (gradient2 -+ (-(eachW/2 + 15), 0)) -&
              (gradient3 -+ (eachW/2 + 15, 0)),
              "additional (radial) gradients [reflect colors]"
            )
             -+ (0,   eachH/2 + 20)
          ))
      )
  }

  // concentric colored rings
  def drawColorRings(
      S: (Double, Double) => Shape
    )(
      w: Double, h: Double
    )(
      colors: Seq[Color]
    ): Shape = {
    val eachRingW = w / colors.length
    val eachRingH = h / colors.length
    val coloredShapes = colors.reverse.zipWithIndex.map { p =>
      (p._1, p._2 + 1)
    }.map { case (color, i) =>
      val penTransform: Pen => Pen =
          if (i == colors.length) identity _ else _.stroke(NullInk)
      S(eachRingW * i, eachRingH * i) -~ penTransform(Pen.fill(color))
    }
    // reverse so smaller shapes over larger ones, and thereby not occluded
    (NullShape /: coloredShapes.reverse) ( _ -& _ )
  }

  // concentric radial gradient
  def drawGradient(
      S: (Double, Double) => Shape
    )(
      w: Double, h: Double
    )(
      colors: Seq[Color]
    ): Shape = {
    val eachStripeOffsetPct = 100.0 / (colors.length - 1)
    val shape = S(w, h)
    val colorStops = colors.reverse.zipWithIndex.map { p =>
      ColorStop(p._1, p._2 * eachStripeOffsetPct)
    }
    shape -~ Pen.fill(RadialGradient(colorStops, ColorSpread.Reflect))
  }
}

}
