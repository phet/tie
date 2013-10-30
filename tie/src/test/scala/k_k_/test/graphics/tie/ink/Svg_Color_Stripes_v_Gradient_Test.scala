/*
   file: k_k_/test/graphics/tie/ink/Svg_Color_Stripes_v_Gradient_Test.scala

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
class SvgColorStripes_v_LinearGradientTest extends SvgTestBase {

  val filename = "test_color_stripes__v__linear_gradient.svg"

  val title = "Color Stripes v. (Linear) Gradient"


  val colors = RainbowPalette.colors


  protected def createCanvas() = {
    val rectCtor = Rectangle(_, _)
    val hexCtor = (w: Double, h: Double) => Hexagon(.6*w, w, h)

    val (eachW, eachH) = (200, 160)

    val stripes1 = drawColorStripes(rectCtor)(eachW, eachH)(colors)
    val gradient1 = drawGradient(rectCtor)(eachW, eachH)(colors)

    val gradient2 = drawGradient(DiamEllipse(_, _))(eachW, eachH)(colors)
    val gradient3 = drawGradient(hexCtor)(eachW, eachH)(colors)

    new Canvas(
        CanvasProps(450, 450, title = title),
        ((labelShape(stripes1,  "color stripes")     -+ (-(eachW/2 + 15), 0)) -&
         (labelShape(gradient1, "(linear) gradient") -+ (  eachW/2 + 15,  0))
         -+ (0, -(eachH/2 + 20))
         ) -&

        ((labelShape(
              (gradient2 -+ (-(eachW/2 + 15), 0)) -&
              (gradient3 -+ (  eachW/2 + 15,  0)),
              "additional (linear) gradients"
            )
            -+ (0, eachH/2 + 20)
          ))
      )
  }

  // vertical color stripes
  def drawColorStripes(
      S: (Double, Double) => Shape
    )(
      w: Double, h: Double
    )(
      colors: Seq[Color]
    ): Shape = {
    val eachStripeW = w / colors.length
    val coloredShapes = colors.map { S(eachStripeW, h) -~ Pen.fill(_) }
    val translatedColoredShapes = coloredShapes.zipWithIndex.map { p =>
      p._1 -+ (p._2 * eachStripeW, 0)
    }
    (NullShape /: translatedColoredShapes) ( _ -& _ ) -+
        (-w/2 + eachStripeW/2, 0)
  }

  // vertical linear gradient
  def drawGradient(
      S: (Double, Double) => Shape
    )(
      w: Double, h: Double
    )(
      colors: Seq[Color]
    ): Shape = {
    val eachStripePct = 100.0 / (colors.length - 1) // percentage offset
    val shape = S(w, h)
    val colorStops = colors.zipWithIndex.map { p =>
      ColorStop(p._1, p._2 * eachStripePct)
    }
    shape -~ Pen.fill(LinearGradient(colorStops))
  }
}

}
