/*
   file: k_k_/test/graphics/tie/ink/Svg_Color_Stripes_v_Gradient_Test.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2011 by Corbin "Kip" Kohn
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
class Svg_Color_Stripes_v_Linear_Gradient_Test extends Svg_Test_Base {

  val filename = "test_color_stripes__v__linear_gradient.svg"

  val title = "Color Stripes v. (Linear) Gradient"


  val colors = Rainbow_Palette.colors


  protected def create_canvas() = {
    val rect_ctor = Rectangle(_, _)
    val hex_ctor = (w: Double, h: Double) => Hexagon(.6*w, w, h)

    val (each_w, each_h) = (200, 160)

    val stripes1 = draw_color_stripes(rect_ctor)(each_w, each_h)(colors)
    val gradient1 = draw_gradient(rect_ctor)(each_w, each_h)(colors)

    val gradient2 = draw_gradient(Diam_Ellipse(_, _))(each_w, each_h)(colors)
    val gradient3 = draw_gradient(hex_ctor)(each_w, each_h)(colors)

    new Canvas(Canvas_Props(450, 450, title = title),
               ((label_shape(stripes1,  "color stripes")
                   -+ (-(each_w/2 + 15), 0)) -&
                (label_shape(gradient1, "(linear) gradient")
                   -+ (each_w/2 + 15, 0))
                  -+ (0, -(each_h/2 + 20))) -&

               ((label_shape((gradient2 -+ (-(each_w/2 + 15), 0)) -&
                             (gradient3 -+ (each_w/2 + 15, 0)),
                             "additional (linear) gradients")
                  -+ (0, each_h/2 + 20)))
              )
  }

  // vertical color stripes
  def draw_color_stripes(Shape: (Double, Double) => Drawing_Shape)
                        (w: Double, h: Double)(colors: Seq[Color]):
      Drawing_Shape = {
    val each_stripe_w = w / colors.length
    val colored_shapes =
          colors.map { Shape(each_stripe_w, h) -~ Pen.fill(_) }
    val translated_colored_shapes =
          colored_shapes.zipWithIndex.
                         map { p => p._1 -+ (p._2 * each_stripe_w, 0) }
    (Null_Shape /: translated_colored_shapes) ( _ -& _ ) -+
      (-w/2 + each_stripe_w/2, 0)
  }

  // vertical linear gradient
  def draw_gradient(Shape: (Double, Double) => Drawing_Shape)
                   (w: Double, h: Double)(colors: Seq[Color]): Drawing_Shape = {
    val each_stripe_pct = 100.0 / (colors.length - 1) // percentage offset
    val shape = Shape(w, h)
    val color_stops = colors.zipWithIndex.
                             map { p => Color_Stop(p._1, p._2 * each_stripe_pct) }
    shape -~ Pen.fill(Linear_Gradient(color_stops))
  }
}

}
