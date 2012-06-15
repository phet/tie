/*
   file: k_k_/test/graphics/tie/ink/Svg_Color_Rings_v_Radial_Gradient_Test.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
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
class Svg_Color_Rings_v_Radial_Gradient_Test extends Svg_Test_Base {

  val filename = "test_color_rings__v__radial_gradient.svg"

  val title = "Color Rings v. (Radial) Gradient"


  val colors = Rainbow_Palette.colors


  protected def create_canvas() = {
    val ellipse_ctor = Diam_Ellipse(_, _)
    val hex_ctor = (w: Double, h: Double) => Hexagon(.6*w, w, h)

    val (each_w, each_h) = (200, 160)

    val stripes1 = draw_color_rings(ellipse_ctor)(each_w, each_h)(colors)
    val gradient1 = draw_gradient(ellipse_ctor)(each_w, each_h)(colors)

    val gradient2 = draw_gradient(Rectangle(_, _))(each_w, each_h)(colors)
    val gradient3 = draw_gradient(hex_ctor)(each_w, each_h)(colors)

    new Canvas(Canvas_Props(450, 450, title = title),
               ((label_shape(stripes1,  "color rings")
                   -+ (-(each_w/2 + 15), 0)) -&
                (label_shape(gradient1, "(radial) gradient")
                   -+ (each_w/2 + 15, 0))
                  -+ (0, -(each_h/2 + 20))) -&

               ((label_shape((gradient2 -+ (-(each_w/2 + 15), 0)) -&
                             (gradient3 -+ (each_w/2 + 15, 0)),
                             "additional (radial) gradients [reflect colors]")
                  -+ (0, each_h/2 + 20)))
              )
  }

  // concentric colored rings
  def draw_color_rings(Shape: (Double, Double) => Drawing_Shape)
                      (w: Double, h: Double)(colors: Seq[Color]):
      Drawing_Shape = {
    val each_ring_w = w / colors.length
    val each_ring_h = h / colors.length
    val colored_shapes = colors.reverse.zipWithIndex.
                           map { p => (p._1, p._2 + 1) }.
                           map { case (color, i) =>
                               val pen_transform: Pen => Pen =
                                     if (i == colors.length) identity _
                                     else                    _.stroke(Null_Ink)
                               Shape(each_ring_w * i, each_ring_h * i) -~
                                 pen_transform(Pen.fill(color))
                             }
    // reverse so smaller shapes over larger ones, and thereby not occluded
    (Null_Shape /: colored_shapes.reverse) ( _ -& _ )
  }

  // concentric radial gradient
  def draw_gradient(Shape: (Double, Double) => Drawing_Shape)
                   (w: Double, h: Double)(colors: Seq[Color]): Drawing_Shape = {
    val each_stripe_offset_pct = 100.0 / (colors.length - 1)
    val shape = Shape(w, h)
    val color_stops = colors.reverse.zipWithIndex.
                        map ( p => Color_Stop(p._1,
                                              p._2 * each_stripe_offset_pct) )
    shape -~ Pen.fill(Radial_Gradient(color_stops, Reflect_Colors))
  }
}

}
