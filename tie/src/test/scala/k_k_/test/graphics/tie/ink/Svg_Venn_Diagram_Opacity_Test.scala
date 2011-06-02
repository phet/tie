/*
   file: k_k_/test/graphics/tie/ink/Svg_Venn_Diagram_Opacity_Test.scala

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
class Svg_Venn_Diagram_Opacity_Test extends Svg_Test_Base {

  val filename = "test_venn_diagram_opacity.svg"

  val title = "Tri-Color Venn Diagram Showing Opacity"


  val colors = new Color_Palette_From_Color_Name_Seq {
    protected val color_name_seq =
      Seq("red", "green", "blue")
  }.colors


  protected def create_canvas() = {
    val ellipse_ctor = Diam_Ellipse(_, _)
    val hex_ctor = (w: Double, h: Double) => Hexagon(.6*h, h, w) -% (90)
    val oct_ctor = (w: Double, h: Double) => Octagon(.4*w, w, .5*h, h)
    val pent_ctor = (w: Double, h: Double) => Pentagon(.7*w, w, .6*h, h)
    val trap_ctor = (w: Double, h: Double) => Trapezoid(.4*w, w, h)

    val (each_w, each_h) = (200, 160)

    val venn1 = draw_venn_diagram(pent_ctor)(each_w, each_h)(colors, .15)
    val venn2 = draw_venn_diagram(hex_ctor)(each_w, each_h)(colors, .25)
    val venn3 = draw_venn_diagram(ellipse_ctor)(each_w, each_h)(colors, .5)
    val venn4 = draw_venn_diagram(oct_ctor)(each_w, each_h)(colors, .75)
    val venn5 = draw_venn_diagram(Rectangle(_, _))(each_w, each_h)(colors, .85)
    val venn6 = draw_venn_diagram(trap_ctor)(each_w, each_h)(colors, 2)

    new Canvas(Canvas_Props(700, 450, title = title),
               ((label_shape(venn1, "opacity = .15")
                   -+ (-(each_w + 15), 0)) -&
                (label_shape(venn2, "opacity = .25")) -&
                (label_shape(venn3, "opacity = .5")
                   -+ ((each_w + 15), 0))
                  -+ (0, -(each_h/2 + 20))) -&

               ((label_shape(venn4, "opacity = .75")
                   -+ (-(each_w + 15), 0)) -&
                (label_shape(venn5, "opacity = .85")) -&
                (label_shape(venn6, "opacity = 1.0")
                   -+ (each_w + 15, 0))
                  -+ (0, (each_h/2 + 20)))
              )
  }

  // triple-overlapping venn diagram
  def draw_venn_diagram(Shape: (Double, Double) => Drawing_Shape)
                       (w: Double, h: Double)
                       (colors: Seq[Color], opacity: Double):
      Drawing_Shape = {
    val each_shape_w = w * 2/3
    val each_shape_h = h * 2/3
    val diagram_colors = colors.take(3)
    val colored_shapes = diagram_colors.map( _ -# opacity ).
                                        map( Shape(each_shape_w, each_shape_h)
                                             -~ Pen.fill(_) )
    val (x_shift, y_shift) = (w/2 - w/3,
                              h/2 - h/3)
    (colored_shapes(0) -+ (0, -y_shift)) -&
    (colored_shapes(1) -+ (-x_shift, y_shift)) -&
    (colored_shapes(2) -+ (x_shift, y_shift))
  }
}

}
