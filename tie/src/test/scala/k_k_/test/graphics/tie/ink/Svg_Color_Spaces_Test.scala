/*
   file: k_k_/test/graphics/tie/ink/Svg_Color_Spaces_Test.scala

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
import Assert._

import k_k_.graphics.tie._
import k_k_.graphics.tie.ink.{Named_Colors => C, _}
import k_k_.graphics.tie.ink.palette._
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.text._


@Test
class Svg_Color_Spaces_Green_Red_Test extends Svg_Color_Spaces_Test_Base {

  val filename = "test_color_spaces[green-red].svg"

  val title = "Color Spaces: RGB lerp v. HSL transition v. HSL hue transition"


  protected def create_canvas() = {
    val (color_a, color_b) = (C.green, C.red)
    val (color_a_name, color_b_name) = (color_a.name, color_b.name)

    val color_a_swatch = swatch_ctor(swatch_w, swatch_h) -~ Pen.fill(color_a)
    val color_b_swatch = swatch_ctor(swatch_w, swatch_h) -~ Pen.fill(color_b)

    val labeled_color_a = label_shape(color_a_swatch, color_a_name)
    val labeled_color_b = label_shape(color_b_swatch, color_b_name)

    val transition_name = color_a_name + " ... " + color_b_name

    val const_L_a_label = transition_name + " [HSL transition hue (constant " +
                            "lightness of '" + color_a_name + "')]"
    val const_L_b_label = transition_name + " [HSL transition hue (constant " +
                            "lightness of '" + color_b_name + "')]"
    val const_L_cmp =
          draw_swatch_cmp(color_a.transition_hues_by(color_b, 1.0 / n_stops),
                          const_L_a_label,
                          color_b.transition_hues_by(color_a, 1.0 / n_stops).
                            reverse,
                          const_L_b_label,
                          labeled_color_a, labeled_color_b)

    val rgb_hsl_cmp = draw_rgb_hsl_cmp(color_a, color_b)

    // NOTE: size of both layouts expected to be identical
    val Rectangular(_, layout_h) = rgb_hsl_cmp.bounding_box

    new Canvas(Canvas_Props(1000, 400, title = title),
               (rgb_hsl_cmp -+ (0, -(layout_h/2 + 30))) -&

               (const_L_cmp -+ (0,  (layout_h/2 + 30)))
              )
  }
}


@Test
class Svg_Color_Spaces_Blue_Orange_Test extends Svg_Color_Spaces_Test_Base {

  val filename = "test_color_spaces[blue-orange].svg"

  val title =
        "Color Spaces: RGB lerp v. HSL transition v. HSL saturation, lightness"


  protected def create_canvas() = {
    val (color_a, color_b) = (C.blue, C.orange)
    val (color_a_name, color_b_name) = (color_a.name, color_b.name)

    val (color_a_sat, color_a_desat) = (color_a.saturate(1.0),
                                        color_a.desaturate(1.0))
    val (color_b_sat, color_b_desat) = (color_b.saturate(1.0),
                                        color_b.desaturate(1.0))
    val desat_cmp =
          draw_swatch_cmp(color_a_sat.transition_saturations_by(color_a_desat,
                                                                1.0 / n_stops),
                            color_a_name + " desaturation",
                          color_b_sat.transition_saturations_by(color_b_desat,
                                                                1.0 / n_stops),
                            color_b_name + " desaturation",
                          Null_Shape, Null_Shape)

    val (color_a_light, color_a_dark) = (color_a.lighten(1.0),
                                         color_a.darken(1.0))
    val (color_b_light, color_b_dark) = (color_b.lighten(1.0),
                                         color_b.darken(1.0))
    val lightness_cmp =
          draw_swatch_cmp(color_a_light.transition_lightnesses_by(color_a_dark,
                                                                 1.0 / n_stops),
                            color_a_name + " lightness",
                          color_b_light.transition_lightnesses_by(color_b_dark,
                                                                 1.0 / n_stops),
                            color_b_name + " lightness",
                          Null_Shape, Null_Shape)

    val rgb_hsl_cmp = draw_rgb_hsl_cmp(color_a, color_b)

    // NOTE: size of both layouts expected to be identical
    val Rectangular(_, layout_h) = rgb_hsl_cmp.bounding_box

    new Canvas(Canvas_Props(1000, 540, title = title),
               (rgb_hsl_cmp   -+ (0, -(layout_h + 30))) -&
               (desat_cmp     -+ (0,  0))                 -&
               (lightness_cmp -+ (0,  (layout_h + 30)))
              )
  }
}


abstract class Svg_Color_Spaces_Test_Base extends Svg_Test_Base {

  val n_stops = 16
  val (swatch_w, swatch_h) = (50, 50)
  val each_swatch_pad_x = 5

  final val (row_w, row_h) = (n_stops*(each_swatch_pad_x + swatch_w), swatch_h)

  val swatch_ctor = Rectangle(_, _)
  // val swatch_ctor = (w: Double, h: Double) => Hexagon(.6*w, w, h)


  @Test
  def test_complement_is_inverse() = {
    for (color <- Rainbow_Palette.colors) {
      // NOTE: due to impl., additionally tests RGB -> HSL -> RGB (re)conversion
      assertEquals(color, color.complement.complement)
    }
  }


  def draw_rgb_hsl_cmp(color_a: Named_Color, color_b: Named_Color):
      Drawing_Shape = {
    val (color_a_name, color_b_name) = (color_a.name, color_b.name)

    val color_a_swatch = swatch_ctor(swatch_w, swatch_h) -~ Pen.fill(color_a)
    val color_b_swatch = swatch_ctor(swatch_w, swatch_h) -~ Pen.fill(color_b)

    val labeled_color_a = label_shape(color_a_swatch, color_a_name)
    val labeled_color_b = label_shape(color_b_swatch, color_b_name)

    val transition_name = color_a_name + " ... " + color_b_name
    draw_swatch_cmp(color_a.lerps_by      (color_b, 1.0 / n_stops),
                      transition_name + " [RGB lerps]",
                    color_a.transitions_by(color_b, 1.0 / n_stops),
                      transition_name + " [HSL transitions]",
                    labeled_color_a, labeled_color_b)
  }

  // horizontally aligned color swatches
  def draw_color_swatch_row(Shape: (Double, Double) => Drawing_Shape)
                           (row_w: Double, row_h: Double, horiz_pad: Double)
                           (colors: Seq[Color]):
      Drawing_Shape = {
    val each_col_w = row_w / colors.length
    val each_swatch_w = each_col_w - horiz_pad
    val swatches = colors map { Shape(each_swatch_w, row_h) -~ Pen.fill(_) }
    val translated_swatches =
          swatches.zipWithIndex.map ( x => x._1 -+ (x._2 * each_col_w, 0) )
    (Null_Shape /: translated_swatches) ( _ -& _ ) -+
      (-row_w/2 + each_col_w/2, 0)
  }

  def draw_swatch_cmp(swatch_x_colors: Seq[Color], swatch_x_label: String,
                      swatch_y_colors: Seq[Color], swatch_y_label: String,
                      open_shape: Drawing_Shape, close_shape: Drawing_Shape):
      Drawing_Shape = {

    val swatch_colors = draw_color_swatch_row(swatch_ctor)(row_w, row_h,
                                                           each_swatch_pad_x) _
    val layout_cmp = layout_swatch_row_cmp(row_w, row_h, each_swatch_pad_x) _


    val x_swatches = swatch_colors(swatch_x_colors)
    val y_swatches = swatch_colors(swatch_y_colors)

    val labeled_x_swatches = label_shape(x_swatches, swatch_x_label)
    val labeled_y_swatches = label_shape(y_swatches, swatch_y_label)

    layout_cmp(open_shape, close_shape, labeled_x_swatches, labeled_y_swatches)
  }

  def layout_swatch_row_cmp(row_w: Double, row_h: Double, horiz_pad: Double)
                           (swatch_a: Drawing_Shape, swatch_b: Drawing_Shape,
                            row_x: Drawing_Shape, row_y: Drawing_Shape):
      Drawing_Shape = {
    // NOTE: size of both swatches expected to be identical
    val Rectangular(swatch_w, _) = swatch_a.bounding_box
    ((swatch_a -+ (-(row_w + swatch_w + horiz_pad)/2, 0)) -&
     (swatch_b -+ ( (row_w + swatch_w + horiz_pad)/2, 0)) -&

     (row_x    -+ (0, -(row_h/2 + 15))) -&
     (row_y    -+ (0,  (row_h/2 + 15))))
  }
}

}
