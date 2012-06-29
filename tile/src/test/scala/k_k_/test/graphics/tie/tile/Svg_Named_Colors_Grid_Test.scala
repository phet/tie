/*
   file: k_k_/test/graphics/tie/tile/Svg_Named_Colors_Grid_Test.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.test.graphics.tie.tile

import org.junit._
import Assert._

import k_k_.graphics.tie._
import k_k_.graphics.tie.ink.{Named_Colors => C, _}
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.text._

import k_k_.graphics.tie.tile._
import k_k_.graphics.tie.tile.conversions._


@Test
class Svg_Named_Colors_Grid_Test extends Svg_Test_Base {

  val filename = "test_all_named_colors.svg"

  val title = "All Named Colors (Clustered)"


  val color_names_fpath =
      "/data/k_k_.graphics.tie/color/clustered_color_names.list"
  // val named_colors = palette.All_Named_Colors.colors
  val named_colors = new palette.Color_Palette_From_Color_Name_Seq {

    import k_k_.io.data.String_Seq_From_Data_File

    protected lazy val color_name_seq = named_colors_data.get_seq

    private val named_colors_data =
        new String_Seq_From_Data_File(color_names_fpath) {
          override protected val load_via = classOf[Svg_Named_Colors_Grid_Test]
        }
  }.colors



  protected def create_canvas() = {
    val bg_pen = Pen.fill(C.Black)
    val color_tiles = render_colors(named_colors)//.over_bounding_box(bg_pen)

    new Canvas(Canvas_Props(1300, 680, title = title),
               color_tiles -+@ (0, 0)
              )
  }

  protected def render_colors(colors: Seq[Named_Color]): Shape = {
    val (horiz_pad, vert_pad) = (2.5, 2.5)
    val (n, n_groups) = (colors.length, 14)
    val group_size = n / n_groups + (if (n % n_groups == 0) 0 else 1)
    val color_rows_it = colors.map( render_color(_) ).
                               scale_up_to_uniform.toIterable.
    		               grouped(group_size)
    color_rows_it.map { color_row =>
      color_row.map( _.pad(Center, horiz_pad, vert_pad) ).join(R_Mid)
    }.join(Bottom_Middle)
  }

  protected def render_color(color: Named_Color): Shape = {
    val name_font = Font("Arial", 10)
    val text_colors = Seq(C.White, C.Black)
    val name_caption = text_colors.map( Text_Line(color.name, name_font, 1.1) -~
    		       			  Pen.fill(_) ).
                                   join(B_Mid).pad(10) -+@ (0, 0)
    val Rectangular(w, h) = name_caption.bounding_box
    Octagon(.8*w, w, .8*h, h) -~ Pen(C.black, color) -& name_caption
  }
}
