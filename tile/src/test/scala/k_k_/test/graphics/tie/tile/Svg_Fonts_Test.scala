/*
   file: k_k_/test/graphics/tie/tile/Svg_Fonts_Test.scala

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

import k_k_.graphics.tie._
import k_k_.graphics.tie.ink.{Named_Colors => C, _}
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.text._

import k_k_.graphics.tie.tile._
import k_k_.graphics.tie.tile.conversions._


@Test
class Svg_Fonts_Test extends Svg_Test_Base {

  val filename = "test_font_options.svg"

  val title = "Font Options"


  protected def create_canvas() = {

    def in_every_style(font: Font): Seq[Font] =
      Seq(font.copy(style = Plain),
          font.copy(style = Italic),
          font.copy(style = Oblique))

    def in_every_weight(font: Font): Seq[Font] =
      Seq(Lightest, Light, Little_Light,
          Normal,
          Little_Bold, Nearly_Bold, Bold,
          Bolder, Boldest).map( wt => font.copy(weight = wt) )


    val arial_fonts = Seq(Font("Arial", 10),
                          Font("Arial", 12),
                          Font("Arial", 18)
                         ).flatMap( in_every_style(_) )

    val times_fonts = Seq(Font("Times New Roman", 10),
                          Font("Times New Roman", 12),
                          Font("Times New Roman", 18)
                         ).flatMap( in_every_style(_) )

    val bold_fonts = in_every_weight(arial_fonts(3))

    val font_groups = Seq(arial_fonts, times_fonts.reverse, bold_fonts).
                        map( render_fonts(_) ).
                        map( _.pad(5, 0) )

    new Canvas(Canvas_Props(820, 450, title = title),
               font_groups.take(2).chain(R_Mid).pad(0, 20).at(B_Mid).
                 combo(Top_Middle of font_groups(2).pad(0, 20))
               -@ (0, 0)
              )
  }

  protected def render_fonts(fonts: Seq[Font]): Shape = {
    val (horiz_pad, vert_pad) = (2.5, 2.5)
    val (n, n_groups) = (fonts.length, 3)
    val group_size = n / n_groups + (if (n % n_groups == 0) 0 else 1)
    val font_groups_it = fonts.map( render_font(_) ).
                               grouped(group_size)
    font_groups_it.map { font_group =>
      font_group.map( _.pad(Center, horiz_pad, vert_pad) ).chain(Bottom_Middle)
    }.chain(B_Mid)
  }

  protected def render_font(font: Font): Shape = {
    Text_Line(name_font(font), font) -~ Default_Writing_Pen
  }

  protected def name_font(font: Font): String =
    font.toString
}
