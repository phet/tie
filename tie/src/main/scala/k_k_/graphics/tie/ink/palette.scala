/*
   file: k_k_/graphics/tie/ink/palette.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie.ink

package palette {

import k_k_.io.data.String_Seq_From_Data_File


trait Color_Palette {

  val colors: IndexedSeq[Color]

  lazy val color_names: IndexedSeq[String] =
    colors.map( name_color(_) )

  lazy val every_color_with_name: IndexedSeq[(Color, String)] =
    colors.map( c => (c, name_color(c)) )


  protected def name_color(color: Color) =
    color match {
      case Described_Color(desc)      => desc
      case Named_Color(name, _, _, _) => name
      case c: Color                   => c.as_rgb_string
    }
}

trait Named_Color_Palette {

  val colors: IndexedSeq[Named_Color]
}


abstract class Color_Palette_From_Color_Name_Seq extends Named_Color_Palette {

  lazy val colors: IndexedSeq[Named_Color] =
      Vector( color_name_seq.map( Named_Color(_).get ) : _* )

  protected val color_name_seq: Seq[String]
}


object All_Named_Colors extends Color_Palette_From_Color_Name_Seq {

  protected lazy val color_name_seq = named_colors_data.get_seq

  val path_name = "/data/k_k_.graphics.tie/color/css2_color_names.list"

  private val named_colors_data = new String_Seq_From_Data_File(path_name) {

    override protected val load_via = classOf[Color]
  }
}


object Rainbow_Palette extends Color_Palette_From_Color_Name_Seq {

  protected val color_name_seq =
    Seq("red", "orange", "yellow", "green", "blue", "indigo", "violet")
}

}
