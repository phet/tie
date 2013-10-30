/*
   file: k_k_/graphics/tie/ink/palette.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2013 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie.ink

package palette {

import k_k_.io.data.StringSeqFromDataFile


trait ColorPalette {

  val colors: IndexedSeq[Color]

  lazy val colorNames: IndexedSeq[String] =
    colors.map { nameColor }

  lazy val everyColorWithName: IndexedSeq[(Color, String)] =
    colors.map { c => (c, nameColor(c)) }


  protected def nameColor(color: Color) =
    color match {
      case DescribedColor(desc)      => desc
      case NamedColor(name, _, _, _) => name
      case c: Color                  => c.asRgbString
    }
}

trait NamedColorPalette {
  val colors: IndexedSeq[NamedColor]
}


abstract class ColorPaletteFromColorNameSeq extends NamedColorPalette {

  override lazy val colors: IndexedSeq[NamedColor] = Vector(
      colorNameSeq.map { NamedColor(_).get } : _*
    )

  protected val colorNameSeq: Seq[String]
}


object AllNamedColors extends ColorPaletteFromColorNameSeq {

  override protected lazy val colorNameSeq = namedColorsData.getSeq

  val pathName = "/data/k_k_.graphics.tie/color/css2_color_names.list"

  private val namedColorsData = new StringSeqFromDataFile(pathName) {
    override protected val loadVia = classOf[Color]
  }
}


object RainbowPalette extends ColorPaletteFromColorNameSeq {

  override protected val colorNameSeq = Seq(
      "red", "orange", "yellow", "green", "blue", "indigo", "violet"
    )
}

}
