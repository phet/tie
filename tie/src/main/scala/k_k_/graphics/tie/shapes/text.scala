/*
   file: k_k_/graphics/tie/shapes/text.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie.shapes


package object text {

  val Default_Font = Font("Arial", 12)

  // Text_Direction aliases:
  val L_To_R = Left_To_Right
  val R_To_L = Right_To_Left

  // Writing_Mode aliases:
  val LR_TB = Left_Right__Top_Bottom
  val RL_TB = Right_Left__Top_Bottom
  val TB_RL = Top_Bottom__Right_Left
}


package text {


import java.util.concurrent.ConcurrentHashMap

import scala.collection.mutable.ConcurrentMap
import scala.collection.JavaConversions._

import k_k_.io.data.Char_Double_Seq_From_Data_File
import k_k_.fs.Resources


object Font_Size {

  implicit def from_Double(num: Double): Font_Size =
    Std_Size(num)
}

//!!!!!!!in actuality, these sizes are applicable to any 'length' (see:
// http://www.w3.org/TR/SVG/coords.html#UnitIdentifiers)!!!!!!!!!!
sealed abstract class Font_Size {

  def scale(factor: Double): Font_Size

  def *(scale_factor: Double) =
    scale(scale_factor)
}

case class Std_Size(num: Double) extends Font_Size {

  def scale(factor: Double) =
    Std_Size(factor * num)
}
// case class Pct_Size(percentage: Double) extends Font_Size
// case class Em_Size(num: Double)         extends Font_Size
// case class Ex_Size(num: Double)         extends Font_Size

sealed abstract class Font_Style
case object Plain   extends Font_Style
case object Italic  extends Font_Style
case object Oblique extends Font_Style

sealed abstract class Font_Weight(val css2_equiv: Int)
case object Lightest      extends Font_Weight(100)
case object Light         extends Font_Weight(200)
case object Little_Light  extends Font_Weight(300)
case object Normal        extends Font_Weight(400)
case object Little_Bold   extends Font_Weight(500)
case object Nearly_Bold   extends Font_Weight(600)
case object Bold          extends Font_Weight(700)
case object Bolder        extends Font_Weight(800)
case object Boldest       extends Font_Weight(900)


object Font {

  import Font_Size._

  val default_font_style  = Plain
  val default_font_weight = Normal

  def apply(family: String, size_pts: Double,
            style: Font_Style, weight: Font_Weight) =
    new Font(family, size_pts, style, weight)

  def apply(family: String, size_pts: Double,
            style: Font_Style) =
    new Font(family, size_pts, style)

  def apply(family: String, size_pts: Double,
            weight: Font_Weight) =
    new Font(family, size_pts, weight = weight)

  def apply(family: String, size_pts: Double) =
    new Font(family, size_pts)
}

final case class Font(family: String,
                      size: Font_Size,
                      style: Font_Style   = Font.default_font_style,
                      weight: Font_Weight = Font.default_font_weight) {

  def scale(factor: Double): Font =
    copy(size = size scale factor)

  def *(scale_factor: Double) =
    scale(scale_factor)
}


sealed abstract class Baseline_Shift
case object Above_Baseline extends Baseline_Shift
case object Below_Baseline extends Baseline_Shift


sealed abstract class Text_Decoration
case object No_Decoration extends Text_Decoration
case object Underline     extends Text_Decoration
case object Overline      extends Text_Decoration
case object Strike        extends Text_Decoration
case object Blinking      extends Text_Decoration


sealed abstract class Text_Align
case object Start_Align  extends Text_Align
case object Middle_Align extends Text_Align
case object End_Align    extends Text_Align


sealed abstract class Text_Direction
case object Left_To_Right extends Text_Direction
case object Right_To_Left extends Text_Direction


sealed abstract class Orientation
case object Horizontal extends Orientation
case object Vertical   extends Orientation

sealed abstract class Writing_Mode { val orientation: Orientation }
case object Left_Right__Top_Bottom extends Writing_Mode {
  val orientation = Horizontal
}
case object Right_Left__Top_Bottom extends Writing_Mode {
  val orientation = Horizontal
}
case object Top_Bottom__Right_Left extends Writing_Mode {
  val orientation = Vertical
}


trait Text_Ruler {
  def measure(s: String, compress_spaces_? : Boolean): Dims
}

trait Text_Ruler_Factory {
  def create(font: Font): Text_Ruler
}


object Text {

  implicit def to_Writing(text: Text) =
    Writing(text)
}

sealed abstract class Text {

  def scale(factor: Double): Text

  def *(scale_factor: Double) =
    scale(scale_factor)

  def text_bounding_box(ruler_factory: Text_Ruler_Factory): Dims
}


trait Text_Bounding_Box_Memoization { self: Text =>

  //!!!!!!!verify that the definition of equals on ConcurrentHashMap (held by Memoizing_Text_Ruler_Factory) would play well with modifications due to interceding forget_all()!!!!!!!!!

  def text_bounding_box(ruler_factory: Text_Ruler_Factory): Dims =
    last_bb_calc match {
      case Some((prev_rlr_factory,bbox)) if prev_rlr_factory == ruler_factory =>
        bbox
      case _ =>
        val bbox = calc_text_bounding_box(ruler_factory)
        last_bb_calc = Some(ruler_factory, bbox)
        bbox
    }


  protected def calc_text_bounding_box(ruler_factory: Text_Ruler_Factory): Dims


  @volatile
  private[this] var last_bb_calc: Option[(Text_Ruler_Factory, Dims)] = None
}


object Text_Span {

  val default_text_decoration = No_Decoration


  def apply(text: String, font: Font,
            decor: Text_Decoration = Text_Span.default_text_decoration,
            compress_spaces_? : Boolean = true): Text_Span =
    Basic_Text_Span(text, font, decor, compress_spaces_?)

  def unapply(span: Text_Span):
      Option[(String, Font, Text_Decoration, Boolean)] =
    Some(span.text, span.font, span.decor, span.compress_spaces_?)
}

sealed abstract class Text_Span extends Text {

  def scale(factor: Double): Text_Span

  override // ...to return covariant return type
  def *(scale_factor: Double): Text_Span =
    scale(scale_factor)

  def unary_+ : Text_Span =
    Baseline_Shifted_Text_Span(this, Above_Baseline)

  def unary_- : Text_Span =
    Baseline_Shifted_Text_Span(this, Below_Baseline)

  def text: String
  def font: Font
  def decor: Text_Decoration
  def compress_spaces_? : Boolean

  def +:(str: String): Text_Span

  def prepend(str: String): Text_Span =
    +:(str)

  def ++(str: String): Text_Span

  def append(str: String): Text_Span =
    ++(str)


  def as_line: Text_Line =
    Text_Line(this)
}

final case class Basic_Text_Span(text: String,
                                 font: Font,
                                 decor: Text_Decoration =
                                   Text_Span.default_text_decoration,
                                 compress_spaces_? : Boolean = true)
    extends Text_Span with Text_Bounding_Box_Memoization {

  def +:(str: String): Text_Span =
    copy(text = str + text)

  def ++(str: String): Text_Span =
    copy(text = text + str)


  def scale(factor: Double): Text_Span =
    copy(font = font scale factor)

  protected def calc_text_bounding_box(ruler_factory: Text_Ruler_Factory):
      Dims = {
    val ruler = ruler_factory.create(font)
    ruler.measure(text, compress_spaces_?)
  }
}

final case class Baseline_Shifted_Text_Span(span: Text_Span,
                                            shift: Baseline_Shift)
    extends Text_Span with Text_Bounding_Box_Memoization {

  def +:(str: String): Text_Span =
    copy(span = str +: span)

  def ++(str: String): Text_Span =
    copy(span = span ++ str)


  def scale(factor: Double): Text_Span =
    copy(span = span scale factor)

  def text: String                = span.text
  def font: Font                  = span.font
  def decor: Text_Decoration      = span.decor
  def compress_spaces_? : Boolean = span.compress_spaces_?


  protected def calc_text_bounding_box(ruler_factory: Text_Ruler_Factory):
      Dims = {
    def calc_font_height(font: Font): Double =
      font.size match {
        case Std_Size(num) => num
      }
    span.text_bounding_box(ruler_factory) move (shift match {
      // NOTE: use 1/4 of font height as an approximation of shift
      case Above_Baseline => Point(0, -calc_font_height(font) / 4)
      case Below_Baseline => Point(0,  calc_font_height(font) / 4)
    } )
  }
}


trait Dims_Manipulation { self: Text =>

  val null_rect = Origin_Dims(0, 0)

  protected def sum_width__max_height(r1: Origin_Dims, r2: Dims) =
    Origin_Dims(r1.width + r2.width, math.max(r1.height, r2.height))

  protected def max_width__sum_height(r1: Origin_Dims, r2: Dims) =
    Origin_Dims(math.max(r1.width, r2.width), r1.height + r2.height)

  protected def quarter_turn(r: Origin_Dims) =
    Origin_Dims(r.height, r.width)

  protected def identity(r: Origin_Dims) =
    r

  protected def scale_width(r: Dims, s: Double) =
    Origin_Dims(r.width * s, r.height)

  protected def scale_height(r: Dims, s: Double) =
    Origin_Dims(r.width, r.height * s)
}


object Text_Line {

  val default_breadth    = 1.25
  val default_text_align = Start_Align
  val default_text_dir   = Left_To_Right

  def apply(content: Text_Span,
            breadth: Double,
            align: Text_Align,
            dir: Text_Direction): Text_Line =
    Text_Line(List(content), breadth, align, dir)


  def apply(content: Text_Span): Text_Line =
    Text_Line(List(content))

  def apply(content: Text_Span,
            breadth: Double): Text_Line =
    Text_Line(List(content), breadth)

  def apply(content: Text_Span,
            align: Text_Align): Text_Line =
    Text_Line(List(content), align = align)

  def apply(content: Text_Span,
            breadth: Double,
            align: Text_Align): Text_Line =
    Text_Line(List(content), breadth, align)


  def apply(text: String, font: Font,
            breadth: Double,
            align: Text_Align,
            dir: Text_Direction): Text_Line =
    Text_Line(List(Text_Span(text, font)), breadth, align, dir)

  def apply(text: String, font: Font,
            compress_spaces_? : Boolean,
            breadth: Double,
            align: Text_Align,
            dir: Text_Direction): Text_Line =
    Text_Line(List(Text_Span(text, font, compress_spaces_? =compress_spaces_?)),
              breadth, align, dir)

  def apply(text: String, font: Font,
            decor: Text_Decoration,
            breadth: Double,
            align: Text_Align,
            dir: Text_Direction): Text_Line =
    Text_Line(List(Text_Span(text, font, decor)), breadth, align, dir)

  def apply(text: String, font: Font,
            decor: Text_Decoration,
            compress_spaces_? : Boolean,
            breadth: Double,
            align: Text_Align,
            dir: Text_Direction): Text_Line =
    Text_Line(List(Text_Span(text, font, decor, compress_spaces_?)),
              breadth, align, dir)


  def apply(text: String, font: Font,
            breadth: Double,
            align: Text_Align): Text_Line =
    Text_Line(List(Text_Span(text, font)), breadth, align)

  def apply(text: String, font: Font,
            compress_spaces_? : Boolean,
            breadth: Double,
            align: Text_Align): Text_Line =
    Text_Line(List(Text_Span(text, font, compress_spaces_? =compress_spaces_?)),
              breadth, align)

  def apply(text: String, font: Font,
            decor: Text_Decoration,
            breadth: Double,
            align: Text_Align): Text_Line =
    Text_Line(List(Text_Span(text, font, decor)), breadth, align)

  def apply(text: String, font: Font,
            decor: Text_Decoration,
            compress_spaces_? : Boolean,
            breadth: Double,
            align: Text_Align): Text_Line =
    Text_Line(List(Text_Span(text, font, decor, compress_spaces_?)),
              breadth, align)


  def apply(text: String, font: Font,
            align: Text_Align): Text_Line =
    Text_Line(List(Text_Span(text, font)), align = align)

  def apply(text: String, font: Font,
            compress_spaces_? : Boolean,
            align: Text_Align): Text_Line =
    Text_Line(List(Text_Span(text, font, compress_spaces_? =compress_spaces_?)),
              align = align)

  def apply(text: String, font: Font,
            decor: Text_Decoration,
            align: Text_Align): Text_Line =
    Text_Line(List(Text_Span(text, font, decor)), align = align)

  def apply(text: String, font: Font,
            decor: Text_Decoration,
            compress_spaces_? : Boolean,
            align: Text_Align): Text_Line =
    Text_Line(List(Text_Span(text, font, decor, compress_spaces_?)),
              align = align)


  def apply(text: String, font: Font,
            breadth: Double): Text_Line =
    Text_Line(List(Text_Span(text, font)), breadth)

  def apply(text: String, font: Font,
            compress_spaces_? : Boolean,
            breadth: Double): Text_Line =
    Text_Line(List(Text_Span(text, font, compress_spaces_? =compress_spaces_?)),
              breadth)

  def apply(text: String, font: Font,
            decor: Text_Decoration,
            breadth: Double): Text_Line =
    Text_Line(List(Text_Span(text, font, decor)), breadth)

  def apply(text: String, font: Font,
            decor: Text_Decoration,
            compress_spaces_? : Boolean,
            breadth: Double): Text_Line =
    Text_Line(List(Text_Span(text, font, decor, compress_spaces_?)), breadth)


  def apply(text: String, font: Font): Text_Line =
    Text_Line(List(Text_Span(text, font)))

  def apply(text: String, font: Font,
            compress_spaces_? : Boolean): Text_Line =
    Text_Line(List(Text_Span(text, font, compress_spaces_? =compress_spaces_?)))

  def apply(text: String, font: Font,
            decor: Text_Decoration): Text_Line =
    Text_Line(List(Text_Span(text, font, decor)))

  def apply(text: String, font: Font,
            decor: Text_Decoration,
            compress_spaces_? : Boolean): Text_Line =
    Text_Line(List(Text_Span(text, font, decor, compress_spaces_?)))
}

case class Text_Line(content: List[Text_Span],
                     breadth: Double     = Text_Line.default_breadth,
                     align: Text_Align   = Text_Line.default_text_align,
                     dir: Text_Direction = Text_Line.default_text_dir)
    extends Text with Text_Bounding_Box_Memoization
       with Dims_Manipulation {

  def ::(span: Text_Span): Text_Line =
    copy(content = span :: content)

  def prepend(span: Text_Span): Text_Line =
    ::(span)

  def :::(spans: List[Text_Span]): Text_Line =
    copy(content = spans ::: content)

  def concat(spans: List[Text_Span]): Text_Line =
    :::(spans)


  def scale(factor: Double): Text_Line =
    copy(content = content map ( _ scale factor ))

  override // ...to return covariant return type
  def *(scale_factor: Double): Text_Line =
    scale(scale_factor)

  def text_bounding_box(ruler_factory: Text_Ruler_Factory,
                        enclosing_text_block_mode: Writing_Mode):
      Dims = {
    val horiz_flow = (max_width__sum_height _, scale_height _, identity     _)
    val vert_flow  = (sum_width__max_height _, scale_width  _, quarter_turn _)

    val block_orientation = enclosing_text_block_mode.orientation
    val (combine, scale_line, final_adjustment) = block_orientation match {
      case Horizontal => horiz_flow
      case Vertical   => vert_flow
    }
    val bbox = (null_rect /: content.map( _.text_bounding_box(ruler_factory) )){
                    (sum, r) => combine(sum, scale_line(r, breadth)) }
    final_adjustment(bbox)
  }


  def as_block: Text_Block =
    Text_Block(this)


  protected def calc_text_bounding_box(ruler_factory: Text_Ruler_Factory):
      Dims =
    text_bounding_box(ruler_factory, Text_Block.default_writing_mode)
}


object Text_Block {

  val default_writing_mode = LR_TB


  def apply(content: Text_Line): Text_Block =
    Text_Block(List(content))

  def apply(content: Text_Line, mode: Writing_Mode): Text_Block =
    Text_Block(List(content), mode)


  def apply(text_line_strs: List[String], font: Font): Text_Block =
    Text_Block(text_line_strs map { Text_Line(_, font) })

  def apply(text_line_str: String, font: Font): Text_Block =
    apply(List(text_line_str), font)

  def apply(text_line_strs: List[String], font: Font,
            compress_spaces_? : Boolean): Text_Block =
    Text_Block(text_line_strs map { Text_Line(_, font, compress_spaces_?) })

  def apply(text_line_str: String, font: Font,
            compress_spaces_? : Boolean): Text_Block =
    apply(List(text_line_str), font, compress_spaces_?)


  def apply(text_line_strs: List[String], font: Font,
            mode: Writing_Mode): Text_Block =
    Text_Block(text_line_strs map { Text_Line(_, font) }, mode)

  def apply(text_line_str: String, font: Font, mode: Writing_Mode): Text_Block =
    apply(List(text_line_str), font, mode)

  def apply(text_line_strs: List[String], font: Font,
            compress_spaces_? : Boolean,
            mode: Writing_Mode): Text_Block =
    Text_Block(text_line_strs map {Text_Line(_, font, compress_spaces_?)}, mode)

  def apply(text_line_str: String, font: Font,
            compress_spaces_? : Boolean, mode: Writing_Mode): Text_Block =
    apply(List(text_line_str), font, compress_spaces_?, mode)


  def apply(text_line_strs: List[String], font: Font,
            breadth: Double,
            mode: Writing_Mode): Text_Block =
    Text_Block(text_line_strs map { Text_Line(_, font, breadth) }, mode)

  def apply(text_line_str: String, font: Font,
            breadth: Double,
            mode: Writing_Mode): Text_Block =
    apply(List(text_line_str), font, breadth, mode)

  def apply(text_line_strs: List[String], font: Font,
            compress_spaces_? : Boolean,
            breadth: Double,
            mode: Writing_Mode): Text_Block =
    Text_Block(text_line_strs map
                 { Text_Line(_, font, compress_spaces_?, breadth) },
               mode)

  def apply(text_line_str: String, font: Font,
            compress_spaces_? : Boolean,
            breadth: Double,
            mode: Writing_Mode): Text_Block =
    apply(List(text_line_str), font, compress_spaces_?, breadth, mode)


  def apply(text_line_strs: List[String], font: Font,
            breadth: Double):
      Text_Block =
    Text_Block(text_line_strs map { Text_Line(_, font, breadth) })

  def apply(text_line_str: String, font: Font, breadth: Double): Text_Block =
    apply(List(text_line_str), font, breadth)

  def apply(text_line_strs: List[String], font: Font,
            compress_spaces_? : Boolean,
            breadth: Double):
      Text_Block =
    Text_Block(text_line_strs map
                 { Text_Line(_, font, compress_spaces_?, breadth) })

  def apply(text_line_str: String, font: Font,
            compress_spaces_? : Boolean,
            breadth: Double): Text_Block =
    apply(List(text_line_str), font, compress_spaces_?, breadth)
}

case class Text_Block(content: List[Text_Line],
                      mode: Writing_Mode = Text_Block.default_writing_mode)
    extends Text with Text_Bounding_Box_Memoization
       with Dims_Manipulation {

  def ::(line: Text_Line): Text_Block =
    copy(content = line :: content)

  def prepend(line: Text_Line): Text_Block =
    ::(line)

  def :::(lines: List[Text_Line]): Text_Block =
    copy(content = lines ::: content)

  def concat(lines: List[Text_Line]): Text_Block =
    :::(lines)


  def scale(factor: Double): Text_Block =
    copy(content = content map ( _ scale factor ))

  override // ...to return covariant return type
  def *(scale_factor: Double): Text_Block =
    scale(scale_factor)

  protected def calc_text_bounding_box(ruler_factory: Text_Ruler_Factory):
      Dims = {
    val horiz_flow  = (max_width__sum_height _, identity     _)
    val vert_flow   = (sum_width__max_height _, quarter_turn _)

    val (combine, final_adjustment) = mode.orientation match {
      case Horizontal => horiz_flow
      case Vertical   => vert_flow
    }
    val bbox = (null_rect /: content.map( _.text_bounding_box(ruler_factory,
                                                              mode) )) {
                 combine(_, _) }
    final_adjustment(bbox)
  }
}


trait Memoizing_Text_Ruler_Factory { self: Text_Ruler_Factory =>

  def create(font: Font): Text_Ruler =
    recall.getOrElseUpdate(font, do_create(font))

  def forget_all {
    recall.clear()
  }

  protected def do_create(font: Font): Text_Ruler

  private val recall: ConcurrentMap[Font, Text_Ruler] =
    new ConcurrentHashMap[Font, Text_Ruler]
}


trait Char_Filtering_Text_Ruler { self: Text_Ruler =>
  def filter_chars(s: String, compress_spaces_? : Boolean): String
}

trait Space_Compressable_Filtering_Text_Ruler
    extends Char_Filtering_Text_Ruler { self: Text_Ruler =>

  def filter_chars(s: String, compress_spaces_? : Boolean): String =
    if (compress_spaces_?) s.replaceAll("\\s+", " ")
    else s
}


trait Char_Measurements_Source {

  def char_widths: Seq[(Char, Double)]
}


trait Fallback_To_Avg_Text_Ruler
    extends Text_Ruler
       with Char_Filtering_Text_Ruler { self: Char_Measurements_Source =>

  def measure(s: String, compress_spaces_? : Boolean): Dims =
    Origin_Dims(measure_width(filter_chars(s, compress_spaces_?)),
                uniform_char_height)


  protected def measure_width(s: String): Double =
    // scale entire measurement--not individual chars--to minimize rounding err
    scale((0.0 /: s)( _ + calc_width(_) ))

  protected def calc_width(c: Char): Double =
    get_char_width(c).getOrElse(avg_char_width)

  // NOTE: all measurements shall be of 10pt fonts:
  protected def scale(measure: Double): Double =
    (measure / 10.0) * uniform_char_height


  protected def get_char_width(c: Char): Option[Double] =
    // kludge!--should really be given by `def char_widths: Seq[(Char, Double)]`
    char_measurements.get(c).orElse( if (c == '\t' || c == '\n')
                                       char_measurements.get(' ')
                                     else None )

  protected lazy val avg_char_width: Double =
    if (char_measurements.size == 0) 5.2
    else char_measurements.values.sum / char_measurements.size

  protected val uniform_char_height: Double


  protected lazy val char_measurements: Map[Char, Double] =
    Map(char_widths : _*)
}


trait Loadable_Char_Measurements extends Char_Measurements_Source {

  val load_dir: String

  val font: Font

  def char_widths: Seq[(Char, Double)] =
    for (font_measure_fpath <-
           Resources.matching(calc_measure_fpath_prefix(font, load_dir),
                              classOf[Font]);
         measure_seq <-
           new Char_Double_Seq_From_Data_File(font_measure_fpath) {
             override protected val load_via = classOf[Font]
           }.get_seq)
      yield measure_seq

  protected def calc_measure_fpath_prefix(font: Font, path_dir_base: String):
      String =
    calc_measure_dir(font, path_dir_base) + "/" + calc_measure_fname_base(font)

  protected def calc_measure_fname_base(font: Font): String = {
    val family_name = normalize_font_family(font.family)
    val font_weight = font.weight.css2_equiv
    "w" + font_weight + ".measure."
  }

  protected def calc_measure_dir(font: Font, path_dir_base: String): String =
    path_dir_base + "/" + normalize_font_family(font.family)

  protected def normalize_font_family(name: String) =
    name.toLowerCase.replaceAll("\\s+", "_")
}

object Default_Text_Ruler_Factory
    extends Text_Ruler_Factory with Memoizing_Text_Ruler_Factory {

  protected def do_create(font0: Font): Text_Ruler =
    new Fallback_To_Avg_Text_Ruler
        with Space_Compressable_Filtering_Text_Ruler
        with Loadable_Char_Measurements {

      val load_dir = "/data/k_k_.graphics.tie/font/measure"

      val font = font0

      // WARNING: must preceede use to avoid nasty silent init to 0.0 bug!
      val font_pts = font.size match {
        case Std_Size(num) => num
      }

      protected val uniform_char_height: Double = font_pts
    }
}

}
