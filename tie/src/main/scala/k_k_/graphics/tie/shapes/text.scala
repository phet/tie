/*
   file: k_k_/graphics/tie/shapes/text.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2013 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie.shapes

package text {

import java.util.concurrent.ConcurrentHashMap

import scala.collection.concurrent.{Map => ConcurrentMap}
import scala.collection.JavaConversions._

import k_k_.io.data.CharDoubleSeqFromDataFile
import k_k_.fs.Resources


//!!!!!!!in actuality, these sizes are applicable to any 'length' (see:
// http://www.w3.org/TR/SVG/coords.html#UnitIdentifiers)!!!!!!!!!!
sealed abstract class FontSize {
  def scale(factor: Double): FontSize
  def *(scaleFactor: Double) = scale(scaleFactor)
}

object FontSize {
  implicit def fromDouble(num: Double): FontSize = Std(num)

  case class Std(num: Double) extends FontSize {
    override def scale(factor: Double) = Std(factor * num)
  }
  // case class Pct(percentage: Double) extends FontSize
  // case class Em(num: Double)         extends FontSize
  // case class Ex(num: Double)         extends FontSize
}


sealed abstract class FontStyle
object FontStyle {
  case object Plain   extends FontStyle
  case object Italic  extends FontStyle
  case object Oblique extends FontStyle
}

sealed abstract class FontWeight(val css2Equiv: Int)
object FontWeight {
  case object Lightest    extends FontWeight(100)
  case object Light       extends FontWeight(200)
  case object LittleLight extends FontWeight(300)
  case object Normal      extends FontWeight(400)
  case object LittleBold  extends FontWeight(500)
  case object NearlyBold  extends FontWeight(600)
  case object Bold        extends FontWeight(700)
  case object Bolder      extends FontWeight(800)
  case object Boldest     extends FontWeight(900)
}


object Font {
  import FontSize._

  val defaultFontStyle  = FontStyle.Plain
  val defaultFontWeight = FontWeight.Normal

  val Default = Font("Arial", 12)

  def apply(
      family: String,
      sizePts: Double,
      style: FontStyle,
      weight: FontWeight
    ) =
    new Font(family, sizePts, style, weight)

  def apply(family: String, sizePts: Double, style: FontStyle) =
    new Font(family, sizePts, style)

  def apply(family: String, sizePts: Double, weight: FontWeight) =
    new Font(family, sizePts, weight = weight)

  def apply(family: String, sizePts: Double) = new Font(family, sizePts)
}

case class Font(
   family: String,
   size: FontSize,
   style: FontStyle   = Font.defaultFontStyle,
   weight: FontWeight = Font.defaultFontWeight
  ) {
  def scale(factor: Double): Font = copy(size = size scale factor)
  def *(scaleFactor: Double) = scale(scaleFactor)
}


sealed abstract class BaselineShift
object BaselineShift {
  case object Above extends BaselineShift
  case object Below extends BaselineShift
}


sealed abstract class TextDecoration
object TextDecoration {
  case object Undecorated extends TextDecoration
  case object Underline   extends TextDecoration
  case object Overline    extends TextDecoration
  case object Strike      extends TextDecoration
  case object Blinking    extends TextDecoration
}


sealed abstract class TextAlign
object TextAlign {
  case object Start  extends TextAlign
  case object Middle extends TextAlign
  case object End    extends TextAlign
}


sealed abstract class TextDirection
object TextDirection {
  case object LeftToRight extends TextDirection
  case object RightToLeft extends TextDirection

  // aliases:
  val LtoR = LeftToRight
  val RtoL = RightToLeft
}


sealed abstract class Orientation
object Orientation {
  case object Horizontal extends Orientation
  case object Vertical   extends Orientation
}


sealed abstract class WritingMode { val orientation: Orientation }
object WritingMode {
  case object LeftRight_TopBottom extends WritingMode {
    override val orientation = Orientation.Horizontal
  }
  case object RightLeft_TopBottom extends WritingMode {
    override val orientation = Orientation.Horizontal
  }
  case object TopBottom_RightLeft extends WritingMode {
    override val orientation = Orientation.Vertical
  }

  // aliases:
  val LR_TB = LeftRight_TopBottom
  val RL_TB = RightLeft_TopBottom
  val TB_RL = TopBottom_RightLeft
}


trait TextRuler {
  def measure(s: String, compressSpaces : Boolean): Dims
}

trait TextRulerFactory {
  def create(font: Font): TextRuler
}


object Text {
  implicit def toWriting(text: Text) = Writing(text)
}

sealed abstract class Text {
  def scale(factor: Double): Text

  def *(scaleFactor: Double) = scale(scaleFactor)

  def textBoundingBox(rulerFactory: TextRulerFactory): Dims
}


trait TextBoundingBoxMemoization { self: Text =>

  //!!!!!!!verify that the definition of equals on ConcurrentHashMap (held by MemoizingTextRulerFactory) would play well with modifications due to interceding forgetAll()!!!!!!!!!

  override def textBoundingBox(rulerFactory: TextRulerFactory): Dims =
    lastBbCalc match {
      case Some((prevRlrFactory, bbox)) if prevRlrFactory == rulerFactory =>
        bbox
      case _ =>
        val bbox = calcTextBoundingBox(rulerFactory)
        lastBbCalc = Some(rulerFactory, bbox)
        bbox
    }


  protected def calcTextBoundingBox(rulerFactory: TextRulerFactory): Dims


  @volatile
  private[this] var lastBbCalc: Option[(TextRulerFactory, Dims)] = None
}


object TextSpan {
  val defaultTextDecoration = TextDecoration.Undecorated

  def apply(
      text: String,
      font: Font,
      decor: TextDecoration = TextSpan.defaultTextDecoration,
      compressSpaces: Boolean = true
    ): TextSpan =
    BasicTextSpan(text, font, decor, compressSpaces)

  def unapply(span: TextSpan): Option[(String, Font, TextDecoration, Boolean)] =
    Some(span.text, span.font, span.decor, span.compressSpaces)
}

sealed abstract class TextSpan extends Text {
  def scale(factor: Double): TextSpan

  override // ...to return covariant return type
  def *(scaleFactor: Double): TextSpan = scale(scaleFactor)

  def unary_+ : TextSpan = BaselineShiftedTextSpan(this, BaselineShift.Above)

  def unary_- : TextSpan = BaselineShiftedTextSpan(this, BaselineShift.Below)

  def text: String
  def font: Font
  def decor: TextDecoration
  def compressSpaces: Boolean

  def +:(str: String): TextSpan

  def prepend(str: String): TextSpan = +:(str)

  def ++(str: String): TextSpan

  def append(str: String): TextSpan = ++(str)


  def asLine: TextLine = TextLine(this)
}

case class BasicTextSpan(
    text: String,
    font: Font,
    decor: TextDecoration = TextSpan.defaultTextDecoration,
    compressSpaces: Boolean = true
  ) extends TextSpan with TextBoundingBoxMemoization {

  def +:(str: String): TextSpan = copy(text = str + text)

  def ++(str: String): TextSpan = copy(text = text + str)


  override def scale(factor: Double): TextSpan = copy(font = font scale factor)

  override protected def calcTextBoundingBox(rulerFactory: TextRulerFactory):
      Dims = {
    val ruler = rulerFactory.create(font)
    ruler.measure(text, compressSpaces)
  }
}

case class BaselineShiftedTextSpan(span: TextSpan, shift: BaselineShift)
    extends TextSpan with TextBoundingBoxMemoization {

  def +:(str: String): TextSpan = copy(span = str +: span)

  def ++(str: String): TextSpan = copy(span = span ++ str)


  override def scale(factor: Double): TextSpan = copy(span = span scale factor)

  def text: String            = span.text
  def font: Font              = span.font
  def decor: TextDecoration   = span.decor
  def compressSpaces: Boolean = span.compressSpaces


  override protected def calcTextBoundingBox(rulerFactory: TextRulerFactory):
      Dims = {
    def calcFontHeight(font: Font): Double = font.size match {
      case FontSize.Std(num) => num
    }
    span.textBoundingBox(rulerFactory) move (shift match {
      // NOTE: use 1/4 of font height as an approximation of shift
      case BaselineShift.Above => Point(0, -calcFontHeight(font) / 4)
      case BaselineShift.Below => Point(0,  calcFontHeight(font) / 4)
    } )
  }
}


trait DimsManipulation { self: Text =>
  val nullRect = OriginDims(0, 0)

  protected def sumWidth_maxHeight(r1: OriginDims, r2: Dims) =
    OriginDims(r1.width + r2.width, math.max(r1.height, r2.height))

  protected def maxWidth_sumHeight(r1: OriginDims, r2: Dims) =
    OriginDims(math.max(r1.width, r2.width), r1.height + r2.height)

  protected def quarterTurn(r: OriginDims) = OriginDims(r.height, r.width)

  protected def identity(r: OriginDims) = r

  protected def scaleWidth(r: Dims, s: Double) =
    OriginDims(r.width * s, r.height)

  protected def scaleHeight(r: Dims, s: Double) =
    OriginDims(r.width, r.height * s)
}


object TextLine {
  val defaultBreadth   = 1.25
  val defaultTextAlign = TextAlign.Start
  val defaultTextDir   = TextDirection.LeftToRight

  def apply(
      content: TextSpan,
      breadth: Double,
      align: TextAlign,
      dir: TextDirection
    ): TextLine =
    TextLine(List(content), breadth, align, dir)


  def apply(content: TextSpan): TextLine = TextLine(List(content))

  def apply(content: TextSpan, breadth: Double): TextLine =
    TextLine(List(content), breadth)

  def apply(content: TextSpan, align: TextAlign): TextLine =
    TextLine(List(content), align = align)

  def apply(content: TextSpan, breadth: Double, align: TextAlign): TextLine =
    TextLine(List(content), breadth, align)


  def apply(
      text: String,
      font: Font,
      breadth: Double,
      align: TextAlign,
      dir: TextDirection
    ): TextLine =
    TextLine(List(TextSpan(text, font)), breadth, align, dir)

  def apply(
      text: String,
      font: Font,
      compressSpaces: Boolean,
      breadth: Double,
      align: TextAlign,
      dir: TextDirection
    ): TextLine =
    TextLine(
        List(TextSpan(text, font, compressSpaces = compressSpaces)),
        breadth,
        align,
        dir
      )

  def apply(
      text: String,
      font: Font,
      decor: TextDecoration,
      breadth: Double,
      align: TextAlign,
      dir: TextDirection
    ): TextLine =
    TextLine(List(TextSpan(text, font, decor)), breadth, align, dir)

  def apply(
      text: String,
      font: Font,
      decor: TextDecoration,
      compressSpaces: Boolean,
      breadth: Double,
      align: TextAlign,
      dir: TextDirection
    ): TextLine =
    TextLine(
        List(TextSpan(text, font, decor, compressSpaces)),
        breadth,
        align,
        dir
      )


  def apply(text: String, font: Font, breadth: Double, align: TextAlign):
      TextLine =
    TextLine(List(TextSpan(text, font)), breadth, align)

  def apply(
      text: String,
      font: Font,
      compressSpaces: Boolean,
      breadth: Double,
      align: TextAlign
    ): TextLine =
    TextLine(
        List(TextSpan(text, font, compressSpaces = compressSpaces)),
        breadth,
        align
      )

  def apply(
      text: String,
      font: Font,
      decor: TextDecoration,
      breadth: Double,
      align: TextAlign
    ): TextLine =
    TextLine(List(TextSpan(text, font, decor)), breadth, align)

  def apply(
      text: String,
      font: Font,
      decor: TextDecoration,
      compressSpaces: Boolean,
      breadth: Double,
      align: TextAlign
    ): TextLine =
    TextLine(List(TextSpan(text, font, decor, compressSpaces)), breadth, align)


  def apply(text: String, font: Font, align: TextAlign): TextLine =
    TextLine(List(TextSpan(text, font)), align = align)

  def apply(
      text: String,
      font: Font,
      compressSpaces: Boolean,
      align: TextAlign
    ): TextLine =
    TextLine(
        List(TextSpan(text, font, compressSpaces = compressSpaces)),
        align = align
      )

  def apply(text: String, font: Font, decor: TextDecoration, align: TextAlign):
      TextLine =
    TextLine(List(TextSpan(text, font, decor)), align = align)

  def apply(
      text: String,
      font: Font,
      decor: TextDecoration,
      compressSpaces: Boolean,
      align: TextAlign
    ): TextLine =
    TextLine(List(TextSpan(text, font, decor, compressSpaces)), align = align)


  def apply(text: String, font: Font, breadth: Double): TextLine =
    TextLine(List(TextSpan(text, font)), breadth)

  def apply(text: String, font: Font, compressSpaces: Boolean, breadth: Double):
      TextLine =
    TextLine(List(TextSpan(text, font, compressSpaces=compressSpaces)), breadth)

  def apply(text: String, font: Font, decor: TextDecoration, breadth: Double):
      TextLine =
    TextLine(List(TextSpan(text, font, decor)), breadth)

  def apply(
      text: String,
      font: Font,
      decor: TextDecoration,
      compressSpaces: Boolean,
      breadth: Double
    ): TextLine =
    TextLine(List(TextSpan(text, font, decor, compressSpaces)), breadth)


  def apply(text: String, font: Font): TextLine =
    TextLine(List(TextSpan(text, font)))

  def apply(text: String, font: Font, compressSpaces: Boolean): TextLine =
    TextLine(List(TextSpan(text, font, compressSpaces = compressSpaces)))

  def apply(text: String, font: Font, decor: TextDecoration): TextLine =
    TextLine(List(TextSpan(text, font, decor)))

  def apply(
      text: String,
      font: Font,
      decor: TextDecoration,
      compressSpaces: Boolean
    ): TextLine =
    TextLine(List(TextSpan(text, font, decor, compressSpaces)))
}

case class TextLine(
    content: List[TextSpan],
    breadth: Double    = TextLine.defaultBreadth,
    align: TextAlign   = TextLine.defaultTextAlign,
    dir: TextDirection = TextLine.defaultTextDir
  ) extends Text with TextBoundingBoxMemoization
       with DimsManipulation {

  def ::(span: TextSpan): TextLine = copy(content = span :: content)

  def prepend(span: TextSpan): TextLine = ::(span)

  def :::(spans: List[TextSpan]): TextLine = copy(content = spans ::: content)

  def concat(spans: List[TextSpan]): TextLine = :::(spans)


  override def scale(factor: Double): TextLine =
    copy(content = content map ( _ scale factor ))

  override // ...to return covariant return type
  def *(scaleFactor: Double): TextLine = scale(scaleFactor)

  def textBoundingBox(
      rulerFactory: TextRulerFactory,
      enclosingTextBlockMode: WritingMode
    ): Dims = {
    val horizFlow = (maxWidth_sumHeight _, scaleHeight _, identity     _)
    val vertFlow  = (sumWidth_maxHeight _, scaleWidth  _, quarterTurn  _)

    val blockOrientation = enclosingTextBlockMode.orientation
    val (combine, scaleLine, finalAdjustment) = blockOrientation match {
      case Orientation.Horizontal => horizFlow
      case Orientation.Vertical   => vertFlow
    }
    val bbox = (nullRect /: content.map( _.textBoundingBox(rulerFactory) )){
      (sum, r) => combine(sum, scaleLine(r, breadth))
    }
    finalAdjustment(bbox)
  }


  def asBlock: TextBlock = TextBlock(this)


  override protected def calcTextBoundingBox(rulerFactory: TextRulerFactory):
      Dims =
    textBoundingBox(rulerFactory, TextBlock.defaultWritingMode)
}


object TextBlock {
  val defaultWritingMode = WritingMode.LR_TB

  def apply(content: TextLine): TextBlock = TextBlock(List(content))

  def apply(content: TextLine, mode: WritingMode): TextBlock =
    TextBlock(List(content), mode)


  def apply(lines: List[String], font: Font): TextBlock =
    TextBlock(lines map { TextLine(_, font) })

  def apply(line: String, font: Font): TextBlock =
    apply(List(line), font)

  def apply(lines: List[String], font: Font, compressSpaces: Boolean):
      TextBlock =
    TextBlock(lines map { TextLine(_, font, compressSpaces) })

  def apply(line: String, font: Font, compressSpaces: Boolean):
      TextBlock =
    apply(List(line), font, compressSpaces)


  def apply(lines: List[String], font: Font, mode: WritingMode): TextBlock =
    TextBlock(lines map { TextLine(_, font) }, mode)

  def apply(line: String, font: Font, mode: WritingMode): TextBlock =
    apply(List(line), font, mode)

  def apply(
      lines: List[String],
      font: Font,
      compressSpaces: Boolean,
      mode: WritingMode
    ): TextBlock =
    TextBlock(lines map {TextLine(_, font, compressSpaces)}, mode)

  def apply(
      line: String,
      font: Font,
      compressSpaces: Boolean,
      mode: WritingMode
    ): TextBlock =
    apply(List(line), font, compressSpaces, mode)


  def apply(
      lines: List[String],
      font: Font,
      breadth: Double,
      mode: WritingMode
    ): TextBlock =
    TextBlock(lines map { TextLine(_, font, breadth) }, mode)

  def apply(line: String, font: Font, breadth: Double, mode: WritingMode):
      TextBlock =
    apply(List(line), font, breadth, mode)

  def apply(
      lines: List[String],
      font: Font,
      compressSpaces: Boolean,
      breadth: Double,
      mode: WritingMode
    ): TextBlock =
    TextBlock(lines map { TextLine(_, font, compressSpaces, breadth) }, mode)

  def apply(
      line: String,
      font: Font,
      compressSpaces: Boolean,
      breadth: Double,
      mode: WritingMode
    ): TextBlock =
    apply(List(line), font, compressSpaces, breadth, mode)


  def apply(lines: List[String], font: Font, breadth: Double): TextBlock =
    TextBlock(lines map { TextLine(_, font, breadth) })

  def apply(line: String, font: Font, breadth: Double): TextBlock =
    apply(List(line), font, breadth)

  def apply(
      lines: List[String],
      font: Font,
      compressSpaces: Boolean,
      breadth: Double
    ): TextBlock =
    TextBlock(lines map { TextLine(_, font, compressSpaces, breadth) })

  def apply(line: String, font: Font, compressSpaces: Boolean, breadth: Double):
      TextBlock =
    apply(List(line), font, compressSpaces, breadth)
}

case class TextBlock(
    content: List[TextLine],
    mode: WritingMode = TextBlock.defaultWritingMode
  ) extends Text with TextBoundingBoxMemoization
       with DimsManipulation {
  def ::(line: TextLine): TextBlock = copy(content = line :: content)

  def prepend(line: TextLine): TextBlock = ::(line)

  def :::(lines: List[TextLine]): TextBlock = copy(content = lines ::: content)

  def concat(lines: List[TextLine]): TextBlock = :::(lines)


  override def scale(factor: Double): TextBlock =
    copy(content = content map ( _ scale factor ))

  override // ...to return covariant return type
  def *(scaleFactor: Double): TextBlock = scale(scaleFactor)

  override protected def calcTextBoundingBox(rulerFactory: TextRulerFactory):
      Dims = {
    val horizFlow = (maxWidth_sumHeight _, identity     _)
    val vertFlow  = (sumWidth_maxHeight _, quarterTurn _)

    val (combine, finalAdjustment) = mode.orientation match {
      case Orientation.Horizontal => horizFlow
      case Orientation.Vertical   => vertFlow
    }
    val bbox =
        (nullRect /: content.map { _.textBoundingBox(rulerFactory, mode) }) {
          combine(_, _)
        }
    finalAdjustment(bbox)
  }
}


trait MemoizingTextRulerFactory { self: TextRulerFactory =>

  def create(font: Font): TextRuler =
    recall.getOrElseUpdate(font, doCreate(font))

  def forgetAll {
    recall.clear()
  }

  protected def doCreate(font: Font): TextRuler

  private val recall: ConcurrentMap[Font, TextRuler] =
    new ConcurrentHashMap[Font, TextRuler]
}


trait CharFilteringTextRuler { self: TextRuler =>
  def filterChars(s: String, compressSpaces: Boolean): String
}

trait SpaceCompressableFilteringTextRuler
    extends CharFilteringTextRuler { self: TextRuler =>

  def filterChars(s: String, compressSpaces: Boolean): String =
    if (compressSpaces) s.replaceAll("\\s+", " ")
    else s
}


trait CharMeasurementsSource {

  def charWidths: Seq[(Char, Double)]
}


trait FallbackToAvgTextRuler
    extends TextRuler
       with CharFilteringTextRuler { self: CharMeasurementsSource =>

  def measure(s: String, compressSpaces: Boolean): Dims = OriginDims(
      measureWidth(filterChars(s, compressSpaces)),
      uniformCharHeight
    )


  protected def measureWidth(s: String): Double =
    // scale entire measurement--not individual chars--to minimize rounding err
    scale((0.0 /: s)( _ + calcWidth(_) ))

  protected def calcWidth(c: Char): Double =
    getCharWidth(c).getOrElse(avgCharWidth)

  // NOTE: all measurements shall be of 10pt fonts:
  protected def scale(measure: Double): Double =
    (measure / 10.0) * uniformCharHeight


  protected def getCharWidth(c: Char): Option[Double] =
    // kludge!--should really be given by `def charWidths: Seq[(Char, Double)]`
    charMeasurements.get(c).orElse {
      if (c == '\t' || c == '\n') charMeasurements.get(' ') else None
    }

  protected lazy val avgCharWidth: Double =
    if (charMeasurements.size == 0) 5.2
    else charMeasurements.values.sum / charMeasurements.size

  protected val uniformCharHeight: Double


  protected lazy val charMeasurements: Map[Char, Double] = Map(charWidths: _*)
}


trait LoadableCharMeasurements extends CharMeasurementsSource {

  val loadDir: String

  val font: Font

  def charWidths: Seq[(Char, Double)] =
    for {
      fontMeasureFpath <- Resources.matching(
          calcMeasureFpathPrefix(font, loadDir), classOf[Font]
        )
      measureSeq <- new CharDoubleSeqFromDataFile(fontMeasureFpath) {
        override protected val loadVia = classOf[Font]
      }.getSeq
    } yield measureSeq

  protected def calcMeasureFpathPrefix(font: Font, pathDirBase: String):
      String =
    calcMeasureDir(font, pathDirBase) + "/" + calcMeasureFnameBase(font)

  protected def calcMeasureFnameBase(font: Font): String = {
    val familyName = normalizeFontFamily(font.family)
    val fontWeight = font.weight.css2Equiv
    "w" + fontWeight + ".measure."
  }

  protected def calcMeasureDir(font: Font, pathDirBase: String): String =
    pathDirBase + "/" + normalizeFontFamily(font.family)

  protected def normalizeFontFamily(name: String) =
    name.toLowerCase.replaceAll("\\s+", "_")
}

object DefaultTextRulerFactory
    extends TextRulerFactory with MemoizingTextRulerFactory {

  override protected def doCreate(font0: Font): TextRuler =
    new FallbackToAvgTextRuler
        with SpaceCompressableFilteringTextRuler
        with LoadableCharMeasurements {

      override val loadDir = "/data/k_k_.graphics.tie/font/measure"

      override val font = font0

      // WARNING: must preceede use to avoid nasty silent init to 0.0 bug!
      override protected val uniformCharHeight: Double = font.size match {
        case FontSize.Std(num) => num
      }
    }
}

}
