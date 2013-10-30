/*
   file: k_k_/graphics/tie/ink/Pen.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2013 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie.ink

import k_k_.graphics.tie.effects.Effect.Opacity


sealed abstract class StrokeEnds
object StrokeEnds {
  case object Exact   extends StrokeEnds
  case object Round   extends StrokeEnds
  case object Extend  extends StrokeEnds
  case object Inherit extends StrokeEnds
}


sealed abstract class StrokeCorners

object StrokeCorners {
  case object Exact extends StrokeCorners {
    def apply(clipUnder: Double): Exact = Exact(Some(clipUnder))
  }
  // `clipUnder` is ratio lower than which `Exact` appears as `Clip`, so as to
  // keep small angle corners from poking out and looking more like an end;
  // defined as: clipUnder = stroke-width / sin(angle-theta / 2)
  // (equiv. to: attribute 'stroke-miterlimit' in SVG 1.1)
  case class  Exact(clipUnder: Option[Double]) extends StrokeCorners
  case object Round                            extends StrokeCorners
  case object Clip                             extends StrokeCorners
  case object Inherit                          extends StrokeCorners
}


sealed abstract class FillRule
object FillRule {
  case object NonZero      extends FillRule // default
  case object EvenOdd      extends FillRule
  // NOTE: expected to be included in SVG 2.0; until then, equiv. to NonZeroFill
  case object WindingCount extends FillRule
  case object Inherit      extends FillRule
}


object Pen {

  def stroke(ink: Ink): Pen =
    InkPen(Some(ink), None)

  def stroke(ink: Ink, ends: StrokeEnds): Pen =
    InkPen(Some(ink), None, ends = Some(ends))

  def stroke(ink: Ink, corners: StrokeCorners): Pen =
    InkPen(Some(ink), None, corners = Some(corners))

  def stroke(ink: Ink, ends: StrokeEnds, corners: StrokeCorners): Pen =
    InkPen(Some(ink), None,
           ends    = Some(ends),
           corners = Some(corners))


  def stroke(ink: Ink, width: Double): Pen =
    InkPen(Some(ink), None, Some(width))

  def stroke(ink: Ink, width: Double, ends: StrokeEnds): Pen =
    InkPen(Some(ink), None, Some(width), ends = Some(ends))

  def stroke(ink: Ink, width: Double, corners: StrokeCorners): Pen =
    InkPen(Some(ink), None, Some(width), corners = Some(corners))

  def stroke(
      ink: Ink, width: Double, ends: StrokeEnds, corners: StrokeCorners
    ): Pen =
    InkPen(Some(ink), None, Some(width),
           ends    = Some(ends),
           corners = Some(corners))


  def fill(ink: Ink): Pen =
    InkPen(None, Some(ink))

  def fill(ink: Ink, fillRule: FillRule): Pen =
    InkPen(None, Some(ink), fillRule = Some(fillRule))


  def dashed(ink: Ink, dashPattern: List[Double]): Pen =
    InkPen(Some(ink), None, None, Some(dashPattern))

  def dashed(ink: Ink, dashPattern: List[Double], ends: StrokeEnds): Pen =
    InkPen(Some(ink), None, None, Some(dashPattern),
           ends = Some(ends))

  def dashed(ink: Ink, dashPattern: List[Double],
             corners: StrokeCorners): Pen =
    InkPen(Some(ink), None, None, Some(dashPattern),
           corners = Some(corners))

  def dashed(
      ink: Ink, dashPattern: List[Double], ends: StrokeEnds, corners: StrokeCorners
    ): Pen =
    InkPen(Some(ink), None, None, Some(dashPattern),
           ends    = Some(ends),
           corners = Some(corners))


  def dashed(ink: Ink, dashPattern: List[Double], dashOffset: Double): Pen =
    InkPen(Some(ink), None, None, Some(dashPattern), Some(dashOffset))

  def dashed(
      ink: Ink, dashPattern: List[Double], dashOffset: Double, ends: StrokeEnds
    ): Pen =
    InkPen(Some(ink), None, None, Some(dashPattern), Some(dashOffset),
           ends = Some(ends))

  def dashed(
      ink: Ink, dashPattern: List[Double], dashOffset: Double, corners: StrokeCorners
    ): Pen =
    InkPen(Some(ink), None, None, Some(dashPattern), Some(dashOffset),
           corners = Some(corners))

  def dashed(
      ink: Ink, dashPattern: List[Double], dashOffset: Double, ends: StrokeEnds, corners: StrokeCorners
    ): Pen =
    InkPen(Some(ink), None, None, Some(dashPattern), Some(dashOffset),
           ends    = Some(ends),
           corners = Some(corners))


  def dashed(ink: Ink, dashLength: Double): Pen =
    dashed(ink, List(dashLength, dashLength))

  def dashed(ink: Ink, dashLength: Double, ends: StrokeEnds): Pen =
    dashed(ink, List(dashLength, dashLength), ends)

  def dashed(ink: Ink, dashLength: Double, corners: StrokeCorners): Pen =
    dashed(ink, List(dashLength, dashLength), corners)

  def dashed(
      ink: Ink, dashLength: Double, ends: StrokeEnds, corners: StrokeCorners
    ): Pen =
    dashed(ink, List(dashLength, dashLength), ends, corners)


  def dashed(ink: Ink, width: Double, dashPattern: List[Double]): Pen =
    InkPen(Some(ink), None, Some(width), Some(dashPattern))

  def dashed(
      ink: Ink, width: Double, dashPattern: List[Double], ends: StrokeEnds
    ): Pen =
    InkPen(Some(ink), None, Some(width), Some(dashPattern),
           ends = Some(ends))

  def dashed(
      ink: Ink, width: Double, dashPattern: List[Double], corners: StrokeCorners
    ): Pen =
    InkPen(Some(ink), None, Some(width), Some(dashPattern),
           corners = Some(corners))

  def dashed(
      ink: Ink, width: Double, dashPattern: List[Double], ends: StrokeEnds, corners: StrokeCorners
    ): Pen =
    InkPen(Some(ink), None, Some(width), Some(dashPattern),
           ends    = Some(ends),
           corners = Some(corners))


  def dashed(
      ink: Ink, width: Double, dashPattern: List[Double], dashOffset: Double
    ): Pen =
    InkPen(Some(ink), None, Some(width), Some(dashPattern), Some(dashOffset))

  def dashed(
      ink: Ink, width: Double, dashPattern: List[Double], dashOffset: Double, ends: StrokeEnds
    ): Pen =
    InkPen(Some(ink), None, Some(width), Some(dashPattern), Some(dashOffset),
           ends = Some(ends))

  def dashed(
      ink: Ink, width: Double, dashPattern: List[Double], dashOffset: Double, corners: StrokeCorners
    ): Pen =
    InkPen(Some(ink), None, Some(width), Some(dashPattern), Some(dashOffset),
           corners = Some(corners))

  def dashed(
      ink: Ink, width: Double, dashPattern: List[Double], dashOffset: Double, ends: StrokeEnds, corners: StrokeCorners
    ): Pen =
    InkPen(Some(ink), None, Some(width), Some(dashPattern), Some(dashOffset),
           ends    = Some(ends),
           corners = Some(corners))


  def dashed(ink: Ink, width: Double, dashLength: Double, dashOffset: Double):
    Pen =
    dashed(ink, width, List(dashLength, dashLength), dashOffset)

  def dashed(
      ink: Ink, width: Double, dashLength: Double, dashOffset: Double, ends: StrokeEnds
    ): Pen =
    dashed(ink, width, List(dashLength, dashLength), dashOffset, ends)

  def dashed(
      ink: Ink, width: Double, dashLength: Double, dashOffset: Double, corners: StrokeCorners
    ): Pen =
    dashed(ink, width, List(dashLength, dashLength), dashOffset, corners)

  def dashed(
      ink: Ink, width: Double, dashLength: Double, dashOffset: Double,
      ends: StrokeEnds, corners: StrokeCorners
    ): Pen =
    dashed(ink, width, List(dashLength, dashLength), dashOffset, ends, corners)


  lazy val invisible =
    new InkPen(NullInk, NullInk)


  def apply(stroke: Ink, fill: Ink) =
    new InkPen(stroke, fill)

  def apply(stroke: Ink, fill: Ink, ends: StrokeEnds): Pen =
    new InkPen(stroke, fill, ends = ends)

  def apply(stroke: Ink, fill: Ink, corners: StrokeCorners): Pen =
    new InkPen(stroke, fill, corners = corners)

  def apply(stroke: Ink, fill: Ink, ends: StrokeEnds, corners: StrokeCorners):
    Pen =
    new InkPen(stroke, fill, ends = ends, corners = corners)

  def apply(stroke: Ink, fill: Ink, fillRule: FillRule) =
    new InkPen(stroke, fill, fillRule = fillRule)

  def apply(
      stroke: Ink, fill: Ink, ends: StrokeEnds, corners: StrokeCorners, fillRule: FillRule
    ) =
    new InkPen(
        stroke, fill, ends = ends, corners = corners, fillRule = fillRule
      )


  def apply(stroke: Ink, fill: Ink, width: Double) =
    new InkPen(stroke, fill, width)

  def apply(stroke: Ink, fill: Ink, width: Double, ends: StrokeEnds): Pen =
    new InkPen(stroke, fill, width, ends)

  def apply(stroke: Ink, fill: Ink, width: Double, corners: StrokeCorners):
    Pen =
    new InkPen(stroke, fill, width, corners)

  def apply(
      stroke: Ink, fill: Ink, width: Double, ends: StrokeEnds, corners: StrokeCorners
    ): Pen =
    new InkPen(stroke, fill, width, ends, corners)

  def apply(stroke: Ink, fill: Ink, width: Double, fillRule: FillRule) =
    new InkPen(stroke, fill, width, fillRule)

  def apply(
      stroke: Ink, fill: Ink, width: Double, ends: StrokeEnds, corners: StrokeCorners, fillRule: FillRule
    ) =
    new InkPen(stroke, fill, width, ends, corners, fillRule)
}


sealed abstract class Pen {

  // accessors:
  def stroke:      Option[Ink]
  def fill:        Option[Ink]
  def width:       Option[Double]
  def dashPattern: Option[List[Double]]
  def dashOffset:  Option[Double]
  def ends:        Option[StrokeEnds]
  def corners:     Option[StrokeCorners]
  def fillRule:    Option[FillRule]


  // modifiers:
  def extractStroke: Pen
  def extractFill:   Pen

  def flipInk: Pen
  def flipOpacity: Pen
  def flipInkNopacity: Pen


  // `dashed` methods shall have no effect on other stroke properties
  def dashed(dashPattern: List[Double]): Pen
  def dashed(dashPattern: List[Double], dashOffset: Double): Pen

  def dashed(dashLength: Double): Pen =
    dashed(List(dashLength, dashLength))

  def dashed(dashLength: Double, dashOffset: Double): Pen =
    dashed(List(dashLength, dashLength), dashOffset)


  def stroke(ink: Ink): Pen
  def stroke(ink: Ink, ends: StrokeEnds): Pen
  def stroke(ink: Ink, corners: StrokeCorners): Pen
  def stroke(ink: Ink, ends: StrokeEnds, corners: StrokeCorners): Pen
  def stroke(ink: Ink, width: Double): Pen
  def stroke(ink: Ink, width: Double, ends: StrokeEnds): Pen
  def stroke(ink: Ink, width: Double, corners: StrokeCorners): Pen
  def stroke(ink: Ink, width: Double, ends: StrokeEnds, corners: StrokeCorners):
    Pen
  def stroke(like: Pen): Pen


  def fill(ink: Ink): Pen
  def fill(ink: Ink, fillRule: FillRule): Pen
  def fill(like: Pen): Pen


  def scalePen(scaling: Double): Pen
  def ~*(scaling: Double): Pen = scalePen(scaling)


  def comboPen(other: Pen): Pen
  def ~&(other: Pen): Pen = comboPen(other)


  def exhibitPen(opacity: Opacity): Pen
  def exhibitPen(opacity: Double): Pen = exhibitPen(Opacity(opacity))
  def alpha(opacity: Opacity): Pen = exhibitPen(opacity)
  def alpha(opacity: Double): Pen = exhibitPen(opacity)
  def ~#(opacity: Opacity): Pen = exhibitPen(opacity)
  def ~#(opacity: Double): Pen = exhibitPen(opacity)
}


final case class InkPen protected[ink] (
  stroke:      Option[Ink],
  fill:        Option[Ink],
  width:       Option[Double]        = None,
  dashPattern: Option[List[Double]]  = None,
  dashOffset:  Option[Double]        = None,
  ends:        Option[StrokeEnds]    = None,
  corners:     Option[StrokeCorners] = None,
  fillRule:    Option[FillRule]      = None
  ) extends Pen {

  def this(stroke: Ink, fill: Ink) =
    this(Some(stroke), Some(fill))

  def this(stroke: Ink, fill: Ink, ends: StrokeEnds) =
    this(Some(stroke), Some(fill), ends = Some(ends))

  def this(stroke: Ink, fill: Ink, corners: StrokeCorners) =
    this(Some(stroke), Some(fill), corners = Some(corners))

  def this(stroke: Ink, fill: Ink, ends: StrokeEnds, corners: StrokeCorners) =
    this(Some(stroke), Some(fill), ends = Some(ends), corners = Some(corners))

  def this(stroke: Ink, fill: Ink, fillRule: FillRule) =
    this(Some(stroke), Some(fill), fillRule = Some(fillRule))

  def this(
      stroke: Ink, fill: Ink, ends: StrokeEnds, corners: StrokeCorners,
      fillRule: FillRule
    ) =
    this(
        Some(stroke), Some(fill), None, None, None,
        Some(ends), Some(corners), Some(fillRule)
      )


  def this(stroke: Ink, fill: Ink, width: Double) =
    this(Some(stroke), Some(fill), Some(width))

  def this(stroke: Ink, fill: Ink, width: Double, ends: StrokeEnds) =
    this(Some(stroke), Some(fill), Some(width), ends = Some(ends))

  def this(stroke: Ink, fill: Ink, width: Double, corners: StrokeCorners) =
    this(Some(stroke), Some(fill), Some(width), corners = Some(corners))

  def this(
      stroke: Ink, fill: Ink, width: Double, ends: StrokeEnds, corners: StrokeCorners
    ) =
    this(Some(stroke), Some(fill), Some(width),
         ends = Some(ends), corners = Some(corners))

  def this(stroke: Ink, fill: Ink, width: Double, fillRule: FillRule) =
    this(Some(stroke), Some(fill), Some(width), fillRule = Some(fillRule))

  def this(stroke: Ink, fill: Ink, width: Double,
           ends: StrokeEnds, corners: StrokeCorners,
           fillRule: FillRule) =
    this(Some(stroke), Some(fill), Some(width), None, None,
         Some(ends), Some(corners), Some(fillRule))


  def this(stroke: Ink, fill: Ink, width: Double,
           dashPattern: List[Double], dashOffset: Double,
           ends: StrokeEnds) =
    this(Some(stroke), Some(fill), Some(width),
         Some(dashPattern), Some(dashOffset),
         ends = Some(ends))

  def this(stroke: Ink, fill: Ink, width: Double,
           dashPattern: List[Double], dashOffset: Double,
           corners: StrokeCorners) =
    this(Some(stroke), Some(fill), Some(width),
         Some(dashPattern), Some(dashOffset),
         corners = Some(corners))

  def this(stroke: Ink, fill: Ink, width: Double,
           dashPattern: List[Double], dashOffset: Double,
           ends: StrokeEnds, corners: StrokeCorners) =
    this(Some(stroke), Some(fill), Some(width),
         Some(dashPattern), Some(dashOffset),
         Some(ends), Some(corners))

  def this(stroke: Ink, fill: Ink, width: Double,
           dashPattern: List[Double], dashOffset: Double,
           fillRule: FillRule) =
    this(Some(stroke), Some(fill), Some(width),
         Some(dashPattern), Some(dashOffset),
         None, None, Some(fillRule))

  def this(stroke: Ink, fill: Ink, width: Double,
           dashPattern: List[Double], dashOffset: Double,
           ends: StrokeEnds, corners: StrokeCorners,
           fillRule: FillRule) =
    this(Some(stroke), Some(fill), Some(width),
         Some(dashPattern), Some(dashOffset),
         Some(ends), Some(corners), Some(fillRule))


  def this(stroke: Ink, fill: Ink, width: Double,
           dashLength: Double, dashOffset: Double,
           ends: StrokeEnds) =
    this(Some(stroke), Some(fill), Some(width),
         Some(List(dashLength, dashLength)),
         Some(dashOffset),
         ends = Some(ends))

  def this(stroke: Ink, fill: Ink, width: Double,
           dashLength: Double, dashOffset: Double,
           corners: StrokeCorners) =
    this(Some(stroke), Some(fill), Some(width),
         Some(List(dashLength, dashLength)),
         Some(dashOffset),
         corners = Some(corners))

  def this(stroke: Ink, fill: Ink, width: Double,
           dashLength: Double, dashOffset: Double,
           ends: StrokeEnds, corners: StrokeCorners) =
    this(Some(stroke), Some(fill), Some(width),
         Some(List(dashLength, dashLength)),
         Some(dashOffset),
         Some(ends), Some(corners))

  def this(stroke: Ink, fill: Ink, width: Double,
           dashLength: Double, dashOffset: Double,
           fillRule: FillRule) =
    this(Some(stroke), Some(fill), Some(width),
         Some(List(dashLength, dashLength)),
         Some(dashOffset),
         None, None, Some(fillRule))

  def this(stroke: Ink, fill: Ink, width: Double,
           dashLength: Double, dashOffset: Double,
           ends: StrokeEnds, corners: StrokeCorners,
           fillRule: FillRule) =
    this(Some(stroke), Some(fill), Some(width),
         Some(List(dashLength, dashLength)),
         Some(dashOffset),
         Some(ends), Some(corners), Some(fillRule))


  private def this() = this(None, None)


  val strokeOpacity: Option[Double] = stroke.map( _.opacity )
  val fillOpacity:   Option[Double] = fill.  map( _.opacity )


  def extractStroke: Pen = new InkPen().stroke(this)
  def extractFill:   Pen = new InkPen().fill(this)


  def flipInk: Pen = copy(stroke = fill, fill = stroke)

  def flipOpacity: Pen = {
    val (baseStroke, strokeOpacity) = stroke.map { decompose }.getOrElse {
      (None, None)
    }
    val (baseFill, fillOpacity) = fill.map { decompose }.getOrElse {
      (None, None)
    }
    copy(
        stroke = baseStroke.map { _ alpha fillOpacity.  getOrElse(1.0) },
        fill   = baseFill.  map { _ alpha strokeOpacity.getOrElse(1.0) }
      )
  }

  def flipInkNopacity: Pen = {
    val (baseStroke, strokeOpacity) = stroke.map { decompose }.getOrElse {
      (None, None)
    }
    val (baseFill, fillOpacity) = fill.map { decompose }.getOrElse {
      (None, None)
    }
    copy(
        stroke = baseFill.  map { _ alpha strokeOpacity.getOrElse(1.0) },
        fill   = baseStroke.map { _ alpha fillOpacity.  getOrElse(1.0) }
      )
  }


  def dashed(dashPattern: List[Double]): Pen =
    copy(dashPattern = Some(dashPattern), dashOffset  = None)

  def dashed(dashPattern: List[Double], dashOffset: Double): Pen =
    copy(dashPattern = Some(dashPattern), dashOffset  = Some(dashOffset))


  def stroke(ink: Ink): Pen = new InkPen(
      Some(ink), fill,
      width       = None,
      dashPattern = None,
      dashOffset  = None,
      ends        = None,
      corners     = None,
      fillRule    = fillRule
    )

  def stroke(ink: Ink, ends: StrokeEnds): Pen = new InkPen(
      Some(ink), fill,
      width       = None,
      dashPattern = None,
      dashOffset  = None,
      ends        = Some(ends),
      corners     = None,
      fillRule    = fillRule
    )

  def stroke(ink: Ink, corners: StrokeCorners): Pen = new InkPen(
      Some(ink), fill,
      width       = None,
      dashPattern = None,
      dashOffset  = None,
      ends        = None,
      corners     = Some(corners),
      fillRule    = fillRule
    )

  def stroke(ink: Ink, ends: StrokeEnds, corners: StrokeCorners): Pen =
    new InkPen(
        Some(ink), fill,
        width       = None,
        dashPattern = None,
        dashOffset  = None,
        ends        = Some(ends),
        corners     = Some(corners),
        fillRule    = fillRule
      )


  def stroke(ink: Ink, width: Double): Pen = new InkPen(
      Some(ink), fill, Some(width),
      dashPattern = None,
      dashOffset  = None,
      ends        = None,
      corners     = None,
      fillRule    = fillRule
    )

  def stroke(ink: Ink, width: Double, ends: StrokeEnds): Pen = new InkPen(
      Some(ink), fill, Some(width),
      dashPattern = None,
      dashOffset  = None,
      ends        = Some(ends),
      corners     = None,
      fillRule    = fillRule
    )

  def stroke(ink: Ink, width: Double, corners: StrokeCorners): Pen = new InkPen(
      Some(ink), fill, Some(width),
      dashPattern = None,
      dashOffset  = None,
      ends        = None,
      corners     = Some(corners),
      fillRule    = fillRule
    )

  def stroke(ink: Ink, width: Double, ends: StrokeEnds, corners: StrokeCorners):
      Pen =
    new InkPen(
        Some(ink), fill, Some(width),
        dashPattern = None,
        dashOffset  = None,
        ends        = Some(ends),
        corners     = Some(corners),
        fillRule    = fillRule
      )


  def stroke(like: Pen): Pen = copy(
      stroke      = like.stroke,
      width       = like.width,
      dashPattern = like.dashPattern,
      dashOffset  = like.dashOffset,
      ends        = like.ends,
      corners     = like.corners
    )


  def fill(ink: Ink): Pen =
    copy(fill = Some(ink), fillRule = None)

  def fill(ink: Ink, fillRule: FillRule): Pen =
    copy(fill = Some(ink), fillRule = Some(fillRule))

  def fill(like: Pen): Pen =
    copy(fill = like.fill, fillRule = like.fillRule)


  def scalePen(scaling: Double): Pen = copy(
      width       = width.      map {        _ * scaling },
      dashPattern = dashPattern.map { _.map( _ * scaling ) },
      dashOffset  = dashOffset. map {        _ * scaling },
      corners     = corners.    map { _ match {
        case StrokeCorners.Exact(clipUnder) =>
          StrokeCorners.Exact(clipUnder.map {       _ * scaling })
        case currVal                  => currVal
      } }
    )


  def comboPen(other: Pen): Pen = InkPen(
      other.stroke.     orElse(stroke),
      other.fill.       orElse(fill),
      other.width.      orElse(width),
      other.dashPattern.orElse(dashPattern),
      other.dashOffset. orElse(dashOffset),
      other.ends.       orElse(ends),
      other.corners.    orElse(corners),
      other.fillRule.   orElse(fillRule)
    )


  def exhibitPen(opacity: Opacity): Pen = copy(
      stroke = stroke.map { _ alpha opacity },
      fill   = fill.  map { _ alpha opacity }
    )


  private def decompose(ink: Ink): (Option[Ink], Option[Double]) = ink match {
    case NonOpaqueInk(ink, opacity) => (Some(ink), Some(opacity))
    case ink: Ink                   => (Some(ink), None)
  }
}


object PenTransform {

  private class Extension private[this] (
      composedTransform: Pen => Pen,
      protected val inner: PenTransform
    ) extends AdjustPen(composedTransform) {

    def this(transformPen: PenTransform, subsequentTransform: Pen => Pen) =
      this(transformPen.compose(subsequentTransform), transformPen)

    override def isReplacement: Boolean = inner.isReplacement

    override protected def compose(subsequentTransform: Pen => Pen):
        Pen => Pen =
      inner.compose(subsequentTransform)

    override protected def createComposedPen(child: PenTransform):
        PenTransform =
      inner.createComposedPen(child)
  }


  private class Composition (
      transformPen: PenTransform,
      subsequentTransform: Pen => Pen
    ) extends Extension(transformPen, subsequentTransform) {

    override val stroke      = inner.stroke
    override val fill        = inner.fill
    override val width       = inner.width
    override val dashPattern = inner.dashPattern
    override val dashOffset  = inner.dashOffset
    override val ends        = inner.ends
    override val corners     = inner.corners
    override val fillRule    = inner.fillRule
  }
}

sealed abstract class PenTransform extends Pen {

  val stroke:      Option[Ink]           = None
  val fill:        Option[Ink]           = None
  val width:       Option[Double]        = None
  val dashPattern: Option[List[Double]]  = None
  val dashOffset:  Option[Double]        = None
  val ends:        Option[StrokeEnds]    = None
  val corners:     Option[StrokeCorners] = None
  val fillRule:    Option[FillRule]      = None


  def apply(pen: Pen): Pen

  final def compose(transformPen: PenTransform): PenTransform =
    createComposedPen(transformPen)

  def isReplacement: Boolean = false


  def extractStroke: Pen = new PenTransform.Composition(this, _.extractStroke) {
    override val fill      = None
    override val fillRule = None
  }

  def extractFill:   Pen = new PenTransform.Extension(this, _.extractFill) {
    override val fill      = PenTransform.this.fill
    override val fillRule = PenTransform.this.fillRule
  }


  def flipInk: Pen = new PenTransform.Composition(this, _.flipInk) {
    override val stroke = PenTransform.this.fill
    override val fill   = PenTransform.this.stroke
  }

  def flipOpacity: Pen = new PenTransform.Composition(this, _.flipOpacity) {
    override val stroke = PenTransform.this.flipOpacity.stroke
    override val fill   = PenTransform.this.flipOpacity.fill
  }

  def flipInkNopacity: Pen =
    new PenTransform.Composition(this, _.flipInkNopacity) {
      override val stroke = PenTransform.this.flipInkNopacity.stroke
      override val fill   = PenTransform.this.flipInkNopacity.fill
    }


  def dashed(dashPattern0: List[Double]): Pen =
    new PenTransform.Composition(this, _.dashed(dashPattern0)) {
      override val dashPattern = Some(dashPattern0)
      override val dashOffset  = None
    }

  def dashed(dashPattern0: List[Double], dashOffset0: Double): Pen =
    new PenTransform.Composition(this, _.dashed(dashPattern0, dashOffset0)) {
      override val dashPattern = Some(dashPattern0)
      override val dashOffset  = Some(dashOffset0)
    }


  def stroke(ink: Ink): Pen =
    new PenTransform.Composition(this, _.stroke(ink)) {
      override val stroke      = Some(ink)
      override val width       = None
      override val dashPattern = None
      override val dashOffset  = None
      override val ends        = None
      override val corners     = None
    }

  def stroke(ink: Ink, ends0: StrokeEnds): Pen =
    new PenTransform.Composition(this, _.stroke(ink, ends0)) {
      override val stroke      = Some(ink)
      override val width       = None
      override val dashPattern = None
      override val dashOffset  = None
      override val ends        = Some(ends0)
      override val corners     = None
    }

  def stroke(ink: Ink, corners0: StrokeCorners): Pen =
    new PenTransform.Composition(this, _.stroke(ink, corners0)) {
      override val stroke      = Some(ink)
      override val width       = None
      override val dashPattern = None
      override val dashOffset  = None
      override val ends        = None
      override val corners     = Some(corners0)
    }

  def stroke(ink: Ink, ends0: StrokeEnds, corners0: StrokeCorners): Pen =
    new PenTransform.Composition(this, _.stroke(ink, ends0, corners0)) {
      override val stroke      = Some(ink)
      override val width       = None
      override val dashPattern = None
      override val dashOffset  = None
      override val ends        = Some(ends0)
      override val corners     = Some(corners0)
    }

  def stroke(ink: Ink, width0: Double): Pen =
    new PenTransform.Composition(this, _.stroke(ink, width0)) {
      override val stroke      = Some(ink)
      override val width       = Some(width0)
      override val dashPattern = None
      override val dashOffset  = None
      override val ends        = None
      override val corners     = None
    }

  def stroke(ink: Ink, width0: Double, ends0: StrokeEnds): Pen =
    new PenTransform.Composition(this, _.stroke(ink, width0, ends0)) {
      override val stroke      = Some(ink)
      override val width       = Some(width0)
      override val dashPattern = None
      override val dashOffset  = None
      override val ends        = Some(ends0)
      override val corners     = None
    }

  def stroke(ink: Ink, width0: Double, corners0: StrokeCorners): Pen =
    new PenTransform.Composition(this, _.stroke(ink, width0, corners0)) {
      override val stroke      = Some(ink)
      override val width       = Some(width0)
      override val dashPattern = None
      override val dashOffset  = None
      override val ends        = None
      override val corners     = Some(corners0)
    }

  def stroke(ink: Ink, width0: Double,
             ends0: StrokeEnds, corners0: StrokeCorners): Pen =
    new PenTransform.Composition(this, _.stroke(ink, width0, ends0,corners0)) {
      override val stroke      = Some(ink)
      override val width       = Some(width0)
      override val dashPattern = None
      override val dashOffset  = None
      override val ends        = Some(ends0)
      override val corners     = Some(corners0)
    }

  def stroke(like: Pen): Pen =
    new PenTransform.Composition(this, _.stroke(like)) {
      override val stroke      = like.stroke
      override val width       = like.width
      override val dashPattern = like.dashPattern
      override val dashOffset  = like.dashOffset
      override val ends        = like.ends
      override val corners     = like.corners
    }


  def fill(ink: Ink): Pen =
    new PenTransform.Composition(this, _.fill(ink)) {
      override val fill     = Some(ink)
      override val fillRule = None
    }

  def fill(ink: Ink, fillRule0: FillRule): Pen =
    new PenTransform.Composition(this, _.fill(ink, fillRule0)) {
      override val fill     = Some(ink)
      override val fillRule = Some(fillRule0)
    }
  
  def fill(like: Pen): Pen = new PenTransform.Composition(this, _.fill(like)) {
      override val fill     = like.fill
      override val fillRule = like.fillRule
    }


  def scalePen(scaling: Double): Pen =
    new PenTransform.Composition(this, _.scalePen(scaling)) {
      override val width =
          PenTransform.this.width.map { _ * scaling }
      override val dashPattern =
          PenTransform.this.dashPattern.map { _.map { _ * scaling } }
      override val dashOffset =
          PenTransform.this.dashOffset.map { _ * scaling }
      override val corners =
          PenTransform.this.corners.map { _ match {
            case StrokeCorners.Exact(clipUnder) =>
              StrokeCorners.Exact(clipUnder.map { _ * scaling })
            case currVal                  => currVal
          } }
    }

  def comboPen(other: Pen): Pen =
    new PenTransform.Composition(this, _.comboPen(other)) {
      override val stroke =
        other.stroke.     orElse(PenTransform.this.stroke)
      override val fill =
        other.fill.       orElse(PenTransform.this.fill)
      override val width =
        other.width.      orElse(PenTransform.this.width)
      override val dashPattern =
        other.dashPattern.orElse(PenTransform.this.dashPattern)
      override val dashOffset =
        other.dashOffset. orElse(PenTransform.this.dashOffset)
      override val ends =
        other.ends.       orElse(PenTransform.this.ends)
      override val corners =
        other.corners.    orElse(PenTransform.this.corners)
      override val fillRule =
        other.fillRule.   orElse(PenTransform.this.fillRule)
    }

  def exhibitPen(opacity: Opacity): Pen =
    new PenTransform.Composition(this, _.exhibitPen(opacity)) {
      override val stroke = PenTransform.this.stroke.map( _ alpha opacity )
      override val fill   = PenTransform.this.fill.  map( _ alpha opacity )
    }


  protected[ink] def transform: Pen => Pen

  // used when 'extending' a PenTransform derived class instance
  protected def compose(subsequentTransform: Pen => Pen): Pen => Pen

  // used when combining another PenTransform instance with `this` (as a child)
  protected def createComposedPen(child: PenTransform): PenTransform
}


object AdjustPen extends PenTransform {

  // nothing to adjust yet...
  def apply(pen: Pen): Pen = pen


  def apply(adjustment: Pen => Pen): AdjustPen = new AdjustPen(adjustment)


  protected[ink] val transform = identity[Pen] _

  protected def compose(subsequentTransform: Pen => Pen): Pen => Pen =
    subsequentTransform

  protected def createComposedPen(child: PenTransform): PenTransform =
    child // AdjustPen is 'identity' / stand-in for configuring adjustments
}

sealed class AdjustPen(adjustment: Pen => Pen) extends PenTransform {

  final def apply(pen: Pen): Pen = pen match {
    case p        : InkPen       => adjustment(p)
    case transform: PenTransform => compose(transform)
  }


  protected[ink] val transform = adjustment

  protected def compose(subsequentTransform: Pen => Pen): Pen => Pen =
    subsequentTransform.compose(adjustment)

  protected def createComposedPen(child: PenTransform): PenTransform =
    AdjustPen(adjustment.compose(child.transform))
}


object ReplacePen extends AdjustPen( _ => Pen.invisible ) {

  override def isReplacement: Boolean = true

  override protected def compose(subsequentTransform: Pen => Pen): Pen => Pen =
    _ => Pen.invisible

  override protected def createComposedPen(child: PenTransform): PenTransform =
    ReplacePen
}
