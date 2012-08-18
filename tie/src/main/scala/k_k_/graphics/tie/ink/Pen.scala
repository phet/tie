/*
   file: k_k_/graphics/tie/ink/Pen.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie.ink

import k_k_.graphics.tie.effects.{Opacity_Effect, Opacity}


sealed abstract class Stroke_Ends
case object Exact_Ends   extends Stroke_Ends
case object Round_Ends   extends Stroke_Ends
case object Extend_Ends  extends Stroke_Ends
case object Inherit_Ends extends Stroke_Ends


sealed abstract class Stroke_Corners
case object Exact_Corners                             extends Stroke_Corners {
  def apply(clip_under: Double): Exact_Corners =
    Exact_Corners(Some(clip_under))
}
// `clip_under` is ratio lower than which Exact_Corners appears as Clip_Corners,
// so as to keep small angle corners from poking out and looking more like a
// an end; defined as: clip_under = stroke-width / sin(angle-theta / 2)
// (equiv. to: attribute 'stroke-miterlimit' in SVG 1.1)
case class  Exact_Corners(clip_under: Option[Double]) extends Stroke_Corners
case object Round_Corners                             extends Stroke_Corners
case object Clip_Corners                              extends Stroke_Corners
case object Inherit_Corners                           extends Stroke_Corners


sealed abstract class Fill_Rule
case object Non_Zero_Fill      extends Fill_Rule // default
case object Even_Odd_Fill      extends Fill_Rule
// NOTE: expected to be included in SVG 2.0; until then, equiv. to Non_Zero_Fill
case object Winding_Count_Fill extends Fill_Rule
case object Inherit_Fill       extends Fill_Rule


object Pen {

  def stroke(ink: Ink): Pen =
    Ink_Pen(Some(ink), None)

  def stroke(ink: Ink, ends: Stroke_Ends): Pen =
    Ink_Pen(Some(ink), None, ends = Some(ends))

  def stroke(ink: Ink, corners: Stroke_Corners): Pen =
    Ink_Pen(Some(ink), None, corners = Some(corners))

  def stroke(ink: Ink, ends: Stroke_Ends, corners: Stroke_Corners): Pen =
    Ink_Pen(Some(ink), None,
            ends    = Some(ends),
            corners = Some(corners))


  def stroke(ink: Ink, width: Double): Pen =
    Ink_Pen(Some(ink), None, Some(width))

  def stroke(ink: Ink, width: Double, ends: Stroke_Ends): Pen =
    Ink_Pen(Some(ink), None, Some(width), ends = Some(ends))

  def stroke(ink: Ink, width: Double, corners: Stroke_Corners): Pen =
    Ink_Pen(Some(ink), None, Some(width), corners = Some(corners))

  def stroke(ink: Ink, width: Double,
             ends: Stroke_Ends, corners: Stroke_Corners): Pen =
    Ink_Pen(Some(ink), None, Some(width),
            ends    = Some(ends),
            corners = Some(corners))


  def fill(ink: Ink): Pen =
    Ink_Pen(None, Some(ink))

  def fill(ink: Ink, fill_rule: Fill_Rule): Pen =
    Ink_Pen(None, Some(ink), fill_rule = Some(fill_rule))


  def dashed(ink: Ink, dash_pattern: List[Double]): Pen =
    Ink_Pen(Some(ink), None, None, Some(dash_pattern))

  def dashed(ink: Ink, dash_pattern: List[Double], ends: Stroke_Ends): Pen =
    Ink_Pen(Some(ink), None, None, Some(dash_pattern),
            ends = Some(ends))

  def dashed(ink: Ink, dash_pattern: List[Double],
             corners: Stroke_Corners): Pen =
    Ink_Pen(Some(ink), None, None, Some(dash_pattern),
            corners = Some(corners))

  def dashed(ink: Ink, dash_pattern: List[Double],
             ends: Stroke_Ends, corners: Stroke_Corners): Pen =
    Ink_Pen(Some(ink), None, None, Some(dash_pattern),
            ends    = Some(ends),
            corners = Some(corners))


  def dashed(ink: Ink, dash_pattern: List[Double], dash_offset: Double): Pen =
    Ink_Pen(Some(ink), None, None, Some(dash_pattern), Some(dash_offset))

  def dashed(ink: Ink, dash_pattern: List[Double], dash_offset: Double,
             ends: Stroke_Ends): Pen =
    Ink_Pen(Some(ink), None, None, Some(dash_pattern), Some(dash_offset),
            ends = Some(ends))

  def dashed(ink: Ink, dash_pattern: List[Double], dash_offset: Double,
             corners: Stroke_Corners): Pen =
    Ink_Pen(Some(ink), None, None, Some(dash_pattern), Some(dash_offset),
            corners = Some(corners))

  def dashed(ink: Ink, dash_pattern: List[Double], dash_offset: Double,
             ends: Stroke_Ends, corners: Stroke_Corners): Pen =
    Ink_Pen(Some(ink), None, None, Some(dash_pattern), Some(dash_offset),
            ends    = Some(ends),
            corners = Some(corners))


  def dashed(ink: Ink, dash_length: Double): Pen =
    dashed(ink, List(dash_length, dash_length))

  def dashed(ink: Ink, dash_length: Double, ends: Stroke_Ends): Pen =
    dashed(ink, List(dash_length, dash_length), ends)

  def dashed(ink: Ink, dash_length: Double, corners: Stroke_Corners):
      Pen =
    dashed(ink, List(dash_length, dash_length), corners)

  def dashed(ink: Ink, dash_length: Double,
             ends: Stroke_Ends, corners: Stroke_Corners): Pen =
    dashed(ink, List(dash_length, dash_length), ends, corners)


  def dashed(ink: Ink, width: Double, dash_pattern: List[Double]): Pen =
    Ink_Pen(Some(ink), None, Some(width), Some(dash_pattern))

  def dashed(ink: Ink, width: Double, dash_pattern: List[Double],
             ends: Stroke_Ends): Pen =
    Ink_Pen(Some(ink), None, Some(width), Some(dash_pattern),
            ends = Some(ends))

  def dashed(ink: Ink, width: Double, dash_pattern: List[Double],
             corners: Stroke_Corners): Pen =
    Ink_Pen(Some(ink), None, Some(width), Some(dash_pattern),
            corners = Some(corners))

  def dashed(ink: Ink, width: Double, dash_pattern: List[Double],
             ends: Stroke_Ends, corners: Stroke_Corners): Pen =
    Ink_Pen(Some(ink), None, Some(width), Some(dash_pattern),
            ends    = Some(ends),
            corners = Some(corners))


  def dashed(ink: Ink, width: Double,
             dash_pattern: List[Double], dash_offset: Double): Pen =
    Ink_Pen(Some(ink), None, Some(width), Some(dash_pattern), Some(dash_offset))

  def dashed(ink: Ink, width: Double,
             dash_pattern: List[Double], dash_offset: Double,
             ends: Stroke_Ends): Pen =
    Ink_Pen(Some(ink), None, Some(width), Some(dash_pattern), Some(dash_offset),
            ends = Some(ends))

  def dashed(ink: Ink, width: Double,
             dash_pattern: List[Double], dash_offset: Double,
             corners: Stroke_Corners): Pen =
    Ink_Pen(Some(ink), None, Some(width), Some(dash_pattern), Some(dash_offset),
            corners = Some(corners))

  def dashed(ink: Ink, width: Double,
             dash_pattern: List[Double], dash_offset: Double,
             ends: Stroke_Ends, corners: Stroke_Corners): Pen =
    Ink_Pen(Some(ink), None, Some(width), Some(dash_pattern), Some(dash_offset),
            ends    = Some(ends),
            corners = Some(corners))


  def dashed(ink: Ink, width: Double,
             dash_length: Double, dash_offset: Double): Pen =
    dashed(ink, width, List(dash_length, dash_length), dash_offset)

  def dashed(ink: Ink, width: Double,
             dash_length: Double, dash_offset: Double,
             ends: Stroke_Ends): Pen =
    dashed(ink, width, List(dash_length, dash_length), dash_offset,
           ends)

  def dashed(ink: Ink, width: Double,
             dash_length: Double, dash_offset: Double,
             corners: Stroke_Corners): Pen =
    dashed(ink, width, List(dash_length, dash_length), dash_offset,
           corners)

  def dashed(ink: Ink, width: Double,
             dash_length: Double, dash_offset: Double,
             ends: Stroke_Ends, corners: Stroke_Corners): Pen =
    dashed(ink, width, List(dash_length, dash_length), dash_offset,
           ends, corners)


  lazy val invisible =
    new Ink_Pen(Null_Ink, Null_Ink)


  def apply(stroke: Ink, fill: Ink) =
    new Ink_Pen(stroke, fill)

  def apply(stroke: Ink, fill: Ink, ends: Stroke_Ends): Pen =
    new Ink_Pen(stroke, fill, ends = ends)

  def apply(stroke: Ink, fill: Ink, corners: Stroke_Corners): Pen =
    new Ink_Pen(stroke, fill, corners = corners)

  def apply(stroke: Ink, fill: Ink,
            ends: Stroke_Ends, corners: Stroke_Corners): Pen =
    new Ink_Pen(stroke, fill, ends = ends, corners = corners)

  def apply(stroke: Ink, fill: Ink, fill_rule: Fill_Rule) =
    new Ink_Pen(stroke, fill, fill_rule = fill_rule)

  def apply(stroke: Ink, fill: Ink,
            ends: Stroke_Ends, corners: Stroke_Corners,
            fill_rule: Fill_Rule) =
    new Ink_Pen(stroke, fill,
                ends = ends, corners = corners,
                fill_rule = fill_rule)


  def apply(stroke: Ink, fill: Ink, width: Double) =
    new Ink_Pen(stroke, fill, width)

  def apply(stroke: Ink, fill: Ink, width: Double,
            ends: Stroke_Ends): Pen =
    new Ink_Pen(stroke, fill, width, ends)

  def apply(stroke: Ink, fill: Ink, width: Double,
            corners: Stroke_Corners): Pen =
    new Ink_Pen(stroke, fill, width, corners)

  def apply(stroke: Ink, fill: Ink, width: Double,
            ends: Stroke_Ends, corners: Stroke_Corners): Pen =
    new Ink_Pen(stroke, fill, width, ends, corners)

  def apply(stroke: Ink, fill: Ink, width: Double,
           fill_rule: Fill_Rule) =
    new Ink_Pen(stroke, fill, width, fill_rule)

  def apply(stroke: Ink, fill: Ink, width: Double,
            ends: Stroke_Ends, corners: Stroke_Corners,
            fill_rule: Fill_Rule) =
    new Ink_Pen(stroke, fill, width, ends, corners,
                fill_rule)
}


sealed abstract class Pen {

  // accessors:
  def stroke:       Option[Ink]
  def fill:         Option[Ink]
  def width:        Option[Double]
  def dash_pattern: Option[List[Double]]
  def dash_offset:  Option[Double]
  def ends:         Option[Stroke_Ends]
  def corners:      Option[Stroke_Corners]
  def fill_rule:    Option[Fill_Rule]


  // modifiers:
  def extract_stroke: Pen
  def extract_fill:   Pen

  def flip_ink: Pen
  def flip_opacity: Pen
  def flip_ink_nopacity: Pen


  // `dashed` methods shall have no effect on other stroke properties
  def dashed(dash_pattern: List[Double]): Pen
  def dashed(dash_pattern: List[Double], dash_offset: Double): Pen

  def dashed(dash_length: Double): Pen =
    dashed(List(dash_length, dash_length))

  def dashed(dash_length: Double, dash_offset: Double): Pen =
    dashed(List(dash_length, dash_length), dash_offset)


  def stroke(ink: Ink): Pen
  def stroke(ink: Ink, ends: Stroke_Ends): Pen
  def stroke(ink: Ink, corners: Stroke_Corners): Pen
  def stroke(ink: Ink, ends: Stroke_Ends, corners: Stroke_Corners): Pen
  def stroke(ink: Ink, width: Double): Pen
  def stroke(ink: Ink, width: Double, ends: Stroke_Ends): Pen
  def stroke(ink: Ink, width: Double, corners: Stroke_Corners): Pen
  def stroke(ink: Ink, width: Double,
             ends: Stroke_Ends, corners: Stroke_Corners): Pen
  def stroke(like: Pen): Pen


  def fill(ink: Ink): Pen
  def fill(ink: Ink, fill_rule: Fill_Rule): Pen
  def fill(like: Pen): Pen


  def scale_pen(scaling: Double): Pen
  def ~*(scaling: Double): Pen = scale_pen(scaling)


  def combo_pen(other: Pen): Pen
  def ~&(other: Pen): Pen = combo_pen(other)


  def exhibit_pen(opacity: Opacity_Effect): Pen
  def exhibit_pen(opacity: Double): Pen = exhibit_pen(Opacity(opacity))
  def alpha(opacity: Opacity_Effect): Pen = exhibit_pen(opacity)
  def alpha(opacity: Double): Pen = exhibit_pen(opacity)
  def ~#(opacity: Opacity_Effect): Pen = exhibit_pen(opacity)
  def ~#(opacity: Double): Pen = exhibit_pen(opacity)
}


final case class Ink_Pen protected[ink] (
  stroke:       Option[Ink],
  fill:         Option[Ink],
  width:        Option[Double]         = None,
  dash_pattern: Option[List[Double]]   = None,
  dash_offset:  Option[Double]         = None,
  ends:         Option[Stroke_Ends]    = None,
  corners:      Option[Stroke_Corners] = None,
  fill_rule:    Option[Fill_Rule]      = None
  ) extends Pen {

  def this(stroke: Ink, fill: Ink) =
    this(Some(stroke), Some(fill))

  def this(stroke: Ink, fill: Ink, ends: Stroke_Ends) =
    this(Some(stroke), Some(fill), ends = Some(ends))

  def this(stroke: Ink, fill: Ink, corners: Stroke_Corners) =
    this(Some(stroke), Some(fill), corners = Some(corners))

  def this(stroke: Ink, fill: Ink, ends: Stroke_Ends, corners: Stroke_Corners) =
    this(Some(stroke), Some(fill), ends = Some(ends), corners = Some(corners))

  def this(stroke: Ink, fill: Ink, fill_rule: Fill_Rule) =
    this(Some(stroke), Some(fill), fill_rule = Some(fill_rule))

  def this(stroke: Ink, fill: Ink, ends: Stroke_Ends, corners: Stroke_Corners,
           fill_rule: Fill_Rule) =
    this(Some(stroke), Some(fill), None, None, None,
         Some(ends), Some(corners), Some(fill_rule))


  def this(stroke: Ink, fill: Ink, width: Double) =
    this(Some(stroke), Some(fill), Some(width))

  def this(stroke: Ink, fill: Ink, width: Double, ends: Stroke_Ends) =
    this(Some(stroke), Some(fill), Some(width), ends = Some(ends))

  def this(stroke: Ink, fill: Ink, width: Double, corners: Stroke_Corners) =
    this(Some(stroke), Some(fill), Some(width), corners = Some(corners))

  def this(stroke: Ink, fill: Ink, width: Double,
           ends: Stroke_Ends, corners: Stroke_Corners) =
    this(Some(stroke), Some(fill), Some(width),
         ends = Some(ends), corners = Some(corners))

  def this(stroke: Ink, fill: Ink, width: Double, fill_rule: Fill_Rule) =
    this(Some(stroke), Some(fill), Some(width), fill_rule = Some(fill_rule))

  def this(stroke: Ink, fill: Ink, width: Double,
           ends: Stroke_Ends, corners: Stroke_Corners,
           fill_rule: Fill_Rule) =
    this(Some(stroke), Some(fill), Some(width), None, None,
         Some(ends), Some(corners), Some(fill_rule))


  def this(stroke: Ink, fill: Ink, width: Double,
           dash_pattern: List[Double], dash_offset: Double,
           ends: Stroke_Ends) =
    this(Some(stroke), Some(fill), Some(width),
         Some(dash_pattern), Some(dash_offset),
         ends = Some(ends))

  def this(stroke: Ink, fill: Ink, width: Double,
           dash_pattern: List[Double], dash_offset: Double,
           corners: Stroke_Corners) =
    this(Some(stroke), Some(fill), Some(width),
         Some(dash_pattern), Some(dash_offset),
         corners = Some(corners))

  def this(stroke: Ink, fill: Ink, width: Double,
           dash_pattern: List[Double], dash_offset: Double,
           ends: Stroke_Ends, corners: Stroke_Corners) =
    this(Some(stroke), Some(fill), Some(width),
         Some(dash_pattern), Some(dash_offset),
         Some(ends), Some(corners))

  def this(stroke: Ink, fill: Ink, width: Double,
           dash_pattern: List[Double], dash_offset: Double,
           fill_rule: Fill_Rule) =
    this(Some(stroke), Some(fill), Some(width),
         Some(dash_pattern), Some(dash_offset),
         None, None, Some(fill_rule))

  def this(stroke: Ink, fill: Ink, width: Double,
           dash_pattern: List[Double], dash_offset: Double,
           ends: Stroke_Ends, corners: Stroke_Corners,
           fill_rule: Fill_Rule) =
    this(Some(stroke), Some(fill), Some(width),
         Some(dash_pattern), Some(dash_offset),
         Some(ends), Some(corners), Some(fill_rule))


  def this(stroke: Ink, fill: Ink, width: Double,
           dash_length: Double, dash_offset: Double,
           ends: Stroke_Ends) =
    this(Some(stroke), Some(fill), Some(width),
         Some(List(dash_length, dash_length)),
         Some(dash_offset),
         ends = Some(ends))

  def this(stroke: Ink, fill: Ink, width: Double,
           dash_length: Double, dash_offset: Double,
           corners: Stroke_Corners) =
    this(Some(stroke), Some(fill), Some(width),
         Some(List(dash_length, dash_length)),
         Some(dash_offset),
         corners = Some(corners))

  def this(stroke: Ink, fill: Ink, width: Double,
           dash_length: Double, dash_offset: Double,
           ends: Stroke_Ends, corners: Stroke_Corners) =
    this(Some(stroke), Some(fill), Some(width),
         Some(List(dash_length, dash_length)),
         Some(dash_offset),
         Some(ends), Some(corners))

  def this(stroke: Ink, fill: Ink, width: Double,
           dash_length: Double, dash_offset: Double,
           fill_rule: Fill_Rule) =
    this(Some(stroke), Some(fill), Some(width),
         Some(List(dash_length, dash_length)),
         Some(dash_offset),
         None, None, Some(fill_rule))

  def this(stroke: Ink, fill: Ink, width: Double,
           dash_length: Double, dash_offset: Double,
           ends: Stroke_Ends, corners: Stroke_Corners,
           fill_rule: Fill_Rule) =
    this(Some(stroke), Some(fill), Some(width),
         Some(List(dash_length, dash_length)),
         Some(dash_offset),
         Some(ends), Some(corners), Some(fill_rule))


  private def this() = this(None, None)


  val stroke_opacity: Option[Double] = stroke.map( _.opacity )
  val fill_opacity:   Option[Double] = fill.  map( _.opacity )


  def extract_stroke: Pen = new Ink_Pen().stroke(this)
  def extract_fill:   Pen = new Ink_Pen().fill(this)


  def flip_ink: Pen = copy(stroke = fill, fill = stroke)

  def flip_opacity: Pen = {
    val (base_stroke, stroke_opacity) =
          stroke.map( decompose(_) ).getOrElse((None, None))
    val (base_fill,   fill_opacity)   =
          fill.  map( decompose(_) ).getOrElse((None, None))
    copy(stroke = base_stroke.map( _ alpha fill_opacity.  getOrElse(1.0) ),
         fill   = base_fill.  map( _ alpha stroke_opacity.getOrElse(1.0) ))
  }

  def flip_ink_nopacity: Pen = {
    val (base_stroke, stroke_opacity) =
          stroke.map( decompose(_) ).getOrElse((None, None))
    val (base_fill,   fill_opacity)   =
          fill.  map( decompose(_) ).getOrElse((None, None))
    copy(stroke = base_fill.  map( _ alpha stroke_opacity.getOrElse(1.0) ),
         fill   = base_stroke.map( _ alpha fill_opacity.  getOrElse(1.0) ))
  }


  def dashed(dash_pattern: List[Double]): Pen =
    copy(dash_pattern = Some(dash_pattern),
         dash_offset  = None)

  def dashed(dash_pattern: List[Double], dash_offset: Double): Pen =
    copy(dash_pattern = Some(dash_pattern),
         dash_offset  = Some(dash_offset))


  def stroke(ink: Ink): Pen =
    new Ink_Pen(Some(ink), fill,
                width        = None,
                dash_pattern = None,
                dash_offset  = None,
                ends         = None,
                corners      = None,
                fill_rule    = fill_rule)

  def stroke(ink: Ink, ends: Stroke_Ends): Pen =
    new Ink_Pen(Some(ink), fill,
                width        = None,
                dash_pattern = None,
                dash_offset  = None,
                ends         = Some(ends),
                corners      = None,
                fill_rule    = fill_rule)

  def stroke(ink: Ink, corners: Stroke_Corners): Pen =
    new Ink_Pen(Some(ink), fill,
                width        = None,
                dash_pattern = None,
                dash_offset  = None,
                ends         = None,
                corners      = Some(corners),
                fill_rule    = fill_rule)

  def stroke(ink: Ink,
             ends: Stroke_Ends, corners: Stroke_Corners): Pen =
    new Ink_Pen(Some(ink), fill,
                width        = None,
                dash_pattern = None,
                dash_offset  = None,
                ends         = Some(ends),
                corners      = Some(corners),
                fill_rule    = fill_rule)


  def stroke(ink: Ink, width: Double): Pen =
    new Ink_Pen(Some(ink), fill, Some(width),
                dash_pattern = None,
                dash_offset  = None,
                ends         = None,
                corners      = None,
                fill_rule    = fill_rule)

  def stroke(ink: Ink, width: Double, ends: Stroke_Ends): Pen =
    new Ink_Pen(Some(ink), fill, Some(width),
                dash_pattern = None,
                dash_offset  = None,
                ends         = Some(ends),
                corners      = None,
                fill_rule    = fill_rule)

  def stroke(ink: Ink, width: Double, corners: Stroke_Corners): Pen =
    new Ink_Pen(Some(ink), fill, Some(width),
                dash_pattern = None,
                dash_offset  = None,
                ends         = None,
                corners      = Some(corners),
                fill_rule    = fill_rule)

  def stroke(ink: Ink, width: Double,
             ends: Stroke_Ends, corners: Stroke_Corners): Pen =
    new Ink_Pen(Some(ink), fill, Some(width),
                dash_pattern = None,
                dash_offset  = None,
                ends         = Some(ends),
                corners      = Some(corners),
                fill_rule    = fill_rule)


  def stroke(like: Pen): Pen = copy(
      stroke       = like.stroke,
      width        = like.width,
      dash_pattern = like.dash_pattern,
      dash_offset  = like.dash_offset,
      ends         = like.ends,
      corners      = like.corners
    )


  def fill(ink: Ink): Pen =
    copy(fill = Some(ink), fill_rule = None)

  def fill(ink: Ink, fill_rule: Fill_Rule): Pen =
    copy(fill = Some(ink), fill_rule = Some(fill_rule))

  def fill(like: Pen): Pen =
    copy(fill = like.fill, fill_rule = like.fill_rule)


  def scale_pen(scaling: Double): Pen =
    copy(width        = width.       map(        _ * scaling ),
         dash_pattern = dash_pattern.map( _.map( _ * scaling ) ),
         dash_offset  = dash_offset. map(        _ * scaling ),
         corners      = corners.     map( _ match {
           case Exact_Corners(clip_under) =>
                                Exact_Corners(clip_under. map( _ * scaling ))
           case curr_val                  => curr_val
         } ))


  def combo_pen(other: Pen): Pen =
    Ink_Pen(other.stroke.      orElse(stroke),
            other.fill.        orElse(fill),
            other.width.       orElse(width),
            other.dash_pattern.orElse(dash_pattern),
            other.dash_offset. orElse(dash_offset),
            other.ends.        orElse(ends),
            other.corners.     orElse(corners),
            other.fill_rule.   orElse(fill_rule))


  def exhibit_pen(opacity: Opacity_Effect): Pen =
    copy(stroke = stroke.map( _ alpha opacity ),
         fill   = fill.  map( _ alpha opacity ))


  private def decompose(ink: Ink): (Option[Ink], Option[Double]) =
    ink match {
      case Non_Opaque_Ink(ink, opacity) => (Some(ink), Some(opacity))
      case ink: Ink                     => (Some(ink), None)
    }
}


object Pen_Transform {

  private class Extension private[this] (
    composed_transform: Pen => Pen,
    protected val inner: Pen_Transform
    ) extends Adjust_Pen(composed_transform) {

    def this(transform_pen: Pen_Transform, subsequent_transform: Pen => Pen) =
      this(transform_pen.compose(subsequent_transform), transform_pen)

    override
    def is_replacement: Boolean = inner.is_replacement

    override
    protected def compose(subsequent_transform: Pen => Pen): Pen => Pen =
      inner.compose(subsequent_transform)

    override
    protected def create_composed_pen(child: Pen_Transform): Pen_Transform =
      inner.create_composed_pen(child)
  }


  private class Composition (
    transform_pen: Pen_Transform,
    subsequent_transform: Pen => Pen
    ) extends Extension(transform_pen, subsequent_transform) {

    override val stroke       = inner.stroke
    override val fill         = inner.fill
    override val width        = inner.width
    override val dash_pattern = inner.dash_pattern
    override val dash_offset  = inner.dash_offset
    override val ends         = inner.ends
    override val corners      = inner.corners
    override val fill_rule    = inner.fill_rule
  }
}

sealed abstract class Pen_Transform extends Pen {

  val stroke:       Option[Ink]            = None
  val fill:         Option[Ink]            = None
  val width:        Option[Double]         = None
  val dash_pattern: Option[List[Double]]   = None
  val dash_offset:  Option[Double]         = None
  val ends:         Option[Stroke_Ends]    = None
  val corners:      Option[Stroke_Corners] = None
  val fill_rule:    Option[Fill_Rule]      = None


  def apply(pen: Pen): Pen

  final def compose(transform_pen: Pen_Transform): Pen_Transform =
    create_composed_pen(transform_pen)

  def is_replacement: Boolean = false


  def extract_stroke: Pen =
    new Pen_Transform.Composition(this, _.extract_stroke) {
      override val fill      = None
      override val fill_rule = None
    }

  def extract_fill:   Pen =
    new Pen_Transform.Extension(this, _.extract_fill) {
      override val fill      = Pen_Transform.this.fill
      override val fill_rule = Pen_Transform.this.fill_rule
    }


  def flip_ink: Pen =
    new Pen_Transform.Composition(this, _.flip_ink) {
      override val stroke = Pen_Transform.this.fill
      override val fill   = Pen_Transform.this.stroke
    }

  def flip_opacity: Pen =
    new Pen_Transform.Composition(this, _.flip_opacity) {
      override val stroke = Pen_Transform.this.flip_opacity.stroke
      override val fill   = Pen_Transform.this.flip_opacity.fill
    }

  def flip_ink_nopacity: Pen =
    new Pen_Transform.Composition(this, _.flip_ink_nopacity) {
      override val stroke = Pen_Transform.this.flip_ink_nopacity.stroke
      override val fill   = Pen_Transform.this.flip_ink_nopacity.fill
    }


  def dashed(dash_pattern0: List[Double]): Pen =
    new Pen_Transform.Composition(this, _.dashed(dash_pattern0)) {
      override val dash_pattern = Some(dash_pattern0)
      override val dash_offset  = None
    }

  def dashed(dash_pattern0: List[Double], dash_offset0: Double): Pen =
    new Pen_Transform.Composition(this, _.dashed(dash_pattern0, dash_offset0)) {
      override val dash_pattern = Some(dash_pattern0)
      override val dash_offset  = Some(dash_offset0)
    }


  def stroke(ink: Ink): Pen =
    new Pen_Transform.Composition(this, _.stroke(ink)) {
      override val stroke       = Some(ink)
      override val width        = None
      override val dash_pattern = None
      override val dash_offset  = None
      override val ends         = None
      override val corners      = None
    }

  def stroke(ink: Ink, ends0: Stroke_Ends): Pen =
    new Pen_Transform.Composition(this, _.stroke(ink, ends0)) {
      override val stroke       = Some(ink)
      override val width        = None
      override val dash_pattern = None
      override val dash_offset  = None
      override val ends         = Some(ends0)
      override val corners      = None
    }

  def stroke(ink: Ink, corners0: Stroke_Corners): Pen =
    new Pen_Transform.Composition(this, _.stroke(ink, corners0)) {
      override val stroke       = Some(ink)
      override val width        = None
      override val dash_pattern = None
      override val dash_offset  = None
      override val ends         = None
      override val corners      = Some(corners0)
    }

  def stroke(ink: Ink,
             ends0: Stroke_Ends, corners0: Stroke_Corners): Pen =
    new Pen_Transform.Composition(this, _.stroke(ink, ends0, corners0)) {
      override val stroke       = Some(ink)
      override val width        = None
      override val dash_pattern = None
      override val dash_offset  = None
      override val ends         = Some(ends0)
      override val corners      = Some(corners0)
    }

  def stroke(ink: Ink, width0: Double): Pen =
    new Pen_Transform.Composition(this, _.stroke(ink, width0)) {
      override val stroke       = Some(ink)
      override val width        = Some(width0)
      override val dash_pattern = None
      override val dash_offset  = None
      override val ends         = None
      override val corners      = None
    }

  def stroke(ink: Ink, width0: Double, ends0: Stroke_Ends): Pen =
    new Pen_Transform.Composition(this, _.stroke(ink, width0, ends0)) {
      override val stroke       = Some(ink)
      override val width        = Some(width0)
      override val dash_pattern = None
      override val dash_offset  = None
      override val ends         = Some(ends0)
      override val corners      = None
    }

  def stroke(ink: Ink, width0: Double, corners0: Stroke_Corners): Pen =
    new Pen_Transform.Composition(this, _.stroke(ink, width0, corners0)) {
      override val stroke       = Some(ink)
      override val width        = Some(width0)
      override val dash_pattern = None
      override val dash_offset  = None
      override val ends         = None
      override val corners      = Some(corners0)
    }

  def stroke(ink: Ink, width0: Double,
             ends0: Stroke_Ends, corners0: Stroke_Corners): Pen =
    new Pen_Transform.Composition(this, _.stroke(ink, width0, ends0,corners0)) {
      override val stroke       = Some(ink)
      override val width        = Some(width0)
      override val dash_pattern = None
      override val dash_offset  = None
      override val ends         = Some(ends0)
      override val corners      = Some(corners0)
    }

  def stroke(like: Pen): Pen =
    new Pen_Transform.Composition(this, _.stroke(like)) {
      override val stroke       = like.stroke
      override val width        = like.width
      override val dash_pattern = like.dash_pattern
      override val dash_offset  = like.dash_offset
      override val ends         = like.ends
      override val corners      = like.corners
    }


  def fill(ink: Ink): Pen =
    new Pen_Transform.Composition(this, _.fill(ink)) {
      override val fill      = Some(ink)
      override val fill_rule = None
    }

  def fill(ink: Ink, fill_rule0: Fill_Rule): Pen =
    new Pen_Transform.Composition(this, _.fill(ink, fill_rule0)) {
      override val fill      = Some(ink)
      override val fill_rule = Some(fill_rule0)
    }
  
  def fill(like: Pen): Pen =
    new Pen_Transform.Composition(this, _.fill(like)) {
      override val fill      = like.fill
      override val fill_rule = like.fill_rule
    }


  def scale_pen(scaling: Double): Pen =
    new Pen_Transform.Composition(this, _.scale_pen(scaling)) {
      override val width        =
              Pen_Transform.this.width.       map( _ * scaling )
      override val dash_pattern =
              Pen_Transform.this.dash_pattern.map( _.map( _ * scaling ) )
      override val dash_offset  =
              Pen_Transform.this.dash_offset. map( _ * scaling )
      override val corners      =
              Pen_Transform.this.corners.     map( _ match {
        case Exact_Corners(clip_under) =>
          Exact_Corners(clip_under.map( _ * scaling ))
        case curr_val                  => curr_val
      } )
    }

  def combo_pen(other: Pen): Pen =
    new Pen_Transform.Composition(this, _.combo_pen(other)) {
      override val stroke       =
        other.stroke.      orElse(Pen_Transform.this.stroke)
      override val fill         =
        other.fill.        orElse(Pen_Transform.this.fill)
      override val width        =
        other.width.       orElse(Pen_Transform.this.width)
      override val dash_pattern =
        other.dash_pattern.orElse(Pen_Transform.this.dash_pattern)
      override val dash_offset  =
        other.dash_offset. orElse(Pen_Transform.this.dash_offset)
      override val ends         =
        other.ends.        orElse(Pen_Transform.this.ends)
      override val corners      =
        other.corners.     orElse(Pen_Transform.this.corners)
      override val fill_rule    =
        other.fill_rule.   orElse(Pen_Transform.this.fill_rule)
    }

  def exhibit_pen(opacity: Opacity_Effect): Pen =
    new Pen_Transform.Composition(this, _.exhibit_pen(opacity)) {
      override val stroke = Pen_Transform.this.stroke.map( _ alpha opacity )
      override val fill   = Pen_Transform.this.fill.  map( _ alpha opacity )
    }


  protected[ink] def transform: Pen => Pen

  // used when 'extending' a Pen_Transform derived class instance
  protected def compose(subsequent_transform: Pen => Pen): Pen => Pen

  // used when combining another Pen_Transform instance with `this` (as a child)
  protected def create_composed_pen(child: Pen_Transform): Pen_Transform
}


object Adjust_Pen extends Pen_Transform {

  // nothing to adjust yet...
  def apply(pen: Pen): Pen = pen


  def apply(adjustment: Pen => Pen): Adjust_Pen = new Adjust_Pen(adjustment)


  protected[ink] val transform = identity[Pen] _

  protected def compose(subsequent_transform: Pen => Pen): Pen => Pen =
    subsequent_transform

  protected def create_composed_pen(child: Pen_Transform): Pen_Transform =
    child // Adjust_Pen is 'identity' / stand-in for configuring adjustments
}

sealed class Adjust_Pen(adjustment: Pen => Pen) extends Pen_Transform {

  final def apply(pen: Pen): Pen =
    pen match {
      case p        : Ink_Pen       => adjustment(p)
      case transform: Pen_Transform => compose(transform)
  }


  protected[ink] val transform = adjustment

  protected def compose(subsequent_transform: Pen => Pen): Pen => Pen =
    subsequent_transform.compose(adjustment)

  protected def create_composed_pen(child: Pen_Transform): Pen_Transform =
    Adjust_Pen(adjustment.compose(child.transform))
}


object Replace_Pen extends Adjust_Pen( _ => Pen.invisible ) {

  override
  def is_replacement: Boolean = true

  override
  protected def compose(subsequent_transform: Pen => Pen): Pen => Pen =
    _ => Pen.invisible

  override
  protected def create_composed_pen(child: Pen_Transform): Pen_Transform =
    Replace_Pen
}
