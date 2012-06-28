/*
   file: k_k_/graphics/tie/Canvas.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie

import k_k_.graphics.tie.effects._
import k_k_.graphics.tie.shapes._


final case class Visible_Area(upper_left: Point, width: Double, height: Double)


sealed abstract class Origin_Positioning
case object Origin_Centered extends Origin_Positioning
case object Origin_Top_Left extends Origin_Positioning


trait Canvas_Spec {

  def width: Double
  def height: Double
  def origin_pos: Origin_Positioning
  def title: String
  def desc: Option[String]

  final def visible_area: Visible_Area = {
    val (w, h) = (math.abs(width), math.abs(height))
    val upper_left = origin_pos match {
      case Origin_Centered => Point(-w/2, -h/2)
      case Origin_Top_Left => Point(0, 0)
    }
    Visible_Area(upper_left, w, h)
  }
}


object Canvas_Props {
  val default_title: String = "Draring"
}

case class Canvas_Props(width: Double, height: Double,
                        origin_pos: Origin_Positioning = Origin_Centered,
                        title: String = Canvas_Props.default_title,
                        desc: Option[String] = None)
    extends Canvas_Spec


// NOTE: add (ignored) arg to primary ctor and make private, in order to
// introduce public one w/ equiv. signature, which, when invoked externally,
// reverses List[Shape], whereas list not reversed for internal invoke
class Canvas private (props: Canvas_Props, val shapes: List[Shape],
                      was_internally_invoked: Boolean) {

  def this(props: Canvas_Props, shapes: List[Shape] = Nil) =
    this(props, shapes reverse, true)

  def this(props: Canvas_Props, shape_args: Shape*) =
    this(props, shape_args.reverse toList, true)


  def place(shape: Shape): Canvas =
    new Canvas(props, shape :: shapes, true)

  def place(shape: Shape, center_x: Double, center_y: Double): Canvas =
    (center_x, center_y) match {
      case (0, 0) => place(shape)
      case _      => place(Translated_Shape(shape, center_x, center_y))
    }


  def exhibit(effect: Effect): Canvas =
    create_effected_canvas(effect)

  def exhibit(opacity: Double): Canvas =
    exhibit(Opacity_Effect(opacity))

  def exhibit(filter: Filter): Canvas =
    exhibit(Filter_Effect(filter))


  def -#(effect: Effect): Canvas =
    exhibit(effect)

  def -#(opacity: Double): Canvas =
    exhibit(opacity)

  def -#(filter: Filter): Canvas =
    exhibit(filter)


  final def visible_area: Visible_Area =
    props.visible_area

  final def width: Double =
    props.width

  final def height: Double =
    props.height

  final def origin_pos: Origin_Positioning =
    props.origin_pos


  def title: String =
    props.title

  def desc: Option[String] =
    props.desc


  protected def create_effected_canvas(effect: Effect): Canvas =
    new Canvas(props, shapes map ( _ exhibit effect ), true)
}
