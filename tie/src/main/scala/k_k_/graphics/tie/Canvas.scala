/*
   file: k_k_/graphics/tie/Canvas.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2013 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie

import k_k_.graphics.tie.effects._
import k_k_.graphics.tie.shapes._


case class VisibleArea(upperLeft: Point, width: Double, height: Double)


sealed abstract class OriginPos
object OriginPos {
  case object Centered extends OriginPos
  case object TopLeft extends OriginPos
}


trait CanvasSpec {
  def width: Double
  def height: Double
  def originPos: OriginPos
  def title: String
  def desc: Option[String]

  final def visibleArea: VisibleArea = {
    val (w, h) = (math.abs(width), math.abs(height))
    val upperLeft = originPos match {
      case OriginPos.Centered => Point(-w/2, -h/2)
      case OriginPos.TopLeft => Point(0, 0)
    }
    VisibleArea(upperLeft, w, h)
  }
}


object CanvasProps {
  val defaultTitle: String = "Draring"
}

case class CanvasProps(
    width: Double,
    height: Double,
    originPos: OriginPos = OriginPos.Centered,
    title: String = CanvasProps.defaultTitle,
    desc: Option[String] = None
  ) extends CanvasSpec


// NOTE: add (ignored) arg to primary ctor and make private, in order to
// introduce public one w/ equiv. signature, which, when invoked externally,
// reverses List[Shape], whereas list not reversed for internal invocation
class Canvas private (
    props: CanvasProps,
    val shapes: List[Shape],
    wasInvokedInternally: Boolean
  ) {

  def this(props: CanvasProps, shapes: List[Shape] = Nil) =
    this(props, shapes.reverse, true)

  def this(props: CanvasProps, shapeArgs: Shape*) =
    this(props, shapeArgs.reverse.toList, true)


  def place(shape: Shape): Canvas = new Canvas(props, shape :: shapes, true)

  def place(shape: Shape, centerX: Double, centerY: Double): Canvas =
    (centerX, centerY) match {
      case (0, 0) => place(shape)
      case _      => place(TranslatedShape(shape, centerX, centerY))
    }


  def exhibit(effect: Effect): Canvas = createEffectedCanvas(effect)

  def exhibit(opacity: Double): Canvas = exhibit(Effect.Opacity(opacity))

  def exhibit(filter: Filter): Canvas = exhibit(Effect.Filtering(filter))


  def -#(effect: Effect): Canvas = exhibit(effect)

  def -#(opacity: Double): Canvas = exhibit(opacity)

  def -#(filter: Filter): Canvas = exhibit(filter)


  final def visibleArea: VisibleArea = props.visibleArea

  final def width: Double = props.width

  final def height: Double = props.height

  final def originPos: OriginPos = props.originPos


  def title: String = props.title

  def desc: Option[String] = props.desc


  protected def createEffectedCanvas(effect: Effect): Canvas =
    new Canvas(props, shapes map ( _ exhibit effect ), true)
}
