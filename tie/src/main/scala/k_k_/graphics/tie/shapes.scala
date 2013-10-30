/*
   file: k_k_/graphics/tie/shapes.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2013 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie

package shapes {

import java.io.{File, FileNotFoundException, ByteArrayInputStream}
import javax.imageio.ImageIO

import k_k_.graphics.tie.shapes.path._
import k_k_.graphics.tie.shapes.text.{Text, DefaultTextRulerFactory}
import k_k_.graphics.tie.effects._
import k_k_.graphics.tie.ink.Pen
import k_k_.graphics.tie.transform._

import k_k_.algo.{Math => MoreMath}


//????redo Rectangular as a type class????

object Rectangular {
  def unapply(rect: Rectangular): Option[(Double, Double)] =
    Some(rect.width, rect.height)
}

/**
 * Mixin abstraction for specifying that a type has rectangular dimensions
 * (i.e. `width` x `height`).
 */
trait Rectangular {
  val width: Double
  val height: Double
}


object BoundingBoxed {
  def apply(bboxed: BoundingBoxed): Dims =
    bboxed match {
      case dims: Dims => dims
      case _          => bboxed.boundingBox
    }
}

trait BoundingBoxed {

  def boundingBox: Dims

  def centerPt: Point = boundingBox.centerPt

  final def boundingBoxShape: SimpleShape = boundingBox.asShape

  final def boundingBoxPath: Path = boundingBoxShape.asPath

  // aliases:
  final def bbox      = boundingBox
  final def bboxShape = boundingBoxShape
  final def bboxPath  = boundingBoxPath
}

trait BoundingShaped { self: BoundingBoxed =>
  def bestBoundingShape: SimpleShape = boundingBox.asShape
}


sealed trait ShapeOp { self: Shape => }


object NullaryShapeOp {

  // WARNING: be careful when matching to not forget the empty parens ('()'),
  // so the compiler will know not to match this object (when matching an
  // extension of its companion trait is what is desired).  i.e. use:
  //   case nop @ NullaryShapeOp() => ...
  // not:
  //   case nop @ NullaryShapeOp => ...
  // or prepare for the error: pattern type is incompatible with expected type;
  // [INFO]  found   : object k_k_.graphics.tie.shapes.NullaryShapeOp
  // [INFO]  required: k_k_.graphics.tie.shapes.Shape
  // [INFO]         case NullaryShapeOp => (None, true)
  // [INFO]              ^
  def unapply(s: NullaryShapeOp): Boolean = true
}

trait NullaryShapeOp extends ShapeOp { self: Shape => }


object UnaryShapeOp {
  def unapply(s: UnaryShapeOp): Option[Shape] = Some(s.shape)
}

trait UnaryShapeOp extends ShapeOp { self: Shape =>

  val shape: Shape

  def child = shape

  def child_=(replacementShape: Shape) = replace(replacementShape)

  // NOTE: `with` included in result type for symmetry with BinaryShapeOp;
  // does enable the ridiculous: (uShapeOp.child = shape1).child = shape2
  def replace(replacementShape: Shape): Shape with UnaryShapeOp
}


object BinaryShapeOp {
  def unapply(s: BinaryShapeOp): Option[(Shape, Shape)] = Some(s.left, s.right)
}

trait BinaryShapeOp extends ShapeOp { self: Shape =>

  def left:  Shape

  def right: Shape

  def left_=(replacementShape: Shape) = replaceLeft(replacementShape)

  def right_=(replacementShape: Shape) = replaceRight(replacementShape)

  // NOTE: `with` in result type allows for chaining in the form of:
  //   (binShapeOp.left = shape1).right = shape2
  def replace(replacementLeft: Shape = left, replacementRight: Shape = right):
    Shape with BinaryShapeOp

  def replaceLeft(replacementLeft: Shape) = replace(replacementLeft, right)

  def replaceRight(replacementRight: Shape) = replace(left, replacementRight)
}


trait ShapeTraversal { self: Shape =>

  final def mapped(f: Shape => Shape): Shape =
    this match {
      case nshape @ NullaryShapeOp()    => f(nshape)
      case ushape @ UnaryShapeOp(s)     => ushape.replace(f(s))
      case bshape @ BinaryShapeOp(l, r) => bshape.replace(f(l), f(r))
    }

  // NOTE: distinct name precludes 'missing param type for expanded func' error
  final def mappedWhen(pf: PartialFunction[Shape, Shape]): Shape = {

    val liftedPf = pf.lift

    def walk(currShape: Shape): Option[Shape] =
      currShape match {
        case nshape @ NullaryShapeOp()    => liftedPf(nshape)
        case ushape @ UnaryShapeOp(s)     => walk(s).map(ushape.replace(_))
        case bshape @ BinaryShapeOp(l, r) =>
          walk(l) match {
            case Some(s) =>
              walk(r).map(bshape.replace(s, _)).orElse {
                Some(bshape.replace(s, r))
              }
            case None =>
              walk(r).map(bshape.replace(l, _))
          }
      }
    walk(this).getOrElse(this)

// impl. which would always rebuild the entire tree
//    def walk(currShape: Shape): Shape =
//      currShape match {
//        case s if pf.isDefinedAt(s)         => pf(s)
//        case nshape @ NullaryShapeOp()    => nshape
//        case ushape @ UnaryShapeOp(s)     => ushape.replace(walk(s))
//        case bshape @ BinaryShapeOp(l, r) => bshape.replace(walk(l),walk(r))
//      }
//    walk(this)//way!
  }

  final def map[T](ordering: => Seq[Shape])(f: Shape => T): Seq[T] =
    ordering map f

  // pre-order traversal:
  final def map[T](f: Shape => T): Seq[T] = map(preOrder)(f)

  final def collect[T](ordering: => Seq[Shape])(pf: PartialFunction[Shape, T]):
      Seq[T] =
    (ordering map pf.lift) filter ( _ ne None ) map ( _.get )

  // pre-order traversal:
  final def collect[T](pf: PartialFunction[Shape, T]): Seq[T] =
    collect(preOrder)(pf)

  final def contains(ordering: => Seq[Shape])(elem: Shape): Boolean =
    ordering contains elem

  final def contains(elem: Shape): Boolean =
    contains(preOrder)(elem)


  final def count(ordering: => Seq[Shape])(pred: Shape => Boolean): Int =
    ordering count pred

  final def count(pred: Shape => Boolean): Int =
    count(preOrder)(pred)


  final def exists(ordering: => Seq[Shape])(pred: Shape => Boolean): Boolean =
    ordering exists pred

  final def exists(pred: Shape => Boolean): Boolean =
    exists(preOrder)(pred)


  final def find(ordering: => Seq[Shape])(pred: Shape => Boolean):
      Option[Shape] =
    ordering find pred

  final def find(pred: Shape => Boolean): Option[Shape] =
    find(preOrder)(pred)


  final def preOrder: Seq[Shape] = {
    def walkInSteps(shape: Shape): Stream[Shape] =
      shape match {
        case nshape @ NullaryShapeOp() =>
          Stream(nshape)
        case ushape @ UnaryShapeOp(s) =>
          Stream.cons(ushape, walkInSteps(s))
        case bshape @ BinaryShapeOp(l, r) =>
          Stream.cons(bshape, walkInSteps(l)) append walkInSteps(r)
      }
    walkInSteps(this)
  }

  final def postOrder: Seq[Shape] = {
    def walkInSteps(shape: Shape): Stream[Shape] =
      shape match {
        case nshape @ NullaryShapeOp() =>
          Stream(nshape)
        case ushape @ UnaryShapeOp(s) =>
          walkInSteps(s) append Stream(ushape)
        case bshape @ BinaryShapeOp(l, r) =>
          walkInSteps(l) append walkInSteps(r) append Stream(bshape)
      }
    walkInSteps(this)
  }
}


sealed abstract class ClipRule
object ClipRule {
  case object NonZero extends ClipRule // default
  case object EvenOdd extends ClipRule
  case object Inherit extends ClipRule
}


trait PresentableShape[T <: PresentableShape[T]] { self: T =>

  def combo(over: T): T = createCompositeShape(over)

  def -&(over: T): T = combo(over)


  def clipBy(clipping: T, rule: ClipRule = ClipRule.NonZero): T =
    createClippedShape(clipping, rule)

  def -<>(clipping: T, rule: ClipRule): T = clipBy(clipping, rule)

  def -<>(clipping: T): T = clipBy(clipping)


  def maskBy(mask: T): T = createMaskedShape(mask)

  def -<#>(mask: T): T = maskBy(mask)


  def using(pen: Pen): T = createInkedShape(pen)

  def -~(pen: Pen): T = using(pen)


  def exhibit(effect: Effect): T = createEffectedShape(effect)

  def exhibit(opacity: Double): T = exhibit(Effect.Opacity(opacity))

  def exhibit(filter: Filter): T = exhibit(Effect.Filtering(filter))


  def -#(effect: Effect): T = exhibit(effect)

  def -#(opacity: Double): T = exhibit(opacity)

  def -#(filter: Filter): T = exhibit(filter)


  def as(attribution: Attribution): T = createAttributedShape(attribution)

  def as(attribution1: Attribution, attributions: Attribution*): T =
    // NOTE: evaluation respects operator's left-assoc. semantics
    (this.as(attribution1) /: attributions) { (instance, attribution) =>
      instance.as(attribution)
    }

  def as(id: String): T = createAttributedShape(IdAttribution(id))


  def -:(attribution: Attribution): T = as(attribution)

  def -:(attribution1: Attribution, attributions: Attribution*): T =
    // NOTE: evaluation respects operator's right-assoc. semantics
    (attributions :\ this) { (attribution, instance) =>
      instance.as(attribution)
    }.as(attribution1)

  def -:(id: String): T = as(IdAttribution(id))


  def -:-(attribution: Attribution): T = as(attribution)

  def -:-(attribution1: Attribution, attributions: Attribution*): T =
    as(attribution1, attributions: _*)

  def -:-(id: String): T = as(IdAttribution(id))


  protected def createCompositeShape(other: T): T
  protected def createClippedShape(clipping: T, rule: ClipRule): T
  protected def createMaskedShape(mask: T): T
  protected def createInkedShape(pen: Pen): T
  protected def createEffectedShape(effect: Effect): T
  protected def createAttributedShape(attribution: Attribution): T
}


object Shape {

  /** calculate 'least common fit' bounding box, with center at (0, 0)
   *
   *  definition: the 'least common fit' bounding box is the smallest bounding
   *  box capable of fully containing every respective bounding box of all
   *  `shapes`, if each of their bounding box were centered at (0, 0)
   */
  def commonFitBoundingBox(shapes: Traversable[Shape]): Dims =
    OriginDims.apply _ tupled
      ((0.0, 0.0) /: shapes) { (cfDims, shape) =>
        val bb = shape.boundingBox
        (cfDims._1 max bb.width,
         cfDims._2 max bb.height)
      }

  // alias:
  val commonBbox = commonFitBoundingBox _
}

sealed abstract class Shape
    extends Transforming[Shape]
       with Placeable[Shape]
       with PresentableShape[Shape]
       with BoundingBoxed with BoundingShaped
       with ShapeTraversal { self: ShapeOp =>

  // technically, this method with it's 'conversion' is useless; yet, it nicely
  // captures a useful invariant
  final def asShapeOp: ShapeOp = this


  type TranslatedT          = TranslatedNonSimpleShape
  protected val Translated  = TranslatedNonSimpleShape

  type ScaledT              = ScaledNonSimpleShape
  protected val Scaled      = ScaledNonSimpleShape

  type RotatedT             = RotatedNonSimpleShape
  protected val Rotated     = RotatedNonSimpleShape

  type ReflectedT           = ReflectedNonSimpleShape
  protected val Reflected   = ReflectedNonSimpleShape

  type SkewedHorizT         = SkewedHorizShape
  protected val SkewedHoriz = SkewedHorizShape

  type SkewedVertT          = SkewedVertShape
  protected val SkewedVert  = SkewedVertShape


  protected def createCompositeShape(other: Shape): Shape = {

    // combo successive InvisRectangle`s (for padding) into singular containing

    // NOTE: equiv. algo not possible to implement as method override in
    // InvisRectangle, due to treatment of translated(, etc.) InvisRectangle
    // (see isInvisRect() below)

    // NOTE: code here is directly informed by the usage of InvisRectangle in 
    // _._._.tile.adjust.ShapeAdjustmentMethods.pad()--namely it is
    // neither scaled, rotated, reflected, skewed, nor inked, and is always
    // `under` in a composite

    def isInvisRect(shape: Shape): Boolean =
      shape match {
        case InvisRectangle(_, _)         => true
        case TranslatedShape(inner, _, _) => isInvisRect(inner)
        // no InvisRectangle is expected to be scaled, yet don't overlook here
        case ScaledShape(inner, _, _)     => isInvisRect(inner)
        // thanks to this method, no invis rect shall be within nested composite
        //   case CompositeShape(under, over)
        // no InvisRectangle is expected to be rotated, reflected, skewed, nor
        // inked-- stand by this!
        //   case RotatedShape(inner, _, _, _)
        //   case ReflectedShape(inner, _, _, _)
        //   case SkewedHorizShape(inner, _)
        //   case SkewedVertShape(inner, _)
        //   case InkedShape(_, _)


//??????????what about ClippedShape??????????????

//??????????what about MaskedShape??????????????

        case _ => false
      }

    def mergeInvisRects(r1: Shape, r2: Shape): Shape =
      // NOTE: crucial to combo, not each shape, but each shape's boundingBox,
      // to eliminate potential for infinite recursion, since
      // (Shape).combo implemented ITO this very method
      // InvisRectangle.cloakRect((r1 -& r2).boundingBox.asShape)
      InvisRectangle.cloakRect((r1.boundingBox -& r2.boundingBox).asShape)

    if (isInvisRect(this)) {
      other match {
        case CompositeShape(under, over) if isInvisRect(under) =>
          CompositeShape(mergeInvisRects(this, under), over)
        case _ if isInvisRect(other) =>
          mergeInvisRects(this, other)
        case _ =>
          CompositeShape(this, other)
      }
    } else {
      other match {
        case IdentityShape() => this // special handling for 'identity' shape
        case _               => CompositeShape(this, other)
      }
    }
  }

  protected def createClippedShape(clipping: Shape, rule: ClipRule): Shape =
    clipping match {
      case IdentityShape() => NullShape // IdentityShape clips everything
      case _               => ClippedShape(this, clipping, rule)
    }

  protected def createMaskedShape(mask: Shape): Shape =
    mask match {
      case IdentityShape() => NullShape // IdentityShape masks everything
      case _               => MaskedShape(this, mask)
    }

  protected def createInkedShape(pen: Pen): Shape = InkedShape(this, pen)

  protected def createEffectedShape(effect: Effect): Shape =
    effect match {
      case Effect.Opacity(opacity) => this match {
        case NonOpaqueShape(shape, prevOpacity) =>
          val resultingOpacity = prevOpacity * opacity
          if (resultingOpacity == 1.0)
            shape
          else
            NonOpaqueShape(shape, resultingOpacity)
        case _ =>
            NonOpaqueShape(this, opacity)
      }
      case Effect.Filtering(filter) => FilteredShape(this, filter)
    }

  protected def createAttributedShape(attribution: Attribution): Shape =
    AttributedShape(this, attribution)
}


sealed abstract class TrueShape
    extends Shape { self: ShapeOp =>
  def asPath: Path
}


sealed abstract class FauxShape
    extends Shape with NullaryShapeOp

// for 'best' bounding shape (where shape must be necessarily 'closed')
sealed abstract class SimpleShape
    extends TrueShape {  self: ShapeOp =>

//     with Transforming[SimpleShape]
//     with Placeable[SimpleShape] {

   //!!!!!!!!!!this should be fully abstract, not defined here!!!!!!!
  def asSegments: Seq[Segment] = Nil

  override def bestBoundingShape: SimpleShape = this


/*?????it would seem to be equivalent to 'override' every method to have a
  covariant return type by extending `Transforming[SimpleShape]`
  however it causes the following error, which seems only to be solved by
  overriding every method 'by hand' to specifically state the covariant return
  type--what could be done here???????

[ERROR] .../tie/tie/src/main/scala/k_k_/graphics/tie/shapes.scala:1096: error: illegal inheritance;
[INFO]  class RotatedSimpleShape inherits different type instances of trait Transforming:
[INFO] k_k_.graphics.tie.transformable.Transforming[k_k_.graphics.tie.shapes.SimpleShape] and k_k_.graphics.tie.transformable.Transforming[k_k_.graphics.tie.shapes.Shape]
[INFO] case class RotatedSimpleShape(shape: SimpleShape,
[INFO]            ^


  override type TranslatedT         = TranslatedSimpleShape
  override protected val Translated = TranslatedSimpleShape

  override type ScaledT             = ScaledSimpleShape
  override protected val Scaled     = ScaledSimpleShape

  override type RotatedT            = RotatedSimpleShape
  override protected val Rotated    = RotatedSimpleShape

  override type ReflectedT          = ReflectedSimpleShape
  override protected val Reflected  = ReflectedSimpleShape
*/


  override def move(xDist: Double, yDist: Double): SimpleShape =
    this match {
      case TranslatedSimpleShape(inner, existingXDist, existingYDist) =>
        val combinedXDist = xDist + existingXDist
        val combinedYDist = yDist + existingYDist
        if (combinedXDist == 0.0 && combinedYDist == 0.0)
          inner // successive ops cancel one another
        else
          // adjust 'previous' op by combining with sucessor
          TranslatedSimpleShape(inner, combinedXDist, combinedYDist)
      case _ =>
        TranslatedSimpleShape(this, xDist, yDist)
    }

  override def move(ptOffset: Point): SimpleShape =
    move(ptOffset.x, ptOffset.y)

  override def move(dist: Double): SimpleShape =
    move(dist, dist)

  override def -+(xDist: Double, yDist: Double): SimpleShape =
    move(xDist, yDist)

  override def -+(ptOffset: Point): SimpleShape =
    move(ptOffset.x, ptOffset.y)

  override def -+(dist: Double): SimpleShape =
    move(dist, dist)


  override def to(destPt: Point): SimpleShape =
    move(destPt -+ -centerPt)

  override def to(xCoord: Double, yCoord: Double): SimpleShape =
    to(Point(xCoord, yCoord))

  override def -@(destPt: Point): SimpleShape =
    to(destPt)

  override def -@(xCoord: Double, yCoord: Double): SimpleShape =
    to(Point(xCoord, yCoord))


  override def scale(xScaling: Double, yScaling: Double): SimpleShape = {
    this match {
      case ScaledSimpleShape(inner, existingXScaling,existingYScaling) =>
        val combinedXScaling = xScaling * existingXScaling
        val combinedYScaling = yScaling * existingYScaling
        if (combinedXScaling == 1.0 && combinedYScaling == 1.0)
          inner // successive ops cancel one another
        else
          // adjust 'previous' op by combining with sucessor
          ScaledSimpleShape(inner, combinedXScaling, combinedYScaling)
      case _ =>
        ScaledSimpleShape(this, xScaling, yScaling)
    }
  }


  override def scale(scaling: Double): SimpleShape =
    scale(scaling, scaling)

  override def -*(xScaling: Double, yScaling: Double): SimpleShape =
    scale(xScaling, yScaling)

  override def -*(scaling: Double): SimpleShape =
    scale(scaling, scaling)


  override def rotate(degrees: Double, aboutX: Double, aboutY: Double):
      SimpleShape =
    (aboutX, aboutY, this) match {
      // NOTE: simplify only when both rotate about (0,0)--else too complicated
      case (0, 0, RotatedSimpleShape(inner, existingDegrees, 0, 0)) =>
        val combinedDegrees = degrees + existingDegrees
        if (combinedDegrees % 360 == 0.0)
          inner // successive ops cancel one another
        else
          // adjust 'previous' op by combining with sucessor
          RotatedSimpleShape(inner, combinedDegrees, 0, 0)
      case _ =>
        RotatedSimpleShape(this, degrees, aboutX, aboutY)
    }

  override def rotate(degrees: Double, centerPt: Point = Point(0, 0)):
      SimpleShape =
    rotate(degrees, centerPt.x, centerPt.y)

  override def -%(degrees: Double, aboutX: Double, aboutY: Double):
      SimpleShape =
    rotate(degrees, aboutX, aboutY)

  override def -%(degrees: Double, centerPt: Point = Point(0, 0)): SimpleShape =
    rotate(degrees, centerPt.x, centerPt.y)


  override def reflect(degrees: Double, aboutX: Double, aboutY: Double):
      SimpleShape =
    (aboutX, aboutY, this) match {
      // NOTE: simplify only when both reflect about (0,0)--else too complicated
      case (0, 0, ReflectedSimpleShape(inner, existingDegrees, 0, 0))
             if ((degrees % 360) == (existingDegrees % 360)) =>
          inner // successive ops cancel one another
      case _ =>
        ReflectedSimpleShape(this, degrees, aboutX, aboutY)
    }

  override def reflect(degrees: Double, aboutPt: Point = Point(0, 0)):
      SimpleShape =
    reflect(degrees, aboutPt.x, aboutPt.y)

  override def -|-(degrees: Double, aboutX: Double, aboutY: Double):
      SimpleShape =
    reflect(degrees, aboutX, aboutY)

  override def -|-(degrees: Double, aboutPt: Point = Point(0, 0)): SimpleShape =
    reflect(degrees, aboutPt.x, aboutPt.y)
}


sealed abstract class Segment
    extends TrueShape with NullaryShapeOp

object Line {
  def between(p1: Point, p2: Point): Shape =
    if (p1 == p2) Line(0.001) -@ p1
    else {
      val (run, rise) = p2 - p1
      val rotateDegrees = {
        if      (rise == 0.0) 0
        else if (run  == 0.0) if (rise > 0) 90 else 270
        else math.toDegrees(math.atan(rise / run))
      }
      val length = p1.distance(p2)
      val start: Point = (if (run < 0.0) length/2 else -length/2, 0.0)
      val (moveX, moveY) = p1 - start.rotate(rotateDegrees)
      Line(length).rotate(rotateDegrees).move(moveX, moveY)
    }
}

case class Line(length: Double)
    extends Segment {
  override def boundingBox = OriginDims(length, 0.01)

  override def asPath = {
    val upperLeft: Point = (0 - length/2, 0.0)
    Path.from(upperLeft).
         horiz(length)
         // NOTE: no `close`, since overstroke may double effective stroke-width
  }
}


case class Hemisphere(
    radWidth: Double,
    radHeight: Double,
    isTopHalf: Boolean = true
  ) extends Segment {

  override def boundingBox = OriginDims(radWidth * 2, radHeight)

  override def asPath = {
    val leftStart =
        Point(0 - radWidth, if (isTopHalf) radHeight/2 else 0 - radHeight/2)
    Path.from(leftStart).
         arc(
             radWidth, radHeight,
             if (isTopHalf) ArcChoice.LargeCW else ArcChoice.LargeCCW,
             radWidth * 2, 0
           )
  }
}

object IsoTriangle {
  def apply(baseWidth: Double, height: Double) =
    new IsoTriangle(baseWidth, height)

  def unapply(isoTri: IsoTriangle) =
    Some(isoTri.baseWidth, isoTri.height)
}

sealed class IsoTriangle(val baseWidth: Double, val height: Double)
    extends SimpleShape with NullaryShapeOp {

  override def boundingBox = OriginDims(baseWidth, height)

  override def asPath = {
    val topPoint: Point = (0.0, 0 - height/2) 
    val (slantWidth, slantHeight) = (baseWidth/2, height) 
    Path.from(topPoint).
         line(slantWidth, slantHeight).
         horiz(-baseWidth).
         close
  }

  override def toString = "IsoTriangle(%s, %s)".format(baseWidth, height)
}

case class RightTriangle(baseWidth: Double, height: Double)
    extends SimpleShape with NullaryShapeOp {

  override def boundingBox = OriginDims(baseWidth, height)

  override def asPath = {
    val topPoint: Point = (0 + baseWidth/2, 0 - height/2) 
    Path.from(topPoint).
         vert(height).
         horiz(-baseWidth).
         close
  }
}

object Rectangle {
  def apply(width: Double, height: Double) = new Rectangle(width, height)

  def unapply(rect: Rectangle) = Some(rect.width, rect.height)
}

sealed class Rectangle(val width: Double, val height: Double)
    extends SimpleShape with Rectangular with NullaryShapeOp {

  override def boundingBox = OriginDims(width, height)

  override def asPath = {
    val upperLeft: Point = (0 - width/2, 0 - height/2)
    Path.from(upperLeft).
         horiz( width).vert( height).
         horiz(-width).vert(-height). //???delete vert????
         close
  }

  override def toString = "Rectangle(%s, %s)".format(width, height)
}

case class Parallelogram(sideWidth: Double, fullWidth: Double, height: Double)
    extends SimpleShape with NullaryShapeOp {

  override def boundingBox = OriginDims(fullWidth, height)

  override def asPath = {
  //??????is rearranging these a good idea or not?????
    val (longWidth, shortWidth) = MoreMath.maxMin(sideWidth, fullWidth)
    val slantWidth = longWidth - shortWidth
    val upperLeft: Point = (0 - (longWidth/2 - slantWidth),
                             0 - height/2) 
    Path.from(upperLeft).
         horiz( shortWidth).
         line(-slantWidth, height).
         horiz(-shortWidth).
         close
  }
}

case class Trapezoid(topWidth: Double, bottomWidth: Double, height: Double)
    extends SimpleShape with NullaryShapeOp {

  override def boundingBox = OriginDims(math.max(topWidth, bottomWidth), height)

  override def asPath = {
    val upperLeft: Point = (0 - topWidth/2, 0 - height/2) 
    val (slantWidth, slantHeight) = ((bottomWidth - topWidth) / 2, height)
    Path.from(upperLeft).
         horiz(topWidth).
         line(slantWidth, slantHeight).
         horiz(-bottomWidth).
         close
  }
}

object Pentagon {
  def apply(
      sideWidth: Double, fullWidth: Double,
      sideHeight: Double, fullHeight: Double
    ) =
    new Pentagon(sideWidth, fullWidth, sideHeight, fullHeight)

  def unapply(pent: Pentagon) =
    Some(pent.sideWidth, pent.fullWidth, pent.sideHeight, pent.fullHeight)
}

sealed class Pentagon(
    val sideWidth: Double,
    val fullWidth: Double,
    val sideHeight: Double,
    val fullHeight: Double
  ) extends SimpleShape with NullaryShapeOp {

  override def boundingBox = OriginDims(fullWidth, fullHeight)

  override def asPath = {
    val topPoint: Point = (0.0, 0 - fullHeight/2)
    val (tipSlantWidth, tipSlantHeight) =
        (fullWidth / 2, fullHeight - sideHeight)
    val (baseSlantWidth, baseSlantHeight) =
        ((fullWidth - sideWidth) / 2, sideHeight)
    Path.from(topPoint).
         line( tipSlantWidth,   tipSlantHeight).
         line(-baseSlantWidth,  baseSlantHeight).
         horiz(-sideWidth).
         line(-baseSlantWidth, -baseSlantHeight).
         close
  }

  override def toString = "Pentagon(%s, %s, %s, %s)".
      format(sideWidth, fullWidth, sideHeight, fullHeight)
}

object Hexagon {
  def apply(sideWidth: Double, fullWidth: Double, height: Double) =
    new Hexagon(sideWidth, fullWidth, height)

  def unapply(hex: Hexagon) =
    Some(hex.sideWidth, hex.fullWidth, hex.height)
}

sealed class Hexagon(
    val sideWidth: Double,
    val fullWidth: Double,
    val height: Double
  ) extends SimpleShape with NullaryShapeOp {

  override def boundingBox = OriginDims(fullWidth, height)

  override def asPath = {
    val upperLeft: Point = (0 - sideWidth/2, 0 - height/2)
      //?????????possibly rearrange args to use larger of two?????????
    val (slantWidth, slantHeight) = ((fullWidth - sideWidth) / 2,
                                       height / 2)
    Path.from(upperLeft).
         horiz( sideWidth).line( slantWidth,  slantHeight).
                            line(-slantWidth,  slantHeight).
         horiz(-sideWidth).line(-slantWidth, -slantHeight).
         close
  }

  override def toString = "Hexagon(%s, %s, %s)".
      format(sideWidth, fullWidth, height)
}

object Octagon {
  def apply(
      sideWidth: Double,
      fullWidth: Double,
      sideHeight: Double,
      fullHeight: Double
    ) =
    new Octagon(sideWidth, fullWidth, sideHeight, fullHeight)

  def unapply(oct: Octagon) =
    Some(oct.sideWidth, oct.fullWidth, oct.sideHeight, oct.fullHeight)
}

sealed class Octagon(
    val sideWidth: Double,
    val fullWidth: Double,
    val sideHeight: Double,
    val fullHeight: Double
  ) extends SimpleShape with NullaryShapeOp {

  override def boundingBox = OriginDims(fullWidth, fullHeight)

  override def asPath = {
    val upperLeft: Point = (0 - sideWidth/2, 0 - fullHeight/2)
      //?????????possibly rearrange args to use larger of two?????????
    val (slantWidth, slantHeight) = ((fullWidth  - sideWidth)  / 2,
                                       (fullHeight - sideHeight) / 2)
    Path.from(upperLeft).
        horiz( sideWidth).line( slantWidth,  slantHeight).vert( sideHeight).
                           line(-slantWidth,  slantHeight).
        horiz(-sideWidth).line(-slantWidth, -slantHeight).vert(-sideHeight).
        close
  }

  override def toString = "Octagon(%s, %s, %s, %s)".
      format(sideWidth, fullWidth, sideHeight, fullHeight)
}

object Ellipse {
  def apply(radWidth: Double, radHeight: Double) =
    new Ellipse(radWidth, radHeight)

  def unapply(e: Ellipse) =
    Some(e.radWidth, e.radHeight)
}

sealed class Ellipse(val radWidth: Double, val radHeight: Double)
    extends SimpleShape with NullaryShapeOp {

  override def boundingBox = OriginDims(radWidth*2, radHeight*2)

  override def asPath = {
    val leftStart:  Point = (0 - radWidth, 0.0)
    // NOTE: for SVG 1.1, when arc coords equal current pt., the arc is NOT
    // rendered--even when choosing Large{C}CW arc! therefore, compose ellipse
    // from top and bottom 'half' arcs, before closing.
    Path.from(leftStart).
         arc(radWidth, radHeight, ArcChoice.LargeCW,   radWidth * 2,  0).
         arc(radWidth, radHeight, ArcChoice.LargeCW, -(radWidth * 2), 0).
         close
  }

  override def toString = "Ellipse(%s, %s)".format(radWidth, radHeight)
}


object FreeForm {
  def apply(path: Path) = new FreeForm(path)

  def unapply(freeForm: FreeForm) = Some(freeForm.path)
}

sealed class FreeForm(val path: Path)
    extends TrueShape with NullaryShapeOp {

  override def boundingBox = {
    import Point._

    def calcMaxBoundaries: (Point, Point) = {
      import scala.annotation.tailrec

      @tailrec // ensure tail-call-optimization to handle long Path cmd seqs
      def trackGrowth(
          remainingCmds: List[PathCmd],
          pathMemory: Path.PosMemory,
          currBounds: Option[(Point, Point)]
        ): (Point, Point) = {
        remainingCmds match {
          case Nil => currBounds.getOrElse(((0.0, 0.0), (0.0, 0.0)))
          case cmd :: moreCmds =>
            cmd match {
              // position/path-management commands:
              case MoveAbs(x, y) =>
                trackGrowth(moreCmds, pathMemory.startSubPathAbs(x, y), currBounds)
              case MoveRel(x, y) =>
                trackGrowth(moreCmds, pathMemory.startSubPathRel(x, y), currBounds)
              case Close =>
                trackGrowth(moreCmds, pathMemory.closeSubPath, currBounds)

              // line-drawing commands:
              case LineAbs(x, y) =>
                val newPathMemory = pathMemory.replacePtAbs(x, y)
                val newBounds = recordLine(pathMemory.currPt, newPathMemory, currBounds)
                trackGrowth(moreCmds, newPathMemory, newBounds)
              case LineRel(x, y) =>
                val newPathMemory = pathMemory.replacePtRel(x, y)
                val newBounds = recordLine(pathMemory.currPt, newPathMemory, currBounds)
                trackGrowth(moreCmds, newPathMemory, newBounds)
              case HorizontalAbs(x) =>
                val newPathMemory = pathMemory.replacePtHorizAbs(x)
                val newBounds = recordLine(pathMemory.currPt, newPathMemory, currBounds)
                trackGrowth(moreCmds, newPathMemory, newBounds)
              case HorizontalRel(x) =>
                val newPathMemory = pathMemory.replacePtRel(x, 0)
                val newBounds = recordLine(pathMemory.currPt, newPathMemory, currBounds)
                trackGrowth(moreCmds, newPathMemory, newBounds)
              case VerticalAbs(y) =>
                val newPathMemory = pathMemory.replacePtVertAbs(y)
                val newBounds = recordLine(pathMemory.currPt, newPathMemory, currBounds)
                trackGrowth(moreCmds, newPathMemory, newBounds)
              case VerticalRel(y) =>
                val newPathMemory = pathMemory.replacePtRel(0, y)
                val newBounds = recordLine(pathMemory.currPt, newPathMemory, currBounds)
                trackGrowth(moreCmds, newPathMemory, newBounds)

              // non-linear-drawing commands:




              //!!!!!!!!!for now, to keep things simple, calculate as if these merely specified a line from the current position to the cmd's end point!!!!!!!!




              case EllipticalArcAbs(radWidth, radHeight, xRotateDegrees,
                                    kind: ArcChoice, x, y) =>
                val newPathMemory = pathMemory.replacePtAbs(x, y)
                val newBounds = recordLine(pathMemory.currPt, newPathMemory, currBounds)
                trackGrowth(moreCmds, newPathMemory, newBounds)
              case EllipticalArcRel(radWidth, radHeight, xRotateDegrees,
                                    kind: ArcChoice, x, y) =>
                val newPathMemory = pathMemory.replacePtRel(x, y)
                val newBounds = recordLine(pathMemory.currPt, newPathMemory, currBounds)
                trackGrowth(moreCmds, newPathMemory, newBounds)
              case QuadBezierAbs(xCtl1, yCtl1, x, y) =>
                val newPathMemory = pathMemory.replacePtAbs(x, y)
                val newBounds = recordLine(pathMemory.currPt, newPathMemory, currBounds)
                trackGrowth(moreCmds, newPathMemory, newBounds)
              case QuadBezierRel(xCtl1, yCtl1, x, y) =>
                val newPathMemory = pathMemory.replacePtRel(x, y)
                val newBounds = recordLine(pathMemory.currPt, newPathMemory, currBounds)
                trackGrowth(moreCmds, newPathMemory, newBounds)
              case TangentQuadBezierAbs(x, y) =>
                val newPathMemory = pathMemory.replacePtAbs(x, y)
                val newBounds = recordLine(pathMemory.currPt, newPathMemory, currBounds)
                trackGrowth(moreCmds, newPathMemory, newBounds)
              case TangentQuadBezierRel(x, y) =>
                val newPathMemory = pathMemory.replacePtRel(x, y)
                val newBounds = recordLine(pathMemory.currPt, newPathMemory, currBounds)
                trackGrowth(moreCmds, newPathMemory, newBounds)
              case CubicBezierAbs(xCtl1, yCtl1, xCtl2, yCtl2, x, y) =>
                val newPathMemory = pathMemory.replacePtAbs(x, y)
                val newBounds = recordLine(pathMemory.currPt, newPathMemory, currBounds)
                trackGrowth(moreCmds, newPathMemory, newBounds)
              case CubicBezierRel(xCtl1, yCtl1, xCtl2, yCtl2, x, y) =>
                val newPathMemory = pathMemory.replacePtRel(x, y)
                val newBounds = recordLine(pathMemory.currPt, newPathMemory, currBounds)
                trackGrowth(moreCmds, newPathMemory, newBounds)
              case TangentCubicBezierAbs(xCtl1, yCtl1, x, y) =>
                val newPathMemory = pathMemory.replacePtAbs(x, y)
                val newBounds = recordLine(pathMemory.currPt, newPathMemory, currBounds)
                trackGrowth(moreCmds, newPathMemory, newBounds)
              case TangentCubicBezierRel(xCtl1, yCtl1, x, y) =>
                val newPathMemory = pathMemory.replacePtRel(x, y)
                val newBounds = recordLine(pathMemory.currPt, newPathMemory, currBounds)
                trackGrowth(moreCmds, newPathMemory, newBounds)
            }
        }
      }

      def recordLine(
          startPos: Point,
          finishPathMemory: Path.PosMemory,
          currBounds: Option[(Point, Point)]
        ) = {
        val finishPos = finishPathMemory.currPt
        val (lineMinX, lineMaxX) = MoreMath.minMax(startPos.x, finishPos.x)
        val (lineMinY, lineMaxY) = MoreMath.minMax(startPos.y, finishPos.y)
        currBounds match {
          case None =>
            Some((Point(lineMinX, lineMinY),
                  Point(lineMaxX, lineMaxY)))

          case Some(prevBounds @ (Point(minX, minY), Point(maxX, maxY))) =>
            // NOTE: providing type for `adjustments` is most succinct way
            // to type-check: it precludes need to specify param type and to
            // explicitly construct `Point` in result of each anon. func., and
            // it enables direct use `prevBounds` '@-alias' later in `foldLeft`
            val adjustments: List[((Point, Point)) => (Point, Point)] = List(

//???
              { (bounds) =>
                if (lineMinX < minX) ((lineMinX, bounds._1.y), bounds._2)
                else bounds
              },
              { (bounds) =>
                if (lineMaxX > maxX) (bounds._1, (lineMaxX, bounds._2.y))
                else bounds
              },
              { (bounds) =>
                if (lineMinY < minY) ((bounds._1.x, lineMinY), bounds._2)
                else bounds
              },
              { (bounds) =>
                if (lineMaxY > maxY) (bounds._1, (bounds._2.x, lineMaxY))
                else bounds
              })
            val newBounds = (prevBounds /: adjustments) {
              (bounds, adjust) => adjust(bounds)
            }
            Some(newBounds)
        }
      }

      val (initialPosMemory, remainingCmds) = path.initPosMemory
      trackGrowth(remainingCmds, initialPosMemory, None)
    }

    val (minPt, maxPt) = calcMaxBoundaries
    Dims.minContaining(minPt, maxPt)
  }

  override def asPath = path

  override def toString = "FreeForm(%s)".format(path)
}


object Custom {
  def unapply(c: Custom) = Some(c.path)
}

// NOTE: intended as extension point: neither sealed nor final
class Custom protected(protected val customPath: Path)
    extends FreeForm(customPath)


object Writing {
  val textRulerFactory = DefaultTextRulerFactory
}

case class Writing(text: Text)
    extends FauxShape {
  override def boundingBox = text.textBoundingBox(Writing.textRulerFactory)
}


object Image {
  def apply(path: String) = new Image(path)

  def apply(path: String, pathMapper: String => String) =
    new Image(path, pathMapper)

  def apply(
      path: String,
      width: Double,
      height: Double,
      pathMapper: (String => String) = identity
    ) =
    new Image(path, width, height, pathMapper)

  def unapply(img: Image): Option[(String, Double, Double)] =
    Some(img.path, img.width, img.height)


  private def calcDims(fpath: String): (Double, Double) = {
    val bufferedImg = ImageIO.read(new File(fpath))
    (bufferedImg.getWidth, bufferedImg.getHeight)
  }

  private def verifyFpathExists(path: String): String =
    if (new File(path).exists) path
    else throw new FileNotFoundException(path)
}

class Image private (
    val path: String,
    dims: (Double, Double),
    pathMapper: String => String
  ) extends FauxShape with Rectangular {

  def this(path: String, pathMapper: String => String) =
    this(path, Image.calcDims(path), pathMapper)

  def this(path: String) =
    this(path, identity)

  def this(
      path: String,
      width: Double,
      height: Double,
      pathMapper: String => String = identity
    ) =
    this(Image.verifyFpathExists(path), (width, height), pathMapper)

  override val width  = dims._1
  override val height = dims._2

  val mappedFpath = pathMapper(path)

  override lazy val boundingBox = OriginDims(width, height)

  override def toString = "Image(%s, %s, %s)".
      format(path, dims, pathMapper(path))
}


case class EquiTriangle(length: Double)
    extends IsoTriangle(length, length * math.sqrt(3.0)/2)


case class Square(length: Double)
    extends Rectangle(length, length)


object RegPentagon {
  // 'golden ratio' == chords / sides (of regular pentagon)
  val phi = (1 + math.sqrt(5)) / 2

  val Phi = phi - 1 // == 1 / phi (neato!)

  def ofSides(length: Double) = RegPentagon(length * phi)
}

import RegPentagon.{phi, Phi}

case class RegPentagon(shapeWidth: Double)
     extends Pentagon(
         shapeWidth * Phi, shapeWidth,
         (shapeWidth * Phi / 2) * math.sqrt(4 - Phi*Phi),
         shapeWidth * Phi * math.sqrt((phi*phi) - .25)
       )


object RegHexagon {
  def ofSides(length: Double) = RegHexagon(length * 2)
}

case class RegHexagon(shapeWidth: Double)
     extends Hexagon(shapeWidth/2, shapeWidth, shapeWidth/2 * math.sqrt(3.0))


object RegOctagon {
  def ofSides(length: Double) = RegOctagon(length * (1 + math.sqrt(2.0)))
}

case class RegOctagon(shapeWidth: Double)
     extends Octagon(shapeWidth / (1 + math.sqrt(2.0)), shapeWidth,
                     shapeWidth / (1 + math.sqrt(2.0)), shapeWidth)


object Circle {
  def apply(rad: Double) = new Circle(rad)

  def unapply(circ: Circle) = Some(circ.rad)
}

sealed class Circle(val rad: Double)
    extends Ellipse(rad, rad) {
  override def toString = "Circle(%s)".format(rad)
}


object InvisRectangle {

  // HINT: an expensive noop--be sure `shape` actually contains a Rectangle!
  def cloakRect(shape: Shape): Shape =
    shape match {
      case Rectangle(width, height) =>
        InvisRectangle(width, height)
      case TranslatedShape(inner, xDist, yDist) =>
        TranslatedShape(cloakRect(inner), xDist, yDist)

      // NOTE: no InvisRectangle is expected to be scaled, rotated, reflected
      // skewed, inked, nor composed (in non-'under' pos); yet, for
      // robustness, implement anyway:
      case ushape @ UnaryShapeOp(s)     => ushape.replace(cloakRect(s))
      case bshape @ BinaryShapeOp(l, r) =>
        bshape.replace(cloakRect(l), cloakRect(r))
                                                            
      case nshape @ NullaryShapeOp()    =>
        nshape // ouch!--no Rectangle here, return unchanged
    }
}

sealed case class InvisRectangle(w: Double, h: Double)
    extends Rectangle(w, h)


// NOTE: DiamEllipse, DiamCircle allow for substitutability w/ Rectangle, ala:
//     val ShapeClass = if (foo) Rectangle else DiamEllipse
//     val shape = ShapeClass(50, 20)

case class DiamEllipse(diamWidth: Double, diamHeight: Double)
    extends Ellipse(diamWidth/2, diamHeight/2)

case class DiamCircle(diam: Double)
    extends Circle(diam/2)


object TranslatedShape {
  def apply(shape: Shape, xDist: Double, yDist: Double) =
    TranslatedNonSimpleShape(shape, xDist, yDist)

  def unapply(shape: Shape) =
    shape match {
      case s: TranslatedNonSimpleShape =>
        Some(s.shape, s.xDist, s.yDist)
      case s: TranslatedSimpleShape =>
        Some(s.shape, s.xDist, s.yDist)
      case _ => None
    }
}

object TranslatedNonSimpleShape
    extends TranslatedTransformable[Shape] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[TranslatedNonSimpleShape]
}

case class TranslatedNonSimpleShape(shape: Shape, xDist: Double, yDist: Double)
    extends Shape with UnaryShapeOp {

  override lazy val boundingBox = shape.boundingBox.move(xDist, yDist)

  override def replace(replacementShape: Shape) = copy(shape = replacementShape)
}

/*!!!not needed since not possible for SimpleShape to extend Transforming[SimpleShape]!!!

object TranslatedSimpleShape
    extends TranslatedTransformable[SimpleShape] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[TranslatedSimpleShape]
}
*/

case class TranslatedSimpleShape(
    shape: SimpleShape,
    xDist: Double,
    yDist: Double
  ) extends SimpleShape with UnaryShapeOp {

  override lazy val boundingBox = shape.boundingBox.move(xDist, yDist)

  override def asPath: Path = shape.asPath.move(xDist, yDist)

  override def replace(replacementShape: Shape) =
    replacementShape match {
      case simpleShape: SimpleShape =>
        copy(shape = simpleShape)
      case _ =>
        TranslatedShape(replacementShape, xDist, yDist)
    }
}


object ScaledShape {
  def apply(shape: Shape, xScaling: Double, yScaling: Double) =
    ScaledNonSimpleShape(shape, xScaling, yScaling)

  def unapply(shape: Shape) =
    shape match {
      case s: ScaledNonSimpleShape =>
        Some(s.shape, s.xScaling, s.yScaling)
      case s: ScaledSimpleShape =>
        Some(s.shape, s.xScaling, s.yScaling)
      case _ => None
    }
}

object ScaledNonSimpleShape
    extends ScaledTransformable[Shape] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[ScaledNonSimpleShape]
}

case class ScaledNonSimpleShape(shape: Shape,xScaling: Double, yScaling: Double)
    extends Shape with UnaryShapeOp {

  override lazy val boundingBox = shape.boundingBox.scale(xScaling, yScaling)

  override def replace(replacementShape: Shape) = copy(shape = replacementShape)
}

case class ScaledSimpleShape(
    shape: SimpleShape,
    xScaling: Double,
    yScaling: Double
  ) extends SimpleShape with UnaryShapeOp {

  override lazy val boundingBox = shape.boundingBox.scale(xScaling, yScaling)

  override def asPath: Path = shape.asPath.scale(xScaling, yScaling)

  override def replace(replacementShape: Shape) =
    replacementShape match {
      case simpleShape: SimpleShape =>
        copy(shape = simpleShape)
      case _ =>
        ScaledShape(replacementShape, xScaling, yScaling)
    }
}


object RotatedShape {

  def apply(shape: Shape, degrees: Double, xPivot: Double, yPivot: Double) =
    RotatedNonSimpleShape(shape, degrees, xPivot, yPivot)

  def unapply(shape: Shape) =
    shape match {
      case s: RotatedNonSimpleShape =>
        Some(s.shape, s.degrees, s.xPivot, s.yPivot)
      case s: RotatedSimpleShape =>
        Some(s.shape, s.degrees, s.xPivot, s.yPivot)
      case _ => None
    }
}

object RotatedNonSimpleShape
    extends RotatedTransformable[Shape] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[RotatedNonSimpleShape]
}

case class RotatedNonSimpleShape(
    shape: Shape,
    degrees: Double,
    xPivot: Double,
    yPivot: Double
  ) extends Shape with UnaryShapeOp {

  override lazy val boundingBox =
    shape.boundingBox.rotate(degrees, xPivot, yPivot)

  override def replace(replacementShape: Shape) = copy(shape = replacementShape)
}

case class RotatedSimpleShape(
    shape: SimpleShape,
    degrees: Double,
    xPivot: Double,
    yPivot: Double
  ) extends SimpleShape with UnaryShapeOp {

  override lazy val boundingBox =
    shape.boundingBox.rotate(degrees, xPivot, yPivot)

  override def asPath: Path = shape.asPath.rotate(degrees, xPivot, yPivot)

  override def replace(replacementShape: Shape) =
    replacementShape match {
      case simpleShape: SimpleShape =>
        copy(shape = simpleShape)
      case _ =>
        RotatedShape(replacementShape, degrees, xPivot, yPivot)
    }
}


object ReflectedShape {
  def apply(shape: Shape, degrees: Double, xPivot: Double, yPivot: Double) =
    ReflectedNonSimpleShape(shape, degrees, xPivot, yPivot)

  def unapply(shape: Shape) =
    shape match {
      case s: ReflectedNonSimpleShape =>
        Some(s.shape, s.degrees, s.xPivot, s.yPivot)
      case s: ReflectedSimpleShape =>
        Some(s.shape, s.degrees, s.xPivot, s.yPivot)
      case _ => None
    }
}

object ReflectedNonSimpleShape
    extends ReflectedTransformable[Shape] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[ReflectedNonSimpleShape]
}

case class ReflectedNonSimpleShape(
    shape: Shape,
    degrees: Double,
    xPivot: Double,
    yPivot: Double
  ) extends Shape with UnaryShapeOp {

  override lazy val boundingBox =
    shape.boundingBox.reflect(degrees, xPivot, yPivot)

  override def replace(replacementShape: Shape) = copy(shape = replacementShape)
}

case class ReflectedSimpleShape(
    shape: SimpleShape,
    degrees: Double,
    xPivot: Double,
    yPivot: Double
  ) extends SimpleShape with UnaryShapeOp {

  override lazy val boundingBox =
    shape.boundingBox.reflect(degrees, xPivot, yPivot)

  override def asPath: Path = shape.asPath.reflect(degrees, xPivot, yPivot)

  override def replace(replacementShape: Shape) =
    replacementShape match {
      case simpleShape: SimpleShape =>
        copy(shape = simpleShape)
      case _ =>
        ReflectedShape(replacementShape, degrees, xPivot, yPivot)
    }
}


object SkewedHorizShape extends SkewedHorizTransformable[Shape] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[SkewedHorizShape]
}

case class SkewedHorizShape(shape: Shape, degrees: Double)
    extends Shape with UnaryShapeOp {

  override lazy val boundingBox = shape.boundingBox.skewHoriz(degrees)

  override def replace(replacementShape: Shape) = copy(shape = replacementShape)
}


object SkewedVertShape extends SkewedVertTransformable[Shape] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[SkewedVertShape]
}

case class SkewedVertShape(shape: Shape, degrees: Double)
    extends Shape with UnaryShapeOp {

  override lazy val boundingBox = shape.boundingBox.skewVert(degrees)

  override def replace(replacementShape: Shape) = copy(shape = replacementShape)
}


case class CompositeShape(under: Shape, over: Shape)
    extends Shape with BinaryShapeOp {

  override lazy val boundingBox = under.boundingBox.combo(over.boundingBox)

  override def left  = under
  override def right = over

  override def replace(replacementLeft: Shape, replacementRight: Shape) =
    copy(under = replacementLeft, over = replacementRight)
}

case class ClippedShape(clipped: Shape, clipping: Shape, rule: ClipRule)
    extends Shape with BinaryShapeOp {

  override lazy val boundingBox =
    clipped.boundingBox.clipBy(clipping.boundingBox)

  override def left  = clipped
  override def right = clipping

  override def replace(replacementLeft: Shape, replacementRight: Shape) =
    copy(clipped = replacementLeft, clipping = replacementRight)
}

case class MaskedShape(masked: Shape, mask: Shape)
    extends Shape with BinaryShapeOp {

  override lazy val boundingBox = masked.boundingBox.maskBy(mask.boundingBox)

  override def left  = masked
  override def right = mask

  override def bestBoundingShape: SimpleShape = masked.bestBoundingShape

  override def replace(replacementLeft: Shape, replacementRight: Shape) =
    copy(masked = replacementLeft, mask = replacementRight)
}

case class InkedShape(shape: Shape, pen: Pen)
    extends Shape with UnaryShapeOp {

  override lazy val boundingBox =
    shape.boundingBox.using(pen)

  override def bestBoundingShape: SimpleShape = shape.bestBoundingShape

  override def replace(replacementShape: Shape) = copy(shape = replacementShape)
}

case class NonOpaqueShape(shape: Shape, opacity: Double)
    extends Shape with UnaryShapeOp {

  override lazy val boundingBox =
    shape.boundingBox.exhibit(Effect.Opacity(opacity))

  override def bestBoundingShape: SimpleShape = shape.bestBoundingShape

  override def replace(replacementShape: Shape) = copy(shape = replacementShape)
}

case class FilteredShape(shape: Shape, filter: Filter)
    extends Shape with UnaryShapeOp {

  override lazy val boundingBox =
    shape.boundingBox.exhibit(Effect.Filtering(filter))

  override def bestBoundingShape: SimpleShape = shape.bestBoundingShape

  override def replace(replacementShape: Shape) = copy(shape = replacementShape)
}

case class AttributedShape(shape: Shape, attribution: Attribution)
    extends Shape with UnaryShapeOp {

  override lazy val boundingBox = shape.boundingBox.as(attribution)

  override def bestBoundingShape: SimpleShape = shape.bestBoundingShape

  override def replace(replacementShape: Shape) = copy(shape = replacementShape)
}


object Dims {
  def unapply(o: Dims) = Some(o.width, o.height)

  def minContaining(pt1: Point, pt2: Point): Dims = {
    val (minX, maxX) = MoreMath.minMax(pt1.x, pt2.x)
    val (minY, maxY) = MoreMath.minMax(pt1.y, pt2.y)
    val (width, height) =
      (if (maxX == minX) 0.001 else maxX - minX,
       if (maxY == minY) 0.001 else maxY - minY)
    val ctrPt: Point = (maxX - width/2, maxY - height/2)
    OriginDims(width, height) move (ctrPt.x, ctrPt.y)
  }
}


sealed abstract class Dims
    extends Transforming[Dims]
       with Placeable[Dims]
       with PresentableShape[Dims]
       with BoundingBoxed with Rectangular {

  override def boundingBox = this

  def asShape: SimpleShape // derived class of `Shape`

  type TranslatedT          = TranslatedDims
  protected val Translated  = TranslatedDims

  type ScaledT              = ScaledDims
  protected val Scaled      = ScaledDims

  type RotatedT             = RotatedDims
  protected val Rotated     = RotatedDims

  type ReflectedT           = ReflectedDims
  protected val Reflected   = ReflectedDims

  type SkewedHorizT         = SkewedHorizDims
  protected val SkewedHoriz = SkewedHorizDims

  type SkewedVertT          = SkewedVertDims
  protected val SkewedVert  = SkewedVertDims


  protected def createCompositeShape(other: Dims): Dims = {
    val (Dims(w1, h1), Dims(w2, h2)) = (this, other)
    val bb1Ctr = this.centerPt
    val bb2Ctr = other.centerPt
    val (halfW1, halfH1) = (w1/2, h1/2)
    val (halfW2, halfH2) = (w2/2, h2/2)
    val (bb1MinX, bb1MaxX, bb1MinY, bb1MaxY) =
        (bb1Ctr.x - halfW1, bb1Ctr.x + halfW1,
         bb1Ctr.y - halfH1, bb1Ctr.y + halfH1)
    val (bb2MinX, bb2MaxX, bb2MinY, bb2MaxY) =
        (bb2Ctr.x - halfW2, bb2Ctr.x + halfW2,
         bb2Ctr.y - halfH2, bb2Ctr.y + halfH2)
    val (minX, maxX, minY, maxY) =
        (math.min(bb1MinX, bb2MinX), math.max(bb1MaxX, bb2MaxX),
         math.min(bb1MinY, bb2MinY), math.max(bb1MaxY, bb2MaxY))
    Dims.minContaining((minX, minY), (maxX, maxY))
  }

  protected def createClippedShape(clipping: Dims, rule: ClipRule): Dims =
    // since `clipping` delimits what is shown of `this`, return that instead
    clipping

  protected def createMaskedShape(mask: Dims): Dims =
    // since `mask` delimits what is shown of `this`, return that instead
    mask

  protected def createInkedShape(pen: Pen): Dims = {
    // do not apply ink, merely return adjusted for size

    //!!!!this won't work for text, since stroke doesn't surround the figure!!!
    //!!!!!it may also fail when there is a nested pen has already enlarged the bounding box: containing pens would only change the color, not add more size!!!

    pen.width match {
      case Some(len) => this.scale((width + len)/width, (height + len)/height)
      case None => this
    }
  }

  protected def createEffectedShape(effect: Effect): Dims =
    // since effects alter the underlying shape in a (complicated) way that
    // always leaves its dimensions intact, return `this` unchanged
    this

  protected def createAttributedShape(attribution: Attribution): Dims =
    // attribution has no effect on bounding box
    this
}

case class OriginDims(width: Double, height: Double)
    extends Dims {

  override def centerPt = (0.0, 0.0)

  override def asShape: SimpleShape = Rectangle(width, height)
}

object TranslatedDims
    extends TranslatedTransformable[Dims] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[TranslatedDims]
}

case class TranslatedDims(rect: Dims, xDist: Double, yDist: Double)
    extends Dims {

  lazy val width  = rect.width
  lazy val height = rect.height

  override lazy val centerPt = rect.centerPt -+ (xDist, yDist)

  override def asShape: SimpleShape = rect.asShape.move(xDist, yDist)
}

object ScaledDims
    extends ScaledTransformable[Dims] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[ScaledDims]
}

case class ScaledDims(rect: Dims, xScaling: Double, yScaling: Double)
    extends Dims {

  lazy val width  = rect.width  * xScaling
  lazy val height = rect.height * yScaling

  override lazy val centerPt = rect.centerPt -* (xScaling, yScaling)

  override def asShape: SimpleShape = rect.asShape.scale(xScaling, yScaling)
}


trait DimsDisplacement {

  protected def calcDisplacedRect(rect: Dims): Dims = {
    val rectCtr = rect.centerPt
    val (rectHalfW, rectHalfH) = (rect.width/2, rect.height/2)
    val rectCornerPts: List[Point] = List(
        (rectCtr.x - rectHalfW, rectCtr.y - rectHalfH),
        (rectCtr.x - rectHalfW, rectCtr.y + rectHalfH),
        (rectCtr.x + rectHalfW, rectCtr.y - rectHalfH),
        (rectCtr.x + rectHalfW, rectCtr.y + rectHalfH)
      )
    val displacedCornerPts = rectCornerPts map calcDisplacement _
    val (minX, maxX) =
        (displacedCornerPts map { _.x } min,
         displacedCornerPts map { _.x } max)
    val (minY, maxY) =
        (displacedCornerPts map { _.y } min,
         displacedCornerPts map { _.y } max)
    Dims.minContaining((minX, minY), (maxX, maxY))
  }

  protected def calcDisplacement(pt: Point): Point
}


object RotatedDims
    extends RotatedTransformable[Dims] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[RotatedDims]

//????for some reason the following is not equiv to the above:
//  protected val isInstanceOfCompanion =
//    (_: Any).isInstanceOf[RotatedDims]
//
//[ERROR] .../tie/tie/src/main/scala/k_k_/graphics/tie/shapes.scala:1328: error: object creation impossible, since method isInstanceOfCompanion in trait RotatedTransformable of type (x: Any)Boolean is not defined
//[INFO] object RotatedDims
//[INFO]        ^
}

case class RotatedDims(
    rect: Dims,
    degrees: Double,
    xPivot: Double,
    yPivot: Double
  ) extends Dims with DimsDisplacement {

  lazy val width  = equivBoundingBox.width
  lazy val height = equivBoundingBox.height

  override def boundingBox = equivBoundingBox

  override def centerPt = equivBoundingBox.centerPt

  override def asShape: SimpleShape = equivBoundingBox.asShape


  protected def calcDisplacement(pt: Point): Point =
    pt.rotate(degrees, xPivot, yPivot)

  private lazy val equivBoundingBox = calcDisplacedRect(rect)
}

object ReflectedDims
    extends ReflectedTransformable[Dims] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[ReflectedDims]
}

case class ReflectedDims(
    rect: Dims,
    degrees: Double,
    xPivot: Double,
    yPivot: Double
  ) extends Dims with DimsDisplacement {

  lazy val width  = equivBoundingBox.width
  lazy val height = equivBoundingBox.height

  override def boundingBox = equivBoundingBox

  override def centerPt = equivBoundingBox.centerPt

  override def asShape: SimpleShape = equivBoundingBox.asShape


  protected def calcDisplacement(pt: Point): Point =
    pt.reflect(degrees, xPivot, yPivot)

  private lazy val equivBoundingBox = calcDisplacedRect(rect)
}

object SkewedHorizDims
    extends SkewedHorizTransformable[Dims] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[SkewedHorizDims]
}

case class SkewedHorizDims(rect: Dims, degrees: Double)
    extends Dims with DimsDisplacement {

  lazy val width  = equivBoundingBox.width
  lazy val height = equivBoundingBox.height

  override def boundingBox = equivBoundingBox

  override def centerPt = equivBoundingBox.centerPt

  override def asShape: SimpleShape = equivBoundingBox.asShape


  protected def calcDisplacement(pt: Point): Point =
    pt.skewHoriz(degrees)

  private lazy val equivBoundingBox = calcDisplacedRect(rect)
}

object SkewedVertDims
    extends SkewedVertTransformable[Dims] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[SkewedVertDims]
}

case class SkewedVertDims(rect: Dims, degrees: Double)
    extends Dims with DimsDisplacement {

  lazy val width  = equivBoundingBox.width
  lazy val height = equivBoundingBox.height

  override def boundingBox = equivBoundingBox

  override def centerPt = equivBoundingBox.centerPt

  override def asShape: SimpleShape = equivBoundingBox.asShape


  protected def calcDisplacement(pt: Point): Point =
    pt.skewVert(degrees)

  private lazy val equivBoundingBox = calcDisplacedRect(rect)
}

}


package object shapes {

import k_k_.graphics.tie.effects.Effect
import k_k_.graphics.tie.ink.Pen
import k_k_.graphics.tie.shapes.path.Path


object IdentityShape {
  def unapply(shape: Shape): Boolean =
    shape eq NullPFShape
}

// NOTE: define as `val` in package object rather than as object, to avoid need
// to always down-qualify to generalized type; e.g. to simplify the following:
//    ((NullShape: Shape) /: shapesSeq) ( _ -& _ )
//   to:
//    (NullShape /: shapesSeq) ( _ -& _ )
// case object NullShape extends InvisRectangle(0.0001, 0.0001) {

val NullPFShape: SimpleShape =
    new SimpleShape with NullaryShapeOp {

  val (allegedWidth, allegedHeight) = (0.0001, 0.0001)

  override def boundingBox = OriginDims(allegedWidth, allegedHeight)

  override def asPath = Path.from(0, 0).close

  override def toString = "NullShape"


  // there is only one NullShape, which is constant under every transform...

  override def move(xDist: Double, yDist: Double): SimpleShape =
    this

  override def scale(xScaling: Double, yScaling: Double): SimpleShape =
    this

  override def rotate(degrees: Double, aboutX: Double, aboutY: Double):
      SimpleShape =
    this

  override def reflect(degrees: Double, aboutX: Double, aboutY: Double):
      SimpleShape =
    this

  override def skewHoriz(degrees: Double): Shape =
    this

  override def skewVert(degrees: Double): Shape =
    this


  // ...results in an identity when combined with another shape

  override protected def createCompositeShape(other: Shape): Shape =
    other

  // ...clipped and masked with no effect

  override protected def createClippedShape(clipping: Shape, rule: ClipRule):
      Shape =
    this

  override protected def createMaskedShape(mask: Shape): Shape =
    this

  // ...unchanged under the pen

  override protected def createInkedShape(pen: Pen): Shape =
    this

  // ...impervious to every Effect

  override protected def createEffectedShape(effect: Effect): Shape =
    this

  // ...and likewise unattributable

  override protected def createAttributedShape(attribution: Attribution):
      Shape =
    this
}

val NullShape: Shape = NullPFShape

}


/*
    (ugly, messy!) snippet of eariler version of InvisRectangle.cloakRect(),
    prior to definition of ShapeOp:

        case ScaledShape(inner, xScaling, yScaling) =>
          ScaledShape(cloakRect(inner), xScaling, yScaling)
        case RotatedShape(inner, degrees, xPivot, yPivot) =>
          RotatedShape(cloakRect(inner), degrees, xPivot, yPivot)
        case CompositeShape(under, over) =>
          CompositeShape(cloakRect(under), cloakRect(over))
          //????????
        case ClippedShape(clipped, clipping, rule) =>
          ClippedShape(cloakRect(clipped), cloakRect(clipping), rule)
        case MaskedShape(masked, mask) =>
          MaskedShape(cloakRect(masked), cloakRect(mask))
        case InkedShape(inner, pen) =>
          InkedShape(cloakRect(inner), pen)
        case ( _ : Segment | _ : SimpleShape | _ : FreeForm | _ : Writing) =>
          shape // ouch!--no Rectangle here, return unchanged
*/
