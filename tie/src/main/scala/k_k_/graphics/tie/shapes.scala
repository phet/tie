/*
   file: k_k_/graphics/tie/shapes.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
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
import k_k_.graphics.tie.shapes.text.{Text, Default_Text_Ruler_Factory}
import k_k_.graphics.tie.effects._
import k_k_.graphics.tie.ink.Pen
import k_k_.graphics.tie.transform._

import k_k_.algo.{Math => More_Math}


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


object Bounding_Boxed {

  def apply(bboxed: Bounding_Boxed): Dims =
    bboxed match {
      case dims: Dims => dims
      case _          => bboxed.bounding_box
    }
}

trait Bounding_Boxed {

  def bounding_box: Dims

  def center_pt: Point = bounding_box.center_pt

  final def bounding_box_shape: Simple_Shape = bounding_box.as_shape

  final def bounding_box_path: Path = bounding_box_shape.as_path

  // aliases:
  final def bbox       = bounding_box
  final def bbox_shape = bounding_box_shape
  final def bbox_path  = bounding_box_path
}

trait Bounding_Shaped { self: Bounding_Boxed =>

  def best_bounding_shape: Simple_Shape = bounding_box.as_shape
}


sealed trait Shape_Op { self: Shape =>
}


object Nullary_Shape_Op {

  // WARNING: be careful when matching to not forget the empty parens ('()'),
  // so the compiler will know not to match this object (when matching an
  // extension of its companion trait is what is desired).  i.e. use:
  //   case nop @ Nullary_Shape_Op() => ...
  // not:
  //   case nop @ Nullary_Shape_Op => ...
  // or prepare for the error: pattern type is incompatible with expected type;
  // [INFO]  found   : object k_k_.graphics.tie.shapes.Nullary_Shape_Op
  // [INFO]  required: k_k_.graphics.tie.shapes.Shape
  // [INFO]         case Nullary_Shape_Op => (None, true)
  // [INFO]              ^
  def unapply(s: Nullary_Shape_Op): Boolean =
    true
}

trait Nullary_Shape_Op extends Shape_Op { self: Shape =>
}


object Unary_Shape_Op {

  def unapply(s: Unary_Shape_Op): Option[Shape] =
    Some(s.shape)
}

trait Unary_Shape_Op extends Shape_Op { self: Shape =>

  val shape: Shape

  def child =
    shape

  def child_=(replacement_shape: Shape) =
    replace(replacement_shape)

  // NOTE: `with` included in result type for symmetry with Binary_Shape_Op;
  // does enable the ridiculous: (u_shape_op.child = shape1).child = shape2
  def replace(replacement_shape: Shape): Shape with Unary_Shape_Op
}


object Binary_Shape_Op {

  def unapply(s: Binary_Shape_Op): Option[(Shape, Shape)] =
    Some(s.left, s.right)
}

trait Binary_Shape_Op extends Shape_Op { self: Shape =>

  def left:  Shape

  def right: Shape

  def left_=(replacement_shape: Shape) =
    replace_left(replacement_shape)

  def right_=(replacement_shape: Shape) =
    replace_right(replacement_shape)

  // NOTE: `with` in result type allows for chaining in the form of:
  //   (bin_shape_op.left = shape1).right = shape2
  def replace(replacement_left: Shape = left, replacement_right: Shape = right):
    Shape with Binary_Shape_Op

  def replace_left(replacement_left: Shape) =
    replace(replacement_left, right)

  def replace_right(replacement_right: Shape) =
    replace(left, replacement_right)
}


trait Shape_Traversal { self: Shape =>

  final def mapped(f: Shape => Shape): Shape =
    this match {
      case nshape @ Nullary_Shape_Op()    => f(nshape)
      case ushape @ Unary_Shape_Op(s)     => ushape.replace(f(s))
      case bshape @ Binary_Shape_Op(l, r) => bshape.replace(f(l), f(r))
    }

  // NOTE: distinct name precludes 'missing param type for expanded func' error
  final def mapped_when(pf: PartialFunction[Shape, Shape]): Shape = {

    val lifted_pf = pf.lift

    def walk(curr_shape: Shape): Option[Shape] =
      curr_shape match {
        case nshape @ Nullary_Shape_Op()    => lifted_pf(nshape)
        case ushape @ Unary_Shape_Op(s)     => walk(s).map(ushape.replace(_))
        case bshape @ Binary_Shape_Op(l, r) =>
          walk(l) match {
            case Some(s) => walk(r).map(bshape.replace(s, _)).orElse(
                                   Some(bshape.replace(s, r)))
            case None    => walk(r).map(bshape.replace(l, _))
          }
      }
    walk(this).getOrElse(this)

// impl. which would always rebuild the entire tree
//    def walk(curr_shape: Shape): Shape =
//      curr_shape match {
//        case s if pf.isDefinedAt(s)         => pf(s)
//        case nshape @ Nullary_Shape_Op()    => nshape
//        case ushape @ Unary_Shape_Op(s)     => ushape.replace(walk(s))
//        case bshape @ Binary_Shape_Op(l, r) => bshape.replace(walk(l),walk(r))
//      }
//    walk(this)//way!
  }

  final def map[T](ordering: => Seq[Shape])(f: Shape => T): Seq[T] =
    ordering map f

  // pre-order traversal:
  final def map[T](f: Shape => T): Seq[T] =
    map(pre_order)(f)

  final def collect[T](ordering: => Seq[Shape])
                      (pf: PartialFunction[Shape, T]): Seq[T] =
    (ordering map pf.lift) filter ( _ ne None ) map ( _.get )

  // pre-order traversal:
  final def collect[T](pf: PartialFunction[Shape, T]): Seq[T] =
    collect(pre_order)(pf)

  final def contains(ordering: => Seq[Shape])(elem: Shape): Boolean =
    ordering contains elem

  final def contains(elem: Shape): Boolean =
    contains(pre_order)(elem)


  final def count(ordering: => Seq[Shape])(pred: Shape => Boolean): Int =
    ordering count pred

  final def count(pred: Shape => Boolean): Int =
    count(pre_order)(pred)


  final def exists(ordering: => Seq[Shape])(pred: Shape => Boolean): Boolean =
    ordering exists pred

  final def exists(pred: Shape => Boolean): Boolean =
    exists(pre_order)(pred)


  final def find(ordering: => Seq[Shape])
                (pred: Shape => Boolean): Option[Shape] =
    ordering find pred

  final def find(pred: Shape => Boolean): Option[Shape] =
    find(pre_order)(pred)


  final def pre_order: Seq[Shape] = {
    def walk_in_steps(shape: Shape): Stream[Shape] =
      shape match {
        case nshape @ Nullary_Shape_Op() =>
          Stream(nshape)
        case ushape @ Unary_Shape_Op(s) =>
          Stream.cons(ushape, walk_in_steps(s))
        case bshape @ Binary_Shape_Op(l, r) =>
          Stream.cons(bshape, walk_in_steps(l)) append walk_in_steps(r)
      }
    walk_in_steps(this)
  }

  final def post_order: Seq[Shape] = {
    def walk_in_steps(shape: Shape): Stream[Shape] =
      shape match {
        case nshape @ Nullary_Shape_Op() =>
          Stream(nshape)
        case ushape @ Unary_Shape_Op(s) =>
          walk_in_steps(s) append Stream(ushape)
        case bshape @ Binary_Shape_Op(l, r) =>
          walk_in_steps(l) append walk_in_steps(r) append Stream(bshape)
      }
    walk_in_steps(this)
  }
}


sealed abstract class Clip_Rule
case object Non_Zero_Clip extends Clip_Rule // default
case object Even_Odd_Clip extends Clip_Rule
case object Inherit_Clip  extends Clip_Rule


trait Presentable_Shape[T <: Presentable_Shape[T]] { self: T =>

  def combo(over: T): T =
    create_composite_shape(over)

  def -&(over: T): T =
    combo(over)


  def clip_by(clipping: T, rule: Clip_Rule): T =
    create_clipped_shape(clipping, rule)

  def clip_by(clipping: T): T =
    create_clipped_shape(clipping, Non_Zero_Clip)

  def -<>(clipping: T, rule: Clip_Rule): T =
    clip_by(clipping, rule)

  def -<>(clipping: T): T =
    clip_by(clipping)


  def mask_by(mask: T): T =
    create_masked_shape(mask)

  def -<#>(mask: T): T =
    mask_by(mask)


  def using(pen: Pen): T =
    create_inked_shape(pen)

  def -~(pen: Pen): T =
    using(pen)


  def exhibit(effect: Effect): T =
    create_effected_shape(effect)

  def exhibit(opacity: Double): T =
    exhibit(Opacity_Effect(opacity))

  def exhibit(filter: Filter): T =
    exhibit(Filter_Effect(filter))


  def -#(effect: Effect): T =
    exhibit(effect)

  def -#(opacity: Double): T =
    exhibit(opacity)

  def -#(filter: Filter): T =
    exhibit(filter)


  def as(attribution: Attribution): T =
    create_attributed_shape(attribution)

  def as(attribution1: Attribution, attributions: Attribution*): T =
    // NOTE: evaluation respects operator's left-assoc. semantics
    (this.as(attribution1) /: attributions) { (instance, attribution) =>
      instance.as(attribution)
    }

  def as(id: String): T =
    create_attributed_shape(Id_Attribution(id))


  def -:(attribution: Attribution): T =
    as(attribution)

  def -:(attribution1: Attribution, attributions: Attribution*): T =
    // NOTE: evaluation respects operator's right-assoc. semantics
    (attributions :\ this) { (attribution, instance) =>
      instance.as(attribution)
    }.as(attribution1)

  def -:(id: String): T =
    as(Id_Attribution(id))


  def -:-(attribution: Attribution): T =
    as(attribution)

  def -:-(attribution1: Attribution, attributions: Attribution*): T =
    as(attribution1, attributions: _*)

  def -:-(id: String): T =
    as(Id_Attribution(id))


  protected def create_composite_shape(other: T): T

  protected def create_clipped_shape(clipping: T, rule: Clip_Rule): T

  protected def create_masked_shape(mask: T): T

  protected def create_inked_shape(pen: Pen): T

  protected def create_effected_shape(effect: Effect): T

  protected def create_attributed_shape(attribution: Attribution): T
}


object Shape {

  /** calculate 'least common fit' bounding box, with center at (0, 0)
   *
   *  definition: the 'least common fit' bounding box is the smallest bounding
   *  box capable of fully containing every respective bounding box of all
   *  `shapes`, if each of their bounding box were centered at (0, 0)
   */
  def common_fit_bounding_box(shapes: Traversable[Shape]): Dims =
    Origin_Dims.apply _ tupled
      ((0.0, 0.0) /: shapes) { (cf_dims, shape) =>
        val bb = shape.bounding_box
        (cf_dims._1 max bb.width,
         cf_dims._2 max bb.height)
      }

  // alias:
  val common_bbox = common_fit_bounding_box _
}

sealed abstract class Shape
    extends Transforming[Shape]
       with Placeable[Shape]
       with Presentable_Shape[Shape]
       with Bounding_Boxed with Bounding_Shaped
       with Shape_Traversal { self: Shape_Op =>

  // technically, this method with it's 'conversion' is useless; yet, it nicely
  // captures a useful invariant
  final def as_shape_op: Shape_Op = this


  type Translated_T          = Translated_Non_Simple_Shape
  protected val Translated   = Translated_Non_Simple_Shape

  type Scaled_T              = Scaled_Non_Simple_Shape
  protected val Scaled       = Scaled_Non_Simple_Shape

  type Rotated_T             = Rotated_Non_Simple_Shape
  protected val Rotated      = Rotated_Non_Simple_Shape

  type Reflected_T           = Reflected_Non_Simple_Shape
  protected val Reflected    = Reflected_Non_Simple_Shape

  type Skewed_Horiz_T        = Skewed_Horiz_Shape
  protected val Skewed_Horiz = Skewed_Horiz_Shape

  type Skewed_Vert_T         = Skewed_Vert_Shape
  protected val Skewed_Vert  = Skewed_Vert_Shape


  protected def create_composite_shape(other: Shape): Shape = {

    // combo successive Invis_Rectangle`s (for padding) into singular containing

    // NOTE: equiv. algo not possible to implement as method override in
    // Invis_Rectangle, due to treatment of translated(, etc.) Invis_Rectangle
    // (see is_invis_rect() below)

    // NOTE: code here is directly informed by the usage of Invis_Rectangle in 
    // _._._.tile.adjust.Shape_Adjustment_Methods.pad()--namely it is
    // neither scaled, rotated, reflected, skewed, nor inked, and is always
    // `under` in a composite

    def is_invis_rect(shape: Shape): Boolean =
      shape match {
        case Invis_Rectangle(_, _)         => true
        case Translated_Shape(inner, _, _) => is_invis_rect(inner)
        // no Invis_Rectangle is expected to be scaled, yet don't overlook here
        case Scaled_Shape(inner, _, _)     => is_invis_rect(inner)
        // thanks to this method, no invis rect shall be within nested composite
        //   case Composite_Shape(under, over)
        // no Invis_Rectangle is expected to be rotated, reflected, skewed, nor
        // inked-- stand by this!
        //   case Rotated_Shape(inner, _, _, _)
        //   case Reflected_Shape(inner, _, _, _)
        //   case Skewed_Horiz_Shape(inner, _)
        //   case Skewed_Vert_Shape(inner, _)
        //   case Inked_Shape(_, _)


//??????????what about Clipped_Shape??????????????

//??????????what about Masked_Shape??????????????

        case _ => false
      }

    def merge_invis_rects(r1: Shape, r2: Shape): Shape =
      // NOTE: crucial to combo, not each shape, but each shape's bounding_box,
      // to eliminate potential for infinite recursion, since
      // (Shape).combo implemented ITO this very method
      // Invis_Rectangle.cloak_rect((r1 -& r2).bounding_box.as_shape)
      Invis_Rectangle.cloak_rect((r1.bounding_box -& r2.bounding_box).as_shape)

    if (is_invis_rect(this)) {
      other match {
        case Composite_Shape(under, over) if is_invis_rect(under) =>
          Composite_Shape(merge_invis_rects(this, under), over)
        case _ if is_invis_rect(other) =>
          merge_invis_rects(this, other)
        case _ =>
          Composite_Shape(this, other)
      }
    } else {
      other match {
        case Identity_Shape() => this // special handling for 'identity' shape
        case _                => Composite_Shape(this, other)
      }
    }
  }

  protected def create_clipped_shape(clipping: Shape, rule: Clip_Rule): Shape =
    clipping match {
      case Identity_Shape() => Null_Shape // Identity_Shape clips everything
      case _                => Clipped_Shape(this, clipping, rule)
    }

  protected def create_masked_shape(mask: Shape): Shape =
    mask match {
      case Identity_Shape() => Null_Shape // Identity_Shape masks everything
      case _                => Masked_Shape(this, mask)
    }

  protected def create_inked_shape(pen: Pen): Shape =
    Inked_Shape(this, pen)

  protected def create_effected_shape(effect: Effect): Shape =
    effect match {
      case Opacity_Effect(opacity) => this match {
        case Non_Opaque_Shape(shape, prev_opacity) =>
          val resulting_opacity = prev_opacity * opacity
          if (resulting_opacity == 1.0)
            shape
          else
            Non_Opaque_Shape(shape, resulting_opacity)
        case _ =>
            Non_Opaque_Shape(this, opacity)
      }
      case Filter_Effect(filter)   => Filtered_Shape(this, filter)
    }

  protected def create_attributed_shape(attribution: Attribution): Shape =
    Attributed_Shape(this, attribution)
}


sealed abstract class True_Shape
    extends Shape { self: Shape_Op =>

  def as_path: Path
}


sealed abstract class Faux_Shape
    extends Shape with Nullary_Shape_Op

// for 'best' bounding shape (where shape must be necessarily 'closed')
sealed abstract class Simple_Shape
    extends True_Shape {  self: Shape_Op =>

//     with Transforming[Simple_Shape]
//     with Placeable[Simple_Shape] {


  def as_segments: Seq[Segment] =
    Nil //!!!!!!!!!!this should be fully abstract, not defined here!!!!!!!

  override
  def best_bounding_shape: Simple_Shape =
    this


/*?????it would seem to be equivalent to 'override' every method to have a
  covariant return type by extending `Transforming[Simple_Shape]`
  however it causes the following error, which seems only to be solved by
  overriding every method 'by hand' to specifically state the covariant return
  type--what could be done here???????

[ERROR] .../tie/tie/src/main/scala/k_k_/graphics/tie/shapes.scala:1096: error: illegal inheritance;
[INFO]  class Rotated_Simple_Shape inherits different type instances of trait Transforming:
[INFO] k_k_.graphics.tie.transformable.Transforming[k_k_.graphics.tie.shapes.Simple_Shape] and k_k_.graphics.tie.transformable.Transforming[k_k_.graphics.tie.shapes.Shape]
[INFO] final case class Rotated_Simple_Shape(shape: Simple_Shape,
[INFO]                  ^


  override type Translated_T        = Translated_Simple_Shape
  override protected val Translated = Translated_Simple_Shape

  override type Scaled_T            = Scaled_Simple_Shape
  override protected val Scaled     = Scaled_Simple_Shape

  override type Rotated_T           = Rotated_Simple_Shape
  override protected val Rotated    = Rotated_Simple_Shape

  override type Reflected_T         = Reflected_Simple_Shape
  override protected val Reflected  = Reflected_Simple_Shape
*/


  override
  def move(x_dist: Double, y_dist: Double): Simple_Shape =
    this match {
      case Translated_Simple_Shape(inner, existing_x_dist, existing_y_dist) =>
        val combined_x_dist = x_dist + existing_x_dist
        val combined_y_dist = y_dist + existing_y_dist
        if (combined_x_dist == 0.0 && combined_y_dist == 0.0)
          inner // successive ops cancel one another
        else
          // adjust 'previous' op by combining with sucessor
          Translated_Simple_Shape(inner, combined_x_dist, combined_y_dist)
      case _ =>
        Translated_Simple_Shape(this, x_dist, y_dist)
    }

  override
  def move(pt_offset: Point): Simple_Shape =
    move(pt_offset.x, pt_offset.y)

  override
  def move(dist: Double): Simple_Shape =
    move(dist, dist)

  override
  def -+(x_dist: Double, y_dist: Double): Simple_Shape =
    move(x_dist, y_dist)

  override
  def -+(pt_offset: Point): Simple_Shape =
    move(pt_offset.x, pt_offset.y)

  override
  def -+(dist: Double): Simple_Shape =
    move(dist, dist)


  override
  def to(dest_pt: Point): Simple_Shape =
    move(dest_pt -+ -center_pt)

  override
  def to(x_coord: Double, y_coord: Double): Simple_Shape =
    to(Point(x_coord, y_coord))

  override
  def -@(dest_pt: Point): Simple_Shape =
    to(dest_pt)

  override
  def -@(x_coord: Double, y_coord: Double): Simple_Shape =
    to(Point(x_coord, y_coord))


  override
  def scale(x_scaling: Double, y_scaling: Double): Simple_Shape = {
    this match {
      case Scaled_Simple_Shape(inner, existing_x_scaling,existing_y_scaling) =>
        val combined_x_scaling = x_scaling * existing_x_scaling
        val combined_y_scaling = y_scaling * existing_y_scaling
        if (combined_x_scaling == 1.0 && combined_y_scaling == 1.0)
          inner // successive ops cancel one another
        else
          // adjust 'previous' op by combining with sucessor
          Scaled_Simple_Shape(inner, combined_x_scaling, combined_y_scaling)
      case _ =>
        Scaled_Simple_Shape(this, x_scaling, y_scaling)
    }
  }


  override
  def scale(scaling: Double): Simple_Shape =
    scale(scaling, scaling)

  override
  def -*(x_scaling: Double, y_scaling: Double): Simple_Shape =
    scale(x_scaling, y_scaling)

  override
  def -*(scaling: Double): Simple_Shape =
    scale(scaling, scaling)


  override
  def rotate(degrees: Double, about_x: Double, about_y: Double): Simple_Shape =
    (about_x, about_y, this) match {
      // NOTE: simplify only when both rotate about (0,0)--else too complicated
      case (0, 0, Rotated_Simple_Shape(inner, existing_degrees, 0, 0)) =>
        val combined_degrees = degrees + existing_degrees
        if (combined_degrees % 360 == 0.0)
          inner // successive ops cancel one another
        else
          // adjust 'previous' op by combining with sucessor
          Rotated_Simple_Shape(inner, combined_degrees, 0, 0)
      case _ =>
        Rotated_Simple_Shape(this, degrees, about_x, about_y)
    }

  override
  def rotate(degrees: Double, center_pt: Point = Point(0, 0)): Simple_Shape =
    rotate(degrees, center_pt.x, center_pt.y)

  override
  def -%(degrees: Double, about_x: Double, about_y: Double): Simple_Shape =
    rotate(degrees, about_x, about_y)

  override
  def -%(degrees: Double, center_pt: Point = Point(0, 0)): Simple_Shape =
    rotate(degrees, center_pt.x, center_pt.y)


  override
  def reflect(degrees: Double, about_x: Double, about_y: Double): Simple_Shape =
    (about_x, about_y, this) match {
      // NOTE: simplify only when both reflect about (0,0)--else too complicated
      case (0, 0, Reflected_Simple_Shape(inner, existing_degrees, 0, 0))
             if ((degrees % 360) == (existing_degrees % 360)) =>
          inner // successive ops cancel one another
      case _ =>
        Reflected_Simple_Shape(this, degrees, about_x, about_y)
    }

  override
  def reflect(degrees: Double, about_pt: Point = Point(0, 0)): Simple_Shape =
    reflect(degrees, about_pt.x, about_pt.y)

  override
  def -|-(degrees: Double, about_x: Double, about_y: Double): Simple_Shape =
    reflect(degrees, about_x, about_y)

  override
  def -|-(degrees: Double, about_pt: Point = Point(0, 0)): Simple_Shape =
    reflect(degrees, about_pt.x, about_pt.y)
}


sealed abstract class Segment
    extends True_Shape with Nullary_Shape_Op

object Line {

  def between(p1: Point, p2: Point): Shape =
    if (p1 == p2) Line(0.001) -@ p1
    else {
      val (run, rise) = p2 - p1
      val rotate_degrees = {
        if      (rise == 0.0) 0
        else if (run  == 0.0) if (rise > 0) 90 else 270
        else math.toDegrees(math.atan(rise / run))
      }
      val length = p1.distance(p2)
      val start: Point = (if (run < 0.0) length/2 else -length/2, 0.0)
      val (move_x, move_y) = p1 - start.rotate(rotate_degrees)
      Line(length).rotate(rotate_degrees).move(move_x, move_y)
    }
}

final case class Line(length: Double)
    extends Segment {

  def bounding_box =
    Origin_Dims(length, 0.01)

  def as_path = {
    val upper_left: Point = (0 - length/2, 0.0)
    Path.from(upper_left).
         horiz(length)
         // NOTE: no `close`, since overstroke may double effective stroke-width
  }
}


final case class Hemisphere(rad_width: Double, rad_height: Double,
                            is_top_half: Boolean = true)
    extends Segment {

  def bounding_box =
    Origin_Dims(rad_width * 2, rad_height)

  def as_path = {
    val left_start = Point(0 - rad_width,
                           if (is_top_half) rad_height/2 else 0 - rad_height/2)
    Path.from(left_start).
         arc(rad_width, rad_height, if (is_top_half) Large_CW else Large_CCW,
             rad_width * 2, 0)
  }
}

object Iso_Triangle {

  def apply(base_width: Double, height: Double) =
    new Iso_Triangle(base_width, height)

  def unapply(iso_tri: Iso_Triangle) =
    Some(iso_tri.base_width, iso_tri.height)
}

sealed class Iso_Triangle(val base_width: Double, val height: Double)
    extends Simple_Shape with Nullary_Shape_Op {

  def bounding_box =
    Origin_Dims(base_width, height)

  def as_path = {
    val top_point: Point = (0.0, 0 - height/2) 
    val (slant_width, slant_height) = (base_width/2, height) 
    Path.from(top_point).
         line(slant_width, slant_height).
         horiz(-base_width).
         close
  }

  override def toString =
    "Iso_Triangle(" + base_width + "," + height + ")"
}

final case class Right_Triangle(base_width: Double, height: Double)
    extends Simple_Shape with Nullary_Shape_Op {

  def bounding_box =
    Origin_Dims(base_width, height)

  def as_path = {
    val top_point: Point = (0 + base_width/2, 0 - height/2) 
    Path.from(top_point).
         vert(height).
         horiz(-base_width).
         close
  }
}

object Rectangle {

  def apply(width: Double, height: Double) =
    new Rectangle(width, height)

  def unapply(rect: Rectangle) =
    Some(rect.width, rect.height)
}

sealed class Rectangle(val width: Double, val height: Double)
    extends Simple_Shape with Rectangular with Nullary_Shape_Op {

  def bounding_box =
    Origin_Dims(width, height)

  def as_path = {
    val upper_left: Point = (0 - width/2, 0 - height/2)
    Path.from(upper_left).
         horiz( width).vert( height).
         horiz(-width).vert(-height). //???delete vert????
         close
  }

  override def toString =
    "Rectangle(" + width + "," + height + ")"
}

final case class Parallelogram(side_width: Double, full_width: Double,
                               height: Double)
    extends Simple_Shape with Nullary_Shape_Op {

  def bounding_box =
    Origin_Dims(full_width, height)

  def as_path = {
  //??????is rearranging these a good idea or not?????
    val (long_width, short_width) = More_Math.max_min(side_width, full_width)
    val slant_width = long_width - short_width
    val upper_left: Point = (0 - (long_width/2 - slant_width),
                             0 - height/2) 
    Path.from(upper_left).
         horiz( short_width).
         line(-slant_width, height).
         horiz(-short_width).
         close
  }
}

final case class Trapezoid(top_width: Double, bottom_width: Double,
                           height: Double)
    extends Simple_Shape with Nullary_Shape_Op {

  def bounding_box =
    Origin_Dims(math.max(top_width, bottom_width), height)

  def as_path = {
    val upper_left: Point = (0 - top_width/2, 0 - height/2) 
    val (slant_width, slant_height) = ((bottom_width - top_width) / 2, height)
    Path.from(upper_left).
         horiz(top_width).
         line(slant_width, slant_height).
         horiz(-bottom_width).
         close
  }
}

object Pentagon {

  def apply(side_width: Double, full_width: Double,
            side_height: Double, full_height: Double) =
    new Pentagon(side_width, full_width, side_height, full_height)

  def unapply(pent: Pentagon) =
    Some(pent.side_width, pent.full_width, pent.side_height, pent.full_height)
}

sealed class Pentagon(val side_width: Double,  val full_width: Double,
                      val side_height: Double, val full_height: Double)
    extends Simple_Shape with Nullary_Shape_Op {

  def bounding_box =
    Origin_Dims(full_width, full_height)

  def as_path = {
    val top_point: Point = (0.0, 0 - full_height/2)
    val (tip_slant_width, tip_slant_height) = (full_width / 2,
                                               full_height - side_height)
    val (base_slant_width, base_slant_height) = ((full_width - side_width) / 2,
                                                 side_height)
    Path.from(top_point).
         line( tip_slant_width,   tip_slant_height).
         line(-base_slant_width,  base_slant_height).
         horiz(-side_width).
         line(-base_slant_width, -base_slant_height).
         close
  }

  override def toString =
    "Pentagon(" + side_width  + "," + full_width + "," +
                  side_height + "," + full_height + ")"
}

object Hexagon {

  def apply(side_width: Double, full_width: Double, height: Double) =
    new Hexagon(side_width, full_width, height)

  def unapply(hex: Hexagon) =
    Some(hex.side_width, hex.full_width, hex.height)
}

sealed class Hexagon(val side_width: Double, val full_width: Double,
                     val height: Double)
    extends Simple_Shape with Nullary_Shape_Op {

  def bounding_box =
    Origin_Dims(full_width, height)

  def as_path = {
    val upper_left: Point = (0 - side_width/2, 0 - height/2)
      //?????????possibly rearrange args to use larger of two?????????
    val (slant_width, slant_height) = ((full_width - side_width) / 2,
                                       height / 2)
    Path.from(upper_left).
         horiz( side_width).line( slant_width,  slant_height).
                            line(-slant_width,  slant_height).
         horiz(-side_width).line(-slant_width, -slant_height).
         close
  }

  override def toString =
    "Hexagon(" + side_width  + "," + full_width + "," + height + ")"
}

object Octagon {

  def apply(side_width: Double, full_width: Double,
            side_height: Double, full_height: Double) =
    new Octagon(side_width, full_width, side_height, full_height)

  def unapply(oct: Octagon) =
    Some(oct.side_width, oct.full_width, oct.side_height, oct.full_height)
}

sealed class Octagon(val side_width: Double,  val full_width: Double,
                     val side_height: Double, val full_height: Double)
    extends Simple_Shape with Nullary_Shape_Op {

  def bounding_box =
    Origin_Dims(full_width, full_height)

  def as_path = {
    val upper_left: Point = (0 - side_width/2, 0 - full_height/2)
      //?????????possibly rearrange args to use larger of two?????????
    val (slant_width, slant_height) = ((full_width  - side_width)  / 2,
                                       (full_height - side_height) / 2)
    Path.from(upper_left).
        horiz( side_width).line( slant_width,  slant_height).vert( side_height).
                           line(-slant_width,  slant_height).
        horiz(-side_width).line(-slant_width, -slant_height).vert(-side_height).
        close
  }

  override def toString =
    "Octagon(" + side_width  + "," + full_width + "," +
                 side_height + "," + full_height + ")"
}

object Ellipse {

  def apply(rad_width: Double, rad_height: Double) =
    new Ellipse(rad_width, rad_height)

  def unapply(e: Ellipse) =
    Some(e.rad_width, e.rad_height)
}

sealed class Ellipse(val rad_width: Double, val rad_height: Double)
    extends Simple_Shape with Nullary_Shape_Op {

  def bounding_box =
    Origin_Dims(rad_width*2, rad_height*2)

  def as_path = {
    val left_start:  Point = (0 - rad_width, 0.0)
    // NOTE: for SVG 1.1, when arc coords equal current pt., the arc is NOT
    // rendered--even when choosing Large_{C}CW arc! therefore, compose ellipse
    // from top and bottom 'half' arcs, before closing.
    Path.from(left_start).
         arc(rad_width, rad_height, Large_CW,   rad_width * 2,  0).
         arc(rad_width, rad_height, Large_CW, -(rad_width * 2), 0).
         close
  }

  override def toString =
    "Ellipse(" + rad_width + "," + rad_height + ")"
}


object Free_Form {

  def apply(path: Path) =
    new Free_Form(path)

  def unapply(free_form: Free_Form) =
    Some(free_form.path)
}

sealed class Free_Form(val path: Path)
    extends True_Shape with Nullary_Shape_Op {

  def bounding_box = {

    import Point._

    def calc_max_boundaries: (Point, Point) = {

      import scala.annotation.tailrec

      @tailrec // ensure tail-call-optimization to handle long Path cmd seqs
      def track_boundary_growth(remaining_cmds: List[Path_Cmd],
                                path_memory: Path.Pos_Memory,
                                curr_bounds: Option[(Point, Point)]):
          (Point, Point) = {
        remaining_cmds match {
          case Nil => curr_bounds match {
            case Some(bounds) => bounds
            case None => ((0.0, 0.0), (0.0, 0.0))
          }
          case cmd :: more_cmds =>
            cmd match {
              // position/path-management commands:
              case Move_Abs(x, y) =>
                track_boundary_growth(more_cmds,
                                      path_memory.start_sub_path_abs(x, y),
                                      curr_bounds)
              case Move_Rel(x, y) =>
                track_boundary_growth(more_cmds,
                                      path_memory.start_sub_path_rel(x, y),
                                      curr_bounds)
              case Close =>
                track_boundary_growth(more_cmds,
                                      path_memory.close_sub_path,
                                      curr_bounds)

              // line-drawing commands:
              case Line_Abs(x, y) =>
                val new_path_memory = path_memory.replace_pt_abs(x, y)
                val new_bounds = record_line(path_memory.curr_pt,
                                             new_path_memory, curr_bounds)
                track_boundary_growth(more_cmds, new_path_memory, new_bounds)
              case Line_Rel(x, y) =>
                val new_path_memory = path_memory.replace_pt_rel(x, y)
                val new_bounds = record_line(path_memory.curr_pt,
                                             new_path_memory, curr_bounds)
                track_boundary_growth(more_cmds, new_path_memory, new_bounds)
              case Horizontal_Abs(x) =>
                val new_path_memory = path_memory.replace_pt_horiz_abs(x)
                val new_bounds = record_line(path_memory.curr_pt,
                                             new_path_memory, curr_bounds)
                track_boundary_growth(more_cmds, new_path_memory, new_bounds)
              case Horizontal_Rel(x) =>
                val new_path_memory = path_memory.replace_pt_rel(x, 0)
                val new_bounds = record_line(path_memory.curr_pt,
                                             new_path_memory, curr_bounds)
                track_boundary_growth(more_cmds, new_path_memory, new_bounds)
              case Vertical_Abs(y) =>
                val new_path_memory = path_memory.replace_pt_vert_abs(y)
                val new_bounds = record_line(path_memory.curr_pt,
                                             new_path_memory, curr_bounds)
                track_boundary_growth(more_cmds, new_path_memory, new_bounds)
              case Vertical_Rel(y) =>
                val new_path_memory = path_memory.replace_pt_rel(0, y)
                val new_bounds = record_line(path_memory.curr_pt,
                                             new_path_memory, curr_bounds)
                track_boundary_growth(more_cmds, new_path_memory, new_bounds)

              // non-linear-drawing commands:




              //!!!!!!!!!for now, to keep things simple, calculate as if these merely specified a line from the current position to the cmd's end point!!!!!!!!




              case Elliptical_Arc_Abs(rad_width, rad_height, x_rotate_degrees,
                                      kind: Arc_Choice, x, y) =>
                val new_path_memory = path_memory.replace_pt_abs(x, y)
                val new_bounds = record_line(path_memory.curr_pt,
                                             new_path_memory, curr_bounds)
                track_boundary_growth(more_cmds, new_path_memory, new_bounds)
              case Elliptical_Arc_Rel(rad_width, rad_height, x_rotate_degrees,
                                      kind: Arc_Choice, x, y) =>
                val new_path_memory = path_memory.replace_pt_rel(x, y)
                val new_bounds = record_line(path_memory.curr_pt,
                                             new_path_memory, curr_bounds)
                track_boundary_growth(more_cmds, new_path_memory, new_bounds)
              case Quad_Bezier_Abs(x_ctl1, y_ctl1, x, y) =>
                val new_path_memory = path_memory.replace_pt_abs(x, y)
                val new_bounds = record_line(path_memory.curr_pt,
                                             new_path_memory, curr_bounds)
                track_boundary_growth(more_cmds, new_path_memory, new_bounds)
              case Quad_Bezier_Rel(x_ctl1, y_ctl1, x, y) =>
                val new_path_memory = path_memory.replace_pt_rel(x, y)
                val new_bounds = record_line(path_memory.curr_pt,
                                             new_path_memory, curr_bounds)
                track_boundary_growth(more_cmds, new_path_memory, new_bounds)
              case Tangent_Quad_Bezier_Abs(x, y) =>
                val new_path_memory = path_memory.replace_pt_abs(x, y)
                val new_bounds = record_line(path_memory.curr_pt,
                                             new_path_memory, curr_bounds)
                track_boundary_growth(more_cmds, new_path_memory, new_bounds)
              case Tangent_Quad_Bezier_Rel(x, y) =>
                val new_path_memory = path_memory.replace_pt_rel(x, y)
                val new_bounds = record_line(path_memory.curr_pt,
                                             new_path_memory, curr_bounds)
                track_boundary_growth(more_cmds, new_path_memory, new_bounds)
              case Cubic_Bezier_Abs(x_ctl1, y_ctl1, x_ctl2, y_ctl2, x, y) =>
                val new_path_memory = path_memory.replace_pt_abs(x, y)
                val new_bounds = record_line(path_memory.curr_pt,
                                             new_path_memory, curr_bounds)
                track_boundary_growth(more_cmds, new_path_memory, new_bounds)
              case Cubic_Bezier_Rel(x_ctl1, y_ctl1, x_ctl2, y_ctl2, x, y) =>
                val new_path_memory = path_memory.replace_pt_rel(x, y)
                val new_bounds = record_line(path_memory.curr_pt,
                                             new_path_memory, curr_bounds)
                track_boundary_growth(more_cmds, new_path_memory, new_bounds)
              case Tangent_Cubic_Bezier_Abs(x_ctl1, y_ctl1, x, y) =>
                val new_path_memory = path_memory.replace_pt_abs(x, y)
                val new_bounds = record_line(path_memory.curr_pt,
                                             new_path_memory, curr_bounds)
                track_boundary_growth(more_cmds, new_path_memory, new_bounds)
              case Tangent_Cubic_Bezier_Rel(x_ctl1, y_ctl1, x, y) =>
                val new_path_memory = path_memory.replace_pt_rel(x, y)
                val new_bounds = record_line(path_memory.curr_pt,
                                             new_path_memory, curr_bounds)
                track_boundary_growth(more_cmds, new_path_memory, new_bounds)
            }
        }
      }

      def record_line(start_pos: Point, finish_path_memory: Path.Pos_Memory,
                      curr_bounds: Option[(Point, Point)]) = {
        val finish_pos = finish_path_memory.curr_pt
        val (line_min_x, line_max_x) =
          More_Math.min_max(start_pos.x, finish_pos.x)
        val (line_min_y, line_max_y) =
          More_Math.min_max(start_pos.y, finish_pos.y)
        curr_bounds match {
          case None =>
            Some((Point(line_min_x, line_min_y),
                  Point(line_max_x, line_max_y)))

          case Some(prev_bounds @ (Point(min_x, min_y), Point(max_x, max_y))) =>
            // NOTE: providing type for `adjustments` is most succinct way
            // to type-check: it precludes need to specify param type and to
            // explicitly construct `Point` in result of each anon. func., and
            // it enables direct use `prev_bounds` '@-alias' later in `foldLeft`
            val adjustments: List[((Point, Point)) => (Point, Point)] = List(
              { (bounds) =>
                if (line_min_x < min_x)
                  ((line_min_x, bounds._1.y), bounds._2)
                else bounds
              },
              { (bounds) =>
                if (line_max_x > max_x)
                  (bounds._1, (line_max_x, bounds._2.y))
                else bounds
              },
              { (bounds) =>
                if (line_min_y < min_y)
                  ((bounds._1.x, line_min_y), bounds._2)
                else bounds
              },
              { (bounds) =>
                if (line_max_y > max_y)
                  (bounds._1, (bounds._2.x, line_max_y))
                else bounds
              })
            val new_bounds = (prev_bounds /: adjustments) {
              (bounds, adjust) => adjust(bounds)
            }
            Some(new_bounds)
        }
      }

      val (initial_pos_memory, remaining_cmds) = path.init_pos_memory
      track_boundary_growth(remaining_cmds, initial_pos_memory, None)
    }

    val (min_pt, max_pt) = calc_max_boundaries
    Dims.min_containing(min_pt, max_pt)
  }

  def as_path =
    path

  override def toString =
    "Free_Form(" + path + ")"
}


object Custom {

  def unapply(c: Custom) =
    Some(c.path)
}

// NOTE: intended as extension point: neither sealed nor final
class Custom protected(protected val custom_path: Path)
    extends Free_Form(custom_path)


object Writing {

  val text_ruler_factory = Default_Text_Ruler_Factory
}

final case class Writing(text: Text)
    extends Faux_Shape {

  def bounding_box =
    text.text_bounding_box(Writing.text_ruler_factory)
}


object Image {

  def apply(path: String) =
    new Image(path)

  def apply(path: String, path_mapper: String => String) =
    new Image(path, path_mapper)

  def apply(path: String, width: Double, height: Double,
            path_mapper: (String => String) = identity) =
    new Image(path, width, height, path_mapper)

  def unapply(img: Image): Option[(String, Double, Double)] =
    Some(img.path, img.width, img.height)


  private def calc_dims(fpath: String): (Double, Double) = {
    val buffered_img = ImageIO.read(new File(fpath))
    (buffered_img.getWidth, buffered_img.getHeight)
  }

  private def verify_fpath_exists(path: String): String =
    if (new File(path).exists) path
    else throw new FileNotFoundException(path)
}

final class Image private (val path: String, dims: (Double, Double),
                           path_mapper: String => String)
    extends Faux_Shape with Rectangular {

  def this(path: String, path_mapper: String => String) =
    this(path, Image.calc_dims(path), path_mapper)

  def this(path: String) =
    this(path, identity)

  def this(path: String, width: Double, height: Double,
           path_mapper: String => String = identity) =
    this(Image.verify_fpath_exists(path), (width, height), path_mapper)

  val width  = dims._1

  val height = dims._2

  val mapped_fpath = path_mapper(path)

  lazy val bounding_box =
    Origin_Dims(width, height)

  override def toString =
    "Image(" + path + "," + dims + "," + path_mapper(path) + ")"
}


final case class Equi_Triangle(length: Double)
    extends Iso_Triangle(length, length * math.sqrt(3.0)/2)


final case class Square(length: Double)
    extends Rectangle(length, length)


object Reg_Pentagon {

  // 'golden ratio' == chords / sides (of regular pentagon)
  val phi = (1 + math.sqrt(5)) / 2

  val Phi = phi - 1 // == 1 / phi (neato!)

  def of_sides(length: Double) =
    Reg_Pentagon(length * phi)
}

import Reg_Pentagon.{phi, Phi}

final case class Reg_Pentagon(shape_width: Double)
     extends Pentagon(shape_width * Phi, shape_width,
                      (shape_width * Phi / 2) * math.sqrt(4 - Phi*Phi),
                      shape_width * Phi * math.sqrt((phi*phi) - .25))


object Reg_Hexagon {

  def of_sides(length: Double) =
    Reg_Hexagon(length * 2)
}

final case class Reg_Hexagon(shape_width: Double)
     extends Hexagon(shape_width/2, shape_width,
                     shape_width/2 * math.sqrt(3.0))


object Reg_Octagon {

  def of_sides(length: Double) =
    Reg_Octagon(length * (1 + math.sqrt(2.0)))
}

final case class Reg_Octagon(shape_width: Double)
     extends Octagon(shape_width / (1 + math.sqrt(2.0)), shape_width,
                     shape_width / (1 + math.sqrt(2.0)), shape_width)


object Circle {

  def apply(rad: Double) =
    new Circle(rad)

  def unapply(circ: Circle) =
    Some(circ.rad)
}

sealed class Circle(val rad: Double)
    extends Ellipse(rad, rad) {

  override def toString =
    "Circle(" + rad + ")"
}


object Invis_Rectangle {

  // HINT: an expensive noop--be sure `shape` actually contains a Rectangle!
  def cloak_rect(shape: Shape): Shape =
    shape match {
      case Rectangle(width, height) =>
        Invis_Rectangle(width, height)
      case Translated_Shape(inner, x_dist, y_dist) =>
        Translated_Shape(cloak_rect(inner), x_dist, y_dist)

      // NOTE: no Invis_Rectangle is expected to be scaled, rotated, reflected
      // skewed, inked, nor composed (in non-'under' pos); yet, for
      // robustness, implement anyway:
      case ushape @ Unary_Shape_Op(s)     => ushape.replace(cloak_rect(s))
      case bshape @ Binary_Shape_Op(l, r) => bshape.replace(cloak_rect(l),
                                                            cloak_rect(r))
      case nshape @ Nullary_Shape_Op()    =>
        nshape // ouch!--no Rectangle here, return unchanged
    }
}

sealed case class Invis_Rectangle(w: Double, h: Double)
    extends Rectangle(w, h)


// NOTE: Diam_Ellipse, Diam_Circle allow for substitutability w/ Rectangle, ala:
//     val Shape_Class = if (foo) Rectangle else Diam_Ellipse
//     val shape = Shape_Class(50, 20)

final case class Diam_Ellipse(diam_width: Double, diam_height: Double)
    extends Ellipse(diam_width/2, diam_height/2)

final case class Diam_Circle(diam: Double)
    extends Circle(diam/2)


object Translated_Shape {

  def apply(shape: Shape, x_dist: Double, y_dist: Double) =
    Translated_Non_Simple_Shape(shape, x_dist, y_dist)

  def unapply(shape: Shape) =
    shape match {
      case s: Translated_Non_Simple_Shape =>
        Some(s.shape, s.x_dist, s.y_dist)
      case s: Translated_Simple_Shape =>
        Some(s.shape, s.x_dist, s.y_dist)
      case _ => None
    }
}

object Translated_Non_Simple_Shape
    extends Translated_Transformable[Shape] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Translated_Non_Simple_Shape]
}

final case class Translated_Non_Simple_Shape(shape: Shape,
                                             x_dist: Double, y_dist: Double)
    extends Shape with Unary_Shape_Op {

  lazy val bounding_box =
    shape.bounding_box.move(x_dist, y_dist)

  def replace(replacement_shape: Shape) =
    copy(shape = replacement_shape)
}

/*!!!not needed since not possible for Simple_Shape to extend Transforming[Simple_Shape]!!!

object Translated_Simple_Shape
    extends Translated_Transformable[Simple_Shape] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Translated_Simple_Shape]
}
*/

final case class Translated_Simple_Shape(shape: Simple_Shape,
                                         x_dist: Double, y_dist: Double)
    extends Simple_Shape with Unary_Shape_Op {

  lazy val bounding_box =
    shape.bounding_box.move(x_dist, y_dist)

  def as_path: Path =
    shape.as_path.move(x_dist, y_dist)

  def replace(replacement_shape: Shape) =
    replacement_shape match {
      case simple_shape: Simple_Shape =>
        copy(shape = simple_shape)
      case _ =>
        Translated_Shape(replacement_shape, x_dist, y_dist)
    }
}


object Scaled_Shape {

  def apply(shape: Shape, x_scaling: Double, y_scaling: Double) =
    Scaled_Non_Simple_Shape(shape, x_scaling, y_scaling)

  def unapply(shape: Shape) =
    shape match {
      case s: Scaled_Non_Simple_Shape =>
        Some(s.shape, s.x_scaling, s.y_scaling)
      case s: Scaled_Simple_Shape =>
        Some(s.shape, s.x_scaling, s.y_scaling)
      case _ => None
    }
}

object Scaled_Non_Simple_Shape
    extends Scaled_Transformable[Shape] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Scaled_Non_Simple_Shape]
}

final case class Scaled_Non_Simple_Shape(shape: Shape,
                                         x_scaling: Double, y_scaling: Double)
    extends Shape with Unary_Shape_Op {

  lazy val bounding_box =
    shape.bounding_box.scale(x_scaling, y_scaling)

  def replace(replacement_shape: Shape) =
    copy(shape = replacement_shape)
}

final case class Scaled_Simple_Shape(shape: Simple_Shape,
                                     x_scaling: Double, y_scaling: Double)
    extends Simple_Shape with Unary_Shape_Op {

  lazy val bounding_box =
    shape.bounding_box.scale(x_scaling, y_scaling)

  def as_path: Path =
    shape.as_path.scale(x_scaling, y_scaling)

  def replace(replacement_shape: Shape) =
    replacement_shape match {
      case simple_shape: Simple_Shape =>
        copy(shape = simple_shape)
      case _ =>
        Scaled_Shape(replacement_shape, x_scaling, y_scaling)
    }
}


object Rotated_Shape {

  def apply(shape: Shape,
            degrees: Double, x_pivot: Double, y_pivot: Double) =
    Rotated_Non_Simple_Shape(shape, degrees, x_pivot, y_pivot)

  def unapply(shape: Shape) =
    shape match {
      case s: Rotated_Non_Simple_Shape =>
        Some(s.shape, s.degrees, s.x_pivot, s.y_pivot)
      case s: Rotated_Simple_Shape =>
        Some(s.shape, s.degrees, s.x_pivot, s.y_pivot)
      case _ => None
    }
}

object Rotated_Non_Simple_Shape
    extends Rotated_Transformable[Shape] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Rotated_Non_Simple_Shape]
}

final case class Rotated_Non_Simple_Shape(shape: Shape, degrees: Double,
                                          x_pivot: Double, y_pivot: Double)
    extends Shape with Unary_Shape_Op {

  lazy val bounding_box =
    shape.bounding_box.rotate(degrees, x_pivot, y_pivot)

  def replace(replacement_shape: Shape) =
    copy(shape = replacement_shape)
}

final case class Rotated_Simple_Shape(shape: Simple_Shape, degrees: Double,
                                      x_pivot: Double, y_pivot: Double)
    extends Simple_Shape with Unary_Shape_Op {

  lazy val bounding_box =
    shape.bounding_box.rotate(degrees, x_pivot, y_pivot)

  def as_path: Path =
    shape.as_path.rotate(degrees, x_pivot, y_pivot)

  def replace(replacement_shape: Shape) =
    replacement_shape match {
      case simple_shape: Simple_Shape =>
        copy(shape = simple_shape)
      case _ =>
        Rotated_Shape(replacement_shape, degrees, x_pivot, y_pivot)
    }
}


object Reflected_Shape {

  def apply(shape: Shape, degrees: Double, x_pivot: Double, y_pivot: Double) =
    Reflected_Non_Simple_Shape(shape, degrees, x_pivot, y_pivot)

  def unapply(shape: Shape) =
    shape match {
      case s: Reflected_Non_Simple_Shape =>
        Some(s.shape, s.degrees, s.x_pivot, s.y_pivot)
      case s: Reflected_Simple_Shape =>
        Some(s.shape, s.degrees, s.x_pivot, s.y_pivot)
      case _ => None
    }
}

object Reflected_Non_Simple_Shape
    extends Reflected_Transformable[Shape] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Reflected_Non_Simple_Shape]
}

final case class Reflected_Non_Simple_Shape(shape: Shape, degrees: Double,
                                            x_pivot: Double, y_pivot: Double)
    extends Shape with Unary_Shape_Op {

  lazy val bounding_box =
    shape.bounding_box.reflect(degrees, x_pivot, y_pivot)

  def replace(replacement_shape: Shape) =
    copy(shape = replacement_shape)
}

final case class Reflected_Simple_Shape(shape: Simple_Shape, degrees: Double,
                                        x_pivot: Double, y_pivot: Double)
    extends Simple_Shape with Unary_Shape_Op {

  lazy val bounding_box =
    shape.bounding_box.reflect(degrees, x_pivot, y_pivot)

  def as_path: Path =
    shape.as_path.reflect(degrees, x_pivot, y_pivot)

  def replace(replacement_shape: Shape) =
    replacement_shape match {
      case simple_shape: Simple_Shape =>
        copy(shape = simple_shape)
      case _ =>
        Reflected_Shape(replacement_shape, degrees, x_pivot, y_pivot)
    }
}


object Skewed_Horiz_Shape extends Skewed_Horiz_Transformable[Shape] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Skewed_Horiz_Shape]
}

final case class Skewed_Horiz_Shape(shape: Shape, degrees: Double)
    extends Shape with Unary_Shape_Op {

  lazy val bounding_box =
    shape.bounding_box.skew_horiz(degrees)

  def replace(replacement_shape: Shape) =
    copy(shape = replacement_shape)
}


object Skewed_Vert_Shape extends Skewed_Vert_Transformable[Shape] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Skewed_Vert_Shape]
}

final case class Skewed_Vert_Shape(shape: Shape, degrees: Double)
    extends Shape with Unary_Shape_Op {

  lazy val bounding_box =
    shape.bounding_box.skew_vert(degrees)

  def replace(replacement_shape: Shape) =
    copy(shape = replacement_shape)
}


final case class Composite_Shape(under: Shape, over: Shape)
    extends Shape with Binary_Shape_Op {

  lazy val bounding_box =
    under.bounding_box.combo(over.bounding_box)

  def left  = under
  def right = over

  def replace(replacement_left: Shape,
              replacement_right: Shape) =
    copy(under = replacement_left, over = replacement_right)
}

final case class Clipped_Shape(clipped: Shape, clipping: Shape, 
                               rule: Clip_Rule)
    extends Shape with Binary_Shape_Op {

  lazy val bounding_box =
    clipped.bounding_box.clip_by(clipping.bounding_box)

  def left  = clipped
  def right = clipping

  def replace(replacement_left: Shape,
              replacement_right: Shape) =
    copy(clipped = replacement_left, clipping = replacement_right)
}

final case class Masked_Shape(masked: Shape, mask: Shape)
    extends Shape with Binary_Shape_Op {

  lazy val bounding_box =
    masked.bounding_box.mask_by(mask.bounding_box)

  def left  = masked
  def right = mask

  override
  def best_bounding_shape: Simple_Shape =
    masked.best_bounding_shape

  def replace(replacement_left: Shape,
              replacement_right: Shape) =
    copy(masked = replacement_left, mask = replacement_right)
}

final case class Inked_Shape(shape: Shape, pen: Pen)
    extends Shape with Unary_Shape_Op {

  lazy val bounding_box =
    shape.bounding_box.using(pen)

  override
  def best_bounding_shape: Simple_Shape =
    shape.best_bounding_shape

  def replace(replacement_shape: Shape) =
    copy(shape = replacement_shape)
}

final case class Non_Opaque_Shape(shape: Shape, opacity: Double)
    extends Shape with Unary_Shape_Op {

  lazy val bounding_box =
    shape.bounding_box.exhibit(Opacity_Effect(opacity))

  override
  def best_bounding_shape: Simple_Shape =
    shape.best_bounding_shape

  def replace(replacement_shape: Shape) =
    copy(shape = replacement_shape)
}

final case class Filtered_Shape(shape: Shape, filter: Filter)
    extends Shape with Unary_Shape_Op {

  lazy val bounding_box =
    shape.bounding_box.exhibit(Filter_Effect(filter))

  override
  def best_bounding_shape: Simple_Shape =
    shape.best_bounding_shape

  def replace(replacement_shape: Shape) =
    copy(shape = replacement_shape)
}

final case class Attributed_Shape(shape: Shape,
                                  attribution: Attribution)
    extends Shape with Unary_Shape_Op {

  lazy val bounding_box =
    shape.bounding_box.as(attribution)

  override
  def best_bounding_shape: Simple_Shape =
    shape.best_bounding_shape

  def replace(replacement_shape: Shape) =
    copy(shape = replacement_shape)
}


object Dims {

  def unapply(o: Dims) =
    Some(o.width, o.height)

  def min_containing(pt1: Point, pt2: Point): Dims = {
    val (min_x, max_x) = More_Math.min_max(pt1.x, pt2.x)
    val (min_y, max_y) = More_Math.min_max(pt1.y, pt2.y)
    val (width, height) = (if (max_x == min_x) 0.001 else max_x - min_x,
                           if (max_y == min_y) 0.001 else max_y - min_y)
    val ctr_pt: Point = (max_x - width/2, max_y - height/2)
    Origin_Dims(width, height) move (ctr_pt.x, ctr_pt.y)
  }
}


sealed abstract class Dims
    extends Transforming[Dims]
       with Placeable[Dims]
       with Presentable_Shape[Dims]
       with Bounding_Boxed with Rectangular {

  def bounding_box =
    this

  def as_shape: Simple_Shape // derived class of `Shape`

  type Translated_T          = Translated_Dims
  protected val Translated   = Translated_Dims

  type Scaled_T              = Scaled_Dims
  protected val Scaled       = Scaled_Dims

  type Rotated_T             = Rotated_Dims
  protected val Rotated      = Rotated_Dims

  type Reflected_T           = Reflected_Dims
  protected val Reflected    = Reflected_Dims

  type Skewed_Horiz_T        = Skewed_Horiz_Dims
  protected val Skewed_Horiz = Skewed_Horiz_Dims

  type Skewed_Vert_T         = Skewed_Vert_Dims
  protected val Skewed_Vert  = Skewed_Vert_Dims


  protected def create_composite_shape(other: Dims): Dims = {
    val (Dims(w1, h1), Dims(w2, h2)) = (this, other)
    val bb1_ctr = this.center_pt
    val bb2_ctr = other.center_pt
    val (half_w1, half_h1) = (w1/2, h1/2)
    val (half_w2, half_h2) = (w2/2, h2/2)
    val (bb1_min_x, bb1_max_x, bb1_min_y, bb1_max_y) =
      (bb1_ctr.x - half_w1, bb1_ctr.x + half_w1,
       bb1_ctr.y - half_h1, bb1_ctr.y + half_h1)
    val (bb2_min_x, bb2_max_x, bb2_min_y, bb2_max_y) =
      (bb2_ctr.x - half_w2, bb2_ctr.x + half_w2,
       bb2_ctr.y - half_h2, bb2_ctr.y + half_h2)
    val (min_x, max_x, min_y, max_y) =
      (math.min(bb1_min_x, bb2_min_x), math.max(bb1_max_x, bb2_max_x),
       math.min(bb1_min_y, bb2_min_y), math.max(bb1_max_y, bb2_max_y))
    Dims.min_containing((min_x, min_y), (max_x, max_y))
  }

  protected def create_clipped_shape(clipping: Dims, rule: Clip_Rule): Dims =
    // since `clipping` delimits what is shown of `this`, return that instead
    clipping

  protected def create_masked_shape(mask: Dims): Dims =
    // since `mask` delimits what is shown of `this`, return that instead
    mask

  protected def create_inked_shape(pen: Pen): Dims = {
    // do not apply ink, merely return adjusted for size

    //!!!!this won't work for text, since stroke doesn't surround the figure!!!
    //!!!!!it may also fail when there is a nested pen has already enlarged the bounding box: containing pens would only change the color, not add more size!!!

    pen.width match {
      case Some(len) =>
        this.scale((width + len) / width, (height + len) / height)
      case None =>
        this
    }
  }

  protected def create_effected_shape(effect: Effect): Dims =
    // since effects alter the underlying shape in a (complicated) way that
    // always leaves its dimensions intact, return `this` unchanged
    this

  protected def create_attributed_shape(attribution: Attribution): Dims =
    // attribution has no effect on bounding box
    this
}

final case class Origin_Dims(width: Double, height: Double)
    extends Dims {

  override
  def center_pt = (0.0, 0.0)

  def as_shape: Simple_Shape = Rectangle(width, height)
}

object Translated_Dims
    extends Translated_Transformable[Dims] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Translated_Dims]
}

final case class Translated_Dims(rect: Dims, x_dist: Double, y_dist: Double)
    extends Dims {

  lazy val width  = rect.width
  lazy val height = rect.height

  override
  lazy val center_pt = rect.center_pt -+ (x_dist, y_dist)

  def as_shape: Simple_Shape = rect.as_shape.move(x_dist, y_dist)
}

object Scaled_Dims
    extends Scaled_Transformable[Dims] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Scaled_Dims]
}

final case class Scaled_Dims(rect: Dims, x_scaling: Double, y_scaling: Double)
    extends Dims {

  lazy val width  = rect.width  * x_scaling
  lazy val height = rect.height * y_scaling

  override
  lazy val center_pt = rect.center_pt -* (x_scaling, y_scaling)

  def as_shape: Simple_Shape = rect.as_shape.scale(x_scaling, y_scaling)
}


trait Dims_Displacement {

  protected def calc_displaced_rect(rect: Dims): Dims = {
    val rect_ctr = rect.center_pt
    val (rect_half_w, rect_half_h) = (rect.width/2, rect.height/2)
    val rect_corner_pts: List[Point] =
      List((rect_ctr.x - rect_half_w, rect_ctr.y - rect_half_h),
           (rect_ctr.x - rect_half_w, rect_ctr.y + rect_half_h),
           (rect_ctr.x + rect_half_w, rect_ctr.y - rect_half_h),
           (rect_ctr.x + rect_half_w, rect_ctr.y + rect_half_h))
    val displaced_corner_pts = rect_corner_pts map calc_displacement _
    val (min_x, max_x) = (displaced_corner_pts map { _.x } min,
                          displaced_corner_pts map { _.x } max)
    val (min_y, max_y) = (displaced_corner_pts map { _.y } min,
                          displaced_corner_pts map { _.y } max)
    Dims.min_containing((min_x, min_y), (max_x, max_y))
  }

  protected def calc_displacement(pt: Point): Point
}


object Rotated_Dims
    extends Rotated_Transformable[Dims] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Rotated_Dims]

//????for some reason the following is not equiv to the above:
//  protected val isInstanceOfCompanion =
//    (_: Any).isInstanceOf[Rotated_Dims]
//
//[ERROR] .../tie/tie/src/main/scala/k_k_/graphics/tie/shapes.scala:1328: error: object creation impossible, since method isInstanceOfCompanion in trait Rotated_Transformable of type (x: Any)Boolean is not defined
//[INFO] object Rotated_Dims
//[INFO]        ^
}

final case class Rotated_Dims(rect: Dims, degrees: Double,
                              x_pivot: Double, y_pivot: Double)
    extends Dims with Dims_Displacement {

  lazy val width  = equiv_bounding_box.width
  lazy val height = equiv_bounding_box.height

  override
  def bounding_box = equiv_bounding_box

  override
  def center_pt = equiv_bounding_box.center_pt

  def as_shape: Simple_Shape = equiv_bounding_box.as_shape


  protected def calc_displacement(pt: Point): Point =
    pt.rotate(degrees, x_pivot, y_pivot)

  private lazy val equiv_bounding_box = calc_displaced_rect(rect)
}

object Reflected_Dims
    extends Reflected_Transformable[Dims] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Reflected_Dims]
}

final case class Reflected_Dims(rect: Dims, degrees: Double,
                                x_pivot: Double, y_pivot: Double)
    extends Dims with Dims_Displacement {

  lazy val width  = equiv_bounding_box.width
  lazy val height = equiv_bounding_box.height

  override
  def bounding_box = equiv_bounding_box

  override
  def center_pt = equiv_bounding_box.center_pt

  def as_shape: Simple_Shape = equiv_bounding_box.as_shape


  protected def calc_displacement(pt: Point): Point =
    pt.reflect(degrees, x_pivot, y_pivot)

  private lazy val equiv_bounding_box = calc_displaced_rect(rect)
}

object Skewed_Horiz_Dims
    extends Skewed_Horiz_Transformable[Dims] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Skewed_Horiz_Dims]
}

final case class Skewed_Horiz_Dims(rect: Dims, degrees: Double)
    extends Dims with Dims_Displacement {

  lazy val width  = equiv_bounding_box.width
  lazy val height = equiv_bounding_box.height

  override
  def bounding_box = equiv_bounding_box

  override
  def center_pt = equiv_bounding_box.center_pt

  def as_shape: Simple_Shape = equiv_bounding_box.as_shape


  protected def calc_displacement(pt: Point): Point =
    pt.skew_horiz(degrees)

  private lazy val equiv_bounding_box = calc_displaced_rect(rect)
}

object Skewed_Vert_Dims
    extends Skewed_Vert_Transformable[Dims] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Skewed_Vert_Dims]
}

final case class Skewed_Vert_Dims(rect: Dims, degrees: Double)
    extends Dims with Dims_Displacement {

  lazy val width  = equiv_bounding_box.width
  lazy val height = equiv_bounding_box.height

  override
  def bounding_box = equiv_bounding_box

  override
  def center_pt = equiv_bounding_box.center_pt

  def as_shape: Simple_Shape = equiv_bounding_box.as_shape


  protected def calc_displacement(pt: Point): Point =
    pt.skew_vert(degrees)

  private lazy val equiv_bounding_box = calc_displaced_rect(rect)
}

}


package object shapes {

import k_k_.graphics.tie.effects.Effect
import k_k_.graphics.tie.ink.Pen
import k_k_.graphics.tie.shapes.path.Path


object Identity_Shape {

  def unapply(shape: Shape): Boolean =
    shape eq Null_PF_Shape
}

// NOTE: define as `val` in package object rather than as object, to avoid need
// to always down-qualify to generalized type; e.g. to simplify the following:
//    ((Null_Shape: Shape) /: shapes_seq) ( _ -& _ )
//   to:
//    (Null_Shape /: shapes_seq) ( _ -& _ )
// case object Null_Shape extends Invis_Rectangle(0.0001, 0.0001) {

val Null_PF_Shape: Simple_Shape =
    new Simple_Shape with Nullary_Shape_Op {

  val (alleged_width, alleged_height) = (0.0001, 0.0001)

  def bounding_box =
    Origin_Dims(alleged_width, alleged_height)

  def as_path =
    Path.from(0, 0).
         close

  override def toString =
    "Null_Shape"


  // there is only one Null_Shape, which is constant under every transform...

  override
  def move(x_dist: Double, y_dist: Double): Simple_Shape =
    this

  override
  def scale(x_scaling: Double, y_scaling: Double): Simple_Shape =
    this

  override
  def rotate(degrees: Double, about_x: Double, about_y: Double): Simple_Shape =
    this

  override
  def reflect(degrees: Double, about_x: Double, about_y: Double): Simple_Shape =
    this

  override
  def skew_horiz(degrees: Double): Shape =
    this

  override
  def skew_vert(degrees: Double): Shape =
    this


  // ...results in an identity when combined with another shape

  override
  protected def create_composite_shape(other: Shape): Shape =
    other

  // ...clipped and masked with no effect

  override
  protected def create_clipped_shape(clipping: Shape, rule: Clip_Rule):
      Shape =
    this

  override
  protected def create_masked_shape(mask: Shape): Shape =
    this

  // ...unchanged under the pen

  override
  protected def create_inked_shape(pen: Pen): Shape =
    this

  // ...impervious to every Effect

  override
  protected def create_effected_shape(effect: Effect): Shape =
    this

  // ...and likewise unattributable

  override
  protected def create_attributed_shape(attribution: Attribution):
      Shape =
    this
}

val Null_Shape: Shape = Null_PF_Shape

}


/*
    (ugly, messy!) snippet of eariler version of Invis_Rectangle.cloak_rect(),
    prior to definition of Shape_Op:

        case Scaled_Shape(inner, x_scaling, y_scaling) =>
          Scaled_Shape(cloak_rect(inner), x_scaling, y_scaling)
        case Rotated_Shape(inner, degrees, x_pivot, y_pivot) =>
          Rotated_Shape(cloak_rect(inner), degrees, x_pivot, y_pivot)
        case Composite_Shape(under, over) =>
          Composite_Shape(cloak_rect(under), cloak_rect(over))
          //????????
        case Clipped_Shape(clipped, clipping, rule) =>
          Clipped_Shape(cloak_rect(clipped), cloak_rect(clipping), rule)
        case Masked_Shape(masked, mask) =>
          Masked_Shape(cloak_rect(masked), cloak_rect(mask))
        case Inked_Shape(inner, pen) =>
          Inked_Shape(cloak_rect(inner), pen)
        case x @ ( _ : Segment | _ : Simple_Shape | _ : Free_Form |
                   _ : Writing) =>
          shape // ouch!--no Rectangle here, return unchanged
*/
