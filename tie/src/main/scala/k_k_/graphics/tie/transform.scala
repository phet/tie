/*
   file: k_k_/graphics/tie/transform.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie.transform

import k_k_.graphics.tie.shapes.Point


trait Transformable[T] { self: T =>

  def move(x_dist: Double, y_dist: Double): T

  def move(pt_offset: Point): T =
    move(pt_offset.x, pt_offset.y)

  def move(dist: Double): T =
    move(dist, dist)

  def -+(x_dist: Double, y_dist: Double): T =
    move(x_dist, y_dist)

  def -+(pt_offset: Point): T =
    move(pt_offset.x, pt_offset.y)

  def -+(dist: Double): T =
    move(dist, dist)


  def scale(x_scaling: Double, y_scaling: Double): T

  def scale(scaling: Double): T =
    scale(scaling, scaling)

  def -*(x_scaling: Double, y_scaling: Double): T =
    scale(x_scaling, y_scaling)

  def -*(scaling: Double): T =
    scale(scaling, scaling)


  // NOTE: angles measured clockwise
  def rotate(degrees: Double, about_x: Double, about_y: Double): T

  def rotate(degrees: Double, about_pt: Point = Point(0, 0)): T =
    rotate(degrees, about_pt.x, about_pt.y)

  def -%(degrees: Double, about_x: Double, about_y: Double): T =
    rotate(degrees, about_x, about_y)

  def -%(degrees: Double, about_pt: Point = Point(0, 0)): T =
    rotate(degrees, about_pt.x, about_pt.y)


  // NOTE: angles measured clockwise
  def reflect(degrees: Double, about_x: Double, about_y: Double): T

  def reflect(degrees: Double, about_pt: Point = Point(0, 0)): T =
    reflect(degrees, about_pt.x, about_pt.y)

  def -|-(degrees: Double, about_x: Double, about_y: Double): T =
    reflect(degrees, about_x, about_y)

  def -|-(degrees: Double, about_pt: Point = Point(0, 0)): T =
    reflect(degrees, about_pt.x, about_pt.y)


  def skew_horiz(degrees: Double): T

  def -/-(degrees: Double): T =
    skew_horiz(degrees)


  def skew_vert(degrees: Double): T

  def -/|(degrees: Double): T =
    skew_vert(degrees)
}


trait Placeable[T] { self: T with Transformable[T] =>

  def to(dest_pt: Point): T =
    move(dest_pt -+ -center_pt)

  def to(x_coord: Double, y_coord: Double): T =
    to(Point(x_coord, y_coord))

  def -@(dest_pt: Point): T =
    to(dest_pt)

  def -@(x_coord: Double, y_coord: Double): T =
    to(Point(x_coord, y_coord))


  def center_pt: Point
}


trait Translated_Transformable[Transforming_T <: Transforming[Transforming_T]] {

  protected type Translated_T = Transforming_T#Translated_T

  def unapply(x: Translated_T): Option[(Transforming_T, Double, Double)]

  def unapply[T >: Transforming_T](x: T):
      Option[(Transforming_T, Double, Double)] =
    if (isInstanceOfCompanion(x))
      unapply(x.asInstanceOf[Translated_T])
    else
      None

  def apply(tformable: Transforming_T, x_dist: Double, y_dist: Double):
      Transforming_T


  protected def isInstanceOfCompanion(x: Any): Boolean
}

trait Scaled_Transformable[Transforming_T <: Transforming[Transforming_T]] {

  protected type Scaled_T = Transforming_T#Scaled_T

  def unapply(x: Scaled_T): Option[(Transforming_T, Double, Double)]

  def unapply[T >: Transforming_T](x: T):
      Option[(Transforming_T, Double, Double)] =
    if (isInstanceOfCompanion(x))
      unapply(x.asInstanceOf[Scaled_T])
    else
      None

  def apply(tformable: Transforming_T, x_scaling: Double, y_scaling: Double):
      Transforming_T


  protected def isInstanceOfCompanion(x: Any): Boolean
}

trait Rotated_Transformable[Transforming_T <: Transforming[Transforming_T]] {

  protected type Rotated_T = Transforming_T#Rotated_T

  def unapply(x: Rotated_T): Option[(Transforming_T, Double, Double, Double)]

  def unapply[T >: Transforming_T](x: T):
      Option[(Transforming_T, Double, Double, Double)] =
    if (isInstanceOfCompanion(x))
      unapply(x.asInstanceOf[Rotated_T])
    else
      None

  def apply(tformable: Transforming_T, degrees: Double,
            about_x: Double, about_y: Double):
      Transforming_T


  protected def isInstanceOfCompanion(x: Any): Boolean
}

trait Reflected_Transformable[Transforming_T <: Transforming[Transforming_T]] {

  protected type Reflected_T = Transforming_T#Reflected_T

  def unapply(x: Reflected_T): Option[(Transforming_T, Double, Double, Double)]

  def unapply[T >: Transforming_T](x: T):
      Option[(Transforming_T, Double, Double, Double)] =
    if (isInstanceOfCompanion(x))
      unapply(x.asInstanceOf[Reflected_T])
    else
      None

  def apply(tformable: Transforming_T, degrees: Double,
            about_x: Double, about_y: Double):
      Transforming_T


  protected def isInstanceOfCompanion(x: Any): Boolean
}

trait Skewed_Horiz_Transformable[Transforming_T <:
                                   Transforming[Transforming_T]] {

  protected type Skewed_Horiz_T = Transforming_T#Skewed_Horiz_T

  def unapply(x: Skewed_Horiz_T): Option[(Transforming_T, Double)]

  def unapply[T >: Transforming_T](x: T): Option[(Transforming_T, Double)] =
    if (isInstanceOfCompanion(x))
      unapply(x.asInstanceOf[Skewed_Horiz_T])
    else
      None

  def apply(tformable: Transforming_T, degrees: Double): Transforming_T


  protected def isInstanceOfCompanion(x: Any): Boolean
}

trait Skewed_Vert_Transformable[Transforming_T <:
                                  Transforming[Transforming_T]] {

  protected type Skewed_Vert_T = Transforming_T#Skewed_Vert_T

  def unapply(x: Skewed_Vert_T): Option[(Transforming_T, Double)]

  def unapply[T >: Transforming_T](x: T): Option[(Transforming_T, Double)] =
    if (isInstanceOfCompanion(x))
      unapply(x.asInstanceOf[Skewed_Vert_T])
    else
      None

  def apply(tformable: Transforming_T, degrees: Double): Transforming_T


  protected def isInstanceOfCompanion(x: Any): Boolean
}


trait Transforming[T <: Transforming[T]] extends Transformable[T] { self: T =>

  type Translated_T <: T

  protected val Translated: Translated_Transformable[T]

  def move(x_dist: Double, y_dist: Double): T = {
    this match {
      case Translated(inner, existing_x_dist, existing_y_dist) =>
        val combined_x_dist = x_dist + existing_x_dist
        val combined_y_dist = y_dist + existing_y_dist
        if (combined_x_dist == 0.0 && combined_y_dist == 0.0)
          inner // successive ops cancel one another
        else
          // adjust 'previous' op by combining with sucessor
          Translated(inner, combined_x_dist, combined_y_dist)
      case _ =>
        Translated(this, x_dist, y_dist)
    }
  }


  type Scaled_T <: T

  protected val Scaled: Scaled_Transformable[T]

  def scale(x_scaling: Double, y_scaling: Double): T = {
    this match {
      case Scaled(inner, existing_x_scaling, existing_y_scaling) =>
        val combined_x_scaling = x_scaling * existing_x_scaling
        val combined_y_scaling = y_scaling * existing_y_scaling
        if (combined_x_scaling == 1.0 && combined_y_scaling == 1.0)
          inner // successive ops cancel one another
        else
          // adjust 'previous' op by combining with sucessor
          Scaled(inner, combined_x_scaling, combined_y_scaling)
      case _ =>
        Scaled(this, x_scaling, y_scaling)
    }
  }


  type Rotated_T <: T

  protected val Rotated: Rotated_Transformable[T]

  def rotate(degrees: Double, about_x: Double, about_y: Double): T = {
    (about_x, about_y, this) match {
      // NOTE: simplify only when both rotate about (0,0)--else too complicated
      case (0, 0, Rotated(inner, existing_degrees, 0, 0)) =>
        val combined_degrees = degrees + existing_degrees
        if (combined_degrees % 360 == 0.0)
          inner // successive ops cancel one another
        else
          // adjust 'previous' op by combining with sucessor
          Rotated(inner, combined_degrees, 0, 0)
      case _ =>
        Rotated(this, degrees, about_x, about_y)
    }
  }


  type Reflected_T <: T

  protected val Reflected: Reflected_Transformable[T]

  def reflect(degrees: Double, about_x: Double, about_y: Double): T = {
    (about_x, about_y, this) match {
      // NOTE: simplify only when both reflect about (0,0)--else too complicated
      case (0, 0, Reflected(inner, existing_degrees, 0, 0))
             if ((degrees % 360) == (existing_degrees % 360)) =>
          inner // successive ops cancel one another
      case _ =>
        Reflected(this, degrees, about_x, about_y)
    }
  }


  type Skewed_Horiz_T <: T

  protected val Skewed_Horiz: Skewed_Horiz_Transformable[T]

  def skew_horiz(degrees: Double): T = {
    this match {
      case Skewed_Horiz(inner, existing_degrees) =>
        val combined_degrees = degrees + existing_degrees
        if (combined_degrees == 0)
          inner // successive ops cancel one another
        else
          // adjust 'previous' op by combining with sucessor
          Skewed_Horiz(inner, combined_degrees)
      case _ =>
        Skewed_Horiz(this, degrees)
    }
  }


  type Skewed_Vert_T <: T

  protected val Skewed_Vert: Skewed_Vert_Transformable[T]

  def skew_vert(degrees: Double): T = {
    this match {
      case Skewed_Vert(inner, existing_degrees) =>
        val combined_degrees = degrees + existing_degrees
        if (combined_degrees == 0)
          inner // successive ops cancel one another
        else
          // adjust 'previous' op by combining with sucessor
          Skewed_Vert(inner, combined_degrees)
      case _ =>
        Skewed_Vert(this, degrees)
    }
  }
}


/*
   writing the above code was a painful saga, which left the Our Gentle
   Programmer beaten and bruised by the scala compiler; the following notes
   aim to illustrate the tale, and record attempted solutions which led only
   to a world of pain:

   update II: it's even possible to safely factor the whole unapply
   overload impl into this class, so long as the
   isInstanceOf[Translated_T] is safely implemented by the extending
   object.  'safely' is the key concept, since this is not checked by the
   compiler: one need be damn sure that for any value x, for which
   isInstanceOfCompanion(x) is true, that x.asInstanceOf[Translated_T]
   will not blow up!!!!!

   this is the final, chosen solution:

trait Translated_Transformable[Transformable_T <:
                                 Transformable[Transformable_T]] {

  protected type Translated_T = Transformable_T#Translated_T

  def unapply(x: Translated_T): Option[(Transformable_T, Double, Double)]

  def unapply[T >: Transformable_T](x: T):
      Option[(Transformable_T, Double, Double)] =
    if (isInstanceOfCompanion(x))
      unapply(x.asInstanceOf[Translated_T])
    else
      None

  def apply(tformable: Transformable_T, x_dist: Double, y_dist: Double):
      Transformable_T


  protected def isInstanceOfCompanion(x: Any): Boolean
}

...

trait Transformable[T <: Transformable[T]] { self: T =>

  type Translated_T <: T

  protected val Translated: Translated_Transformable[T]

  ...
}

...

sealed abstract class Dims
    extends Transformable[Dims]
       with Presentable_Shape[Dims]
       with Bounding_Boxed with Rectangular {

  type Translated_T        = Translated_Dims
  protected val Translated = Translated_Dims

  ...
}

...

object Translated_Dims
    extends Translated_Transformable[Dims] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Translated_Dims]
}

final case class Translated_Dims(rect: Dims, x_dist: Double, y_dist: Double)
    extends Dims {
  ...
}

   ------------------------------------------------------------------------
NOTE: (in terms of the chosen solution above) attempting to replace:

trait Translated_Transformable[Transformable_T <:
                                 Transformable[Transformable_T]] {
  ...
}

...

trait Transformable[T <: Transformable[T]] { self: T =>
  ...
}


with the following:


trait Translated_Transformable[Transforming_T <: Transforming[_]] {
  ...
}

...

trait Transformable[T] { self: T =>

  ...
}

results in the errors:

[ERROR] .../tie/tie/src/main/scala/k_k_/graphics/tie/transformable.scala:173: error: type arguments [T] do not conform to trait Translated_Transformable's type parameter bounds [Transforming_T <: k_k_.graphics.tie.transformable.Transforming[_]]
[INFO]   protected val Translated: Translated_Transformable[T]
[INFO]                             ^
[ERROR] .../tie/tie/src/main/scala/k_k_/graphics/tie/transformable.scala:193: error: type arguments [T] do not conform to trait Scaled_Transformable's type parameter bounds [Transforming_T <: k_k_.graphics.tie.transformable.Transforming[_]]
[INFO]   protected val Scaled: Scaled_Transformable[T]
[INFO]                         ^
[ERROR] .../tie/tie/src/main/scala/k_k_/graphics/tie/transformable.scala:213: error: type arguments [T] do not conform to trait Rotated_Transformable's type parameter bounds [Transforming_T <: k_k_.graphics.tie.transformable.Transforming[_]]
[INFO]   protected val Rotated: Rotated_Transformable[T]
[INFO]                          ^
[ERROR] .../tie/tie/src/main/scala/k_k_/graphics/tie/transformable.scala:233: error: type arguments [T] do not conform to trait Skewed_Horiz_Transformable's type parameter bounds [Transforming_T <: k_k_.graphics.tie.transformable.Transforming[_]]
[INFO]   protected val Skewed_Horiz: Skewed_Horiz_Transformable[T]
[INFO]                               ^
[ERROR] .../tie/tie/src/main/scala/k_k_/graphics/tie/transformable.scala:252: error: type arguments [T] do not conform to trait Skewed_Vert_Transformable's type parameter bounds [Transforming_T <: k_k_.graphics.tie.transformable.Transforming[_]]
[INFO]   protected val Skewed_Vert: Skewed_Vert_Transformable[T]
[INFO]                              ^

   ------------------------------------------------------------------------
this is what happens when x.asInstanceOf[Translated_T] is used directly:

[WARNING] .../tie/tie/src/main/scala/k_k_/graphics/tie/transformable.scala:18: warning: abstract type Transformable_T#Translated_T in type Translated_Transformable.this.Translated_T is unchecked since it is eliminated by erasure
[INFO]     if (x.isInstanceOf[Translated_T])
[INFO]                       ^
[WARNING] one warning found

trait Translated_Transformable[Transformable_T <:
                                 Transformable[Transformable_T]] {

  type Translated_T = Transformable_T#Translated_T

  def unapply(x: Translated_T): Option[(Transformable_T, Double, Double)]

  def unapply[T >: Transformable_T](x: T):
      Option[(Transformable_T, Double, Double)] =
    if (x.isInstanceOf[Translated_T])
      unapply(x.asInstanceOf[Translated_T])
    else
      None

  def apply(tformable: Transformable_T, x_dist: Double, y_dist: Double):
      Transformable_T
}

...

trait Transformable[T <: Transformable[T]] { self: T =>

  type Translated_T <: T

  protected val Translated: Translated_Transformable[T]

  ...
}

...

object Translated_Dims
    extends Translated_Transformable[Dims]


   ------------------------------------------------------------------------

   update: (fortunately!) the determination below is not strictly true:
   it is possible for the object extending this class to have a
   companion class which is a case class.  I had been implicitly
   assuming the following:


trait Translated_Transformable[Transformable_T <:
                                 Transformable[Transformable_T]] {

  def unapply(x Transformable_T): Option[(Transformable_T, Double, Double)]

  def apply(tformable: Transformable_T, x_dist: Double, y_dist: Double):
      Transformable_T
}

...

object Translated_Dims
    extends Translated_Transformable[Dims] {

  def unapply(ortho_rect: Dims) =
    ortho_rect match {
       case trans: Translated_Dims =>
         Some((trans.rect, trans.x_dist, trans.y_dist))
       case _ => None
    }

  def apply(rect: Dims, x_dist: Double, y_dist: Double) =
    new Translated_Dims(rect, x_dist, y_dist)
}

final class Translated_Dims(val rect: Dims,
                            val x_dist: Double, val y_dist: Double)
    extends Dims {
  ...
}


but it is actually possible to limit unapply to Transformable_T or a
super type, which then causes no overload resolution error with the
definition of unapply generated by the case class.  thus, the
companion class may remain a case class with a nice unapply method
that may be used directly, and its companion object must only define
another unapply which delegates to the case class version when the
value which is statically-typed as a Transformable_T or a super type
is actually dynamically found to be the correct sub type of
Transformable_T!


trait Translated_Transformable[Transformable_T <:
                                 Transformable[Transformable_T]] {

  def unapply[T >: Transformable_T](x: T):
      Option[(Transformable_T, Double, Double)]

  def apply(tformable: Transformable_T, x_dist: Double, y_dist: Double):
      Transformable_T
}

...

object Translated_Dims
    extends Translated_Transformable[Dims] {

  def unapply[T >: Dims](ortho_rect: T): Option[(Dims, Double, Double)] =
    if (ortho_rect.isInstanceOf[Translated_Dims])
      unapply(ortho_rect.asInstanceOf[Translated_Dims])
    else
      None
}

final case class Translated_Dims(rect: Dims, x_dist: Double, y_dist: Double)
    extends Dims {
  ...
}


   ------------------------------------------------------------------------

  the approach above requires extra work in that the object extending
  Translated_Transformable must implement unapply (and apply, if
  wanted), since making its companion class a case class is not an
  possiblity, because doing so leaves the compiler unabile to resolve
  'overloaded unapply' (since the definition above uses the base type
  as the unapply param, whereas the case class uses the exact type of
  itself).  attempting to define the signature of unapply to have the
  exact type does not seem possible due to erasure, which then results
  in runtime failure due to 'unchecked' use (see the study below in
  the comments).


(the code below) causes:
[WARNING] .../tie/tie/src/main/scala/k_k_/graphics/tie/transformable.scala:88: warning: abstract type T#Translated_T in type pattern T#Translated_T is unchecked since it is eliminated by erasure
[INFO]       case Translated(inner, existing_x_dist, existing_y_dist) =>
[INFO]                      ^
[WARNING] one warning found
...
Tests in error:
  test(k_k_.test.graphics.tie.Svg_Sierpinski_Triangle_Test)
  test_Rendering(k_k_.test.graphics.tie.Svg_Renderer_Test)
  test(k_k_.test.graphics.tie.ink.Svg_Color_Rings_v_Radial_Gradient_Test)
  test(k_k_.test.graphics.tie.ink.Svg_Venn_Diagram_Opacity_Test)
  test(k_k_.test.graphics.tie.ink.Svg_Color_Stripes_v_Linear_Gradient_Test)

Tests run: 8, Failures: 0, Errors: 5, Skipped: 0
...
of:
-------------------------------------------------------------------------------
Test set: k_k_.test.graphics.tie.ink.Svg_Venn_Diagram_Opacity_Test
-------------------------------------------------------------------------------
Tests run: 1, Failures: 0, Errors: 1, Skipped: 0, Time elapsed: 0.047 sec <<< FAILURE!
test(k_k_.test.graphics.tie.ink.Svg_Venn_Diagram_Opacity_Test)  Time elapsed: 0.031 sec  <<< ERROR!
java.lang.ClassCastException: k_k_.graphics.tie.shapes.Origin_Dims cannot be cast to k_k_.graphics.tie.shapes.Translated_Dims
        at k_k_.graphics.tie.shapes.Translated_Dims$.unapply(shapes.scala:1286)
        at k_k_.graphics.tie.transformable.Transformable$class.move(transformable.scala:48)
        at k_k_.graphics.tie.shapes.Dims.move(shapes.scala:1175)
        at k_k_.graphics.tie.shapes.Translated_Non_Pre_Formulated_Shape.bounding_box(shapes.scala:1055)
        at k_k_.graphics.tie.shapes.Composite_Shape.bounding_box(shapes.scala:1141)
        at k_k_.graphics.tie.shapes.Composite_Shape.bounding_box(shapes.scala:1141)


trait Translated_Transformable[Transformable_T <: Transformable[Transformable_T]] {

  type Translated_T = Transformable_T#Translated_T

  def unapply(x: Translated_T): Option[(Transformable_T, Double, Double)]

...

trait Transformable[T <: Transformable[T]] { self: T =>

  type Translated_T <: T

  protected val Translated: Translated_Transformable[T]

  def move(x_dist: Double, y_dist: Double): T = {
    this match {
      case Translated(inner, existing_x_dist, existing_y_dist) =>

...

sealed abstract class Dims
    extends Transformable[Dims]
       with Presentable_Shape[Dims]
       with Bounding_Boxed with Rectangular {
  ...

  type Translated_T        = Translated_Dims
  protected val Translated = Translated_Dims

...

object Translated_Dims
    extends Translated_Transformable[Dims]

final case class Translated_Dims(rect: Dims, x_dist: Double, y_dist: Double)
    extends Dims {
  ...
}
*/

/*
(the code below) causes:
[ERROR] .../tie/tie/src/main/scala/k_k_/graphics/tie/shapes.scala:1304: error: cannot resolve overloaded unapply
[INFO] final case class Translated_Dims(rect: Dims,
[INFO]                  ^
[WARNING] .../tie/tie/src/main/scala/k_k_/graphics/tie/transformable.scala:14: warning: abstract type Transformable_T#Translated_T in type Translated_Transformable.this.Translated_T is unchecked since it is eliminated by erasure
[INFO]     if (x.isInstanceOf[Translated_T])
[INFO]                       ^
[WARNING] one warning found
[ERROR] one error found


trait Translated_Transformable[Transformable_T <: Transformable[Transformable_T]] {

  type Translated_T = Transformable_T#Translated_T

  def unapply(x: Translated_T): Option[(Transformable_T, Double, Double)]

  def unapply(x: Transformable_T): Option[(Transformable_T, Double, Double)] =
    if (x.isInstanceOf[Translated_T])
      unapply(x.asInstanceOf[Translated_T])
    else
      None

...

trait Transformable[T <: Transformable[T]] { self: T =>

  type Translated_T <: T

  protected val Translated: Translated_Transformable[T]

  def move(x_dist: Double, y_dist: Double): T = {
    this match {
      case Translated(inner, existing_x_dist, existing_y_dist) =>

...

sealed abstract class Dims
    extends Transformable[Dims]
       with Presentable_Shape[Dims]
       with Bounding_Boxed with Rectangular {
  ...

  type Translated_T        = Translated_Dims
  protected val Translated = Translated_Dims

...

object Translated_Dims
    extends Translated_Transformable[Dims]

final case class Translated_Dims(rect: Dims, x_dist: Double, y_dist: Double)
    extends Dims {
  ...
}

*/
