/*
   file: k_k_/graphics/tie/transform.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2013 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie.transform

import k_k_.graphics.tie.shapes.Point


trait Transformable[T] { self: T =>

  def move(xDist: Double, yDist: Double): T

  def move(ptOffset: Point): T = move(ptOffset.x, ptOffset.y)

  def move(dist: Double): T = move(dist, dist)

  def -+(xDist: Double, yDist: Double): T = move(xDist, yDist)

  def -+(ptOffset: Point): T = move(ptOffset.x, ptOffset.y)

  def -+(dist: Double): T = move(dist, dist)


  def scale(xScaling: Double, yScaling: Double): T

  def scale(scaling: Double): T = scale(scaling, scaling)

  def -*(xScaling: Double, yScaling: Double): T = scale(xScaling, yScaling)

  def -*(scaling: Double): T = scale(scaling, scaling)


  // NOTE: angles measured clockwise
  def rotate(degrees: Double, aboutX: Double, aboutY: Double): T

  def rotate(degrees: Double, aboutPt: Point = Point(0, 0)): T =
    rotate(degrees, aboutPt.x, aboutPt.y)

  def -%(degrees: Double, aboutX: Double, aboutY: Double): T =
    rotate(degrees, aboutX, aboutY)

  def -%(degrees: Double, aboutPt: Point = Point(0, 0)): T =
    rotate(degrees, aboutPt.x, aboutPt.y)


  // NOTE: angles measured clockwise
  def reflect(degrees: Double, aboutX: Double, aboutY: Double): T

  def reflect(degrees: Double, aboutPt: Point = Point(0, 0)): T =
    reflect(degrees, aboutPt.x, aboutPt.y)

  def -|-(degrees: Double, aboutX: Double, aboutY: Double): T =
    reflect(degrees, aboutX, aboutY)

  def -|-(degrees: Double, aboutPt: Point = Point(0, 0)): T =
    reflect(degrees, aboutPt.x, aboutPt.y)


  def skewHoriz(degrees: Double): T

  def -/-(degrees: Double): T = skewHoriz(degrees)


  def skewVert(degrees: Double): T

  def -/|(degrees: Double): T = skewVert(degrees)
}


trait Placeable[T] { self: T with Transformable[T] =>

  def to(destPt: Point): T = move(destPt -+ -centerPt)

  def to(xCoord: Double, yCoord: Double): T = to(Point(xCoord, yCoord))

  def -@(destPt: Point): T = to(destPt)

  def -@(xCoord: Double, yCoord: Double): T = to(Point(xCoord, yCoord))


  def centerPt: Point
}


trait TranslatedTransformable[TransformingT <: Transforming[TransformingT]] {

  protected type TranslatedT = TransformingT#TranslatedT

  def unapply(x: TranslatedT): Option[(TransformingT, Double, Double)]

  def unapply[T >: TransformingT](x: T):
      Option[(TransformingT, Double, Double)] =
    if (isInstanceOfCompanion(x)) unapply(x.asInstanceOf[TranslatedT])
    else None

  def apply(tformable: TransformingT, xDist: Double, yDist: Double):
      TransformingT


  protected def isInstanceOfCompanion(x: Any): Boolean
}

trait ScaledTransformable[TransformingT <: Transforming[TransformingT]] {

  protected type ScaledT = TransformingT#ScaledT

  def unapply(x: ScaledT): Option[(TransformingT, Double, Double)]

  def unapply[T >: TransformingT](x: T):
      Option[(TransformingT, Double, Double)] =
    if (isInstanceOfCompanion(x))
      unapply(x.asInstanceOf[ScaledT])
    else
      None

  def apply(tformable: TransformingT, xScaling: Double, yScaling: Double):
      TransformingT


  protected def isInstanceOfCompanion(x: Any): Boolean
}

trait RotatedTransformable[TransformingT <: Transforming[TransformingT]] {

  protected type RotatedT = TransformingT#RotatedT

  def unapply(x: RotatedT): Option[(TransformingT, Double, Double, Double)]

  def unapply[T >: TransformingT](x: T):
      Option[(TransformingT, Double, Double, Double)] =
    if (isInstanceOfCompanion(x)) unapply(x.asInstanceOf[RotatedT])
    else None

  def apply(
      tformable: TransformingT,
      degrees: Double,
      aboutX: Double,
      aboutY: Double
    ): TransformingT


  protected def isInstanceOfCompanion(x: Any): Boolean
}

trait ReflectedTransformable[TransformingT <: Transforming[TransformingT]] {

  protected type ReflectedT = TransformingT#ReflectedT

  def unapply(x: ReflectedT): Option[(TransformingT, Double, Double, Double)]

  def unapply[T >: TransformingT](x: T):
      Option[(TransformingT, Double, Double, Double)] =
    if (isInstanceOfCompanion(x)) unapply(x.asInstanceOf[ReflectedT])
    else None

  def apply(
      tformable: TransformingT,
      degrees: Double,
      aboutX: Double,
      aboutY: Double
    ): TransformingT


  protected def isInstanceOfCompanion(x: Any): Boolean
}

trait SkewedHorizTransformable[TransformingT <: Transforming[TransformingT]] {

  protected type SkewedHorizT = TransformingT#SkewedHorizT

  def unapply(x: SkewedHorizT): Option[(TransformingT, Double)]

  def unapply[T >: TransformingT](x: T): Option[(TransformingT, Double)] =
    if (isInstanceOfCompanion(x)) unapply(x.asInstanceOf[SkewedHorizT])
    else None

  def apply(tformable: TransformingT, degrees: Double): TransformingT


  protected def isInstanceOfCompanion(x: Any): Boolean
}

trait SkewedVertTransformable[TransformingT <: Transforming[TransformingT]] {

  protected type SkewedVertT = TransformingT#SkewedVertT

  def unapply(x: SkewedVertT): Option[(TransformingT, Double)]

  def unapply[T >: TransformingT](x: T): Option[(TransformingT, Double)] =
    if (isInstanceOfCompanion(x)) unapply(x.asInstanceOf[SkewedVertT])
    else None

  def apply(tformable: TransformingT, degrees: Double): TransformingT


  protected def isInstanceOfCompanion(x: Any): Boolean
}


trait Transforming[T <: Transforming[T]] extends Transformable[T] { self: T =>

  type TranslatedT <: T

  protected val Translated: TranslatedTransformable[T]

  def move(xDist: Double, yDist: Double): T = {
    this match {
      case Translated(inner, existingXDist, existingYDist) =>
        val combinedXDist = xDist + existingXDist
        val combinedYDist = yDist + existingYDist
        if (combinedXDist == 0.0 && combinedYDist == 0.0)
          inner // successive ops cancel one another
        else
          // adjust 'previous' op by combining with sucessor
          Translated(inner, combinedXDist, combinedYDist)
      case _ =>
        Translated(this, xDist, yDist)
    }
  }


  type ScaledT <: T

  protected val Scaled: ScaledTransformable[T]

  def scale(xScaling: Double, yScaling: Double): T = {
    this match {
      case Scaled(inner, existingXScaling, existingYScaling) =>
        val combinedXScaling = xScaling * existingXScaling
        val combinedYScaling = yScaling * existingYScaling
        if (combinedXScaling == 1.0 && combinedYScaling == 1.0)
          inner // successive ops cancel one another
        else
          // adjust 'previous' op by combining with sucessor
          Scaled(inner, combinedXScaling, combinedYScaling)
      case _ =>
        Scaled(this, xScaling, yScaling)
    }
  }


  type RotatedT <: T

  protected val Rotated: RotatedTransformable[T]

  def rotate(degrees: Double, aboutX: Double, aboutY: Double): T = {
    (aboutX, aboutY, this) match {
      // NOTE: simplify only when both rotate about (0,0)--else too complicated
      case (0, 0, Rotated(inner, existingDegrees, 0, 0)) =>
        val combinedDegrees = degrees + existingDegrees
        if (combinedDegrees % 360 == 0.0)
          inner // successive ops cancel one another
        else
          // adjust 'previous' op by combining with sucessor
          Rotated(inner, combinedDegrees, 0, 0)
      case _ =>
        Rotated(this, degrees, aboutX, aboutY)
    }
  }


  type ReflectedT <: T

  protected val Reflected: ReflectedTransformable[T]

  def reflect(degrees: Double, aboutX: Double, aboutY: Double): T = {
    (aboutX, aboutY, this) match {
      // NOTE: simplify only when both reflect about (0,0)--else too complicated
      case (0, 0, Reflected(inner, existingDegrees, 0, 0))
             if ((degrees % 360) == (existingDegrees % 360)) =>
          inner // successive ops cancel one another
      case _ =>
        Reflected(this, degrees, aboutX, aboutY)
    }
  }


  type SkewedHorizT <: T

  protected val SkewedHoriz: SkewedHorizTransformable[T]

  def skewHoriz(degrees: Double): T = {
    this match {
      case SkewedHoriz(inner, existingDegrees) =>
        val combinedDegrees = degrees + existingDegrees
        if (combinedDegrees == 0)
          inner // successive ops cancel one another
        else
          // adjust 'previous' op by combining with sucessor
          SkewedHoriz(inner, combinedDegrees)
      case _ =>
        SkewedHoriz(this, degrees)
    }
  }


  type SkewedVertT <: T

  protected val SkewedVert: SkewedVertTransformable[T]

  def skewVert(degrees: Double): T = {
    this match {
      case SkewedVert(inner, existingDegrees) =>
        val combinedDegrees = degrees + existingDegrees
        if (combinedDegrees == 0)
          inner // successive ops cancel one another
        else
          // adjust 'previous' op by combining with sucessor
          SkewedVert(inner, combinedDegrees)
      case _ =>
        SkewedVert(this, degrees)
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
   isInstanceOf[TranslatedT] is safely implemented by the extending
   object.  'safely' is the key concept, since this is not checked by the
   compiler: one need be damn sure that for any value x, for which
   isInstanceOfCompanion(x) is true, that x.asInstanceOf[TranslatedT]
   will not blow up!!!!!

   this is the final, chosen solution:

trait TranslatedTransformable[TransformableT <: Transformable[TransformableT]] {

  protected type TranslatedT = TransformableT#TranslatedT

  def unapply(x: TranslatedT): Option[(TransformableT, Double, Double)]

  def unapply[T >: TransformableT](x: T):
      Option[(TransformableT, Double, Double)] =
    if (isInstanceOfCompanion(x)) unapply(x.asInstanceOf[TranslatedT])
    else None

  def apply(tformable: TransformableT, xDist: Double, yDist: Double):
      TransformableT


  protected def isInstanceOfCompanion(x: Any): Boolean
}

...

trait Transformable[T <: Transformable[T]] { self: T =>

  type TranslatedT <: T

  protected val Translated: TranslatedTransformable[T]

  ...
}

...

sealed abstract class Dims
    extends Transformable[Dims]
       with PresentableShape[Dims]
       with BoundingBoxed with Rectangular {

  type TranslatedT         = TranslatedDims
  protected val Translated = TranslatedDims

  ...
}

...

object TranslatedDims
    extends TranslatedTransformable[Dims] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[TranslatedDims]
}

final case class TranslatedDims(rect: Dims, xDist: Double, yDist: Double)
    extends Dims {
  ...
}

   ------------------------------------------------------------------------
NOTE: (in terms of the chosen solution above) attempting to replace:

trait TranslatedTransformable[TransformableT <: Transformable[TransformableT]] {
  ...
}

...

trait Transformable[T <: Transformable[T]] { self: T =>
  ...
}


with the following:


trait TranslatedTransformable[TransformingT <: Transforming[_]] {
  ...
}

...

trait Transformable[T] { self: T =>

  ...
}

results in the errors:

[ERROR] .../tie/tie/src/main/scala/k_k_/graphics/tie/transformable.scala:173: error: type arguments [T] do not conform to trait TranslatedTransformable's type parameter bounds [TransformingT <: k_k_.graphics.tie.transformable.Transforming[_]]
[INFO]   protected val Translated: TranslatedTransformable[T]
[INFO]                             ^
[ERROR] .../tie/tie/src/main/scala/k_k_/graphics/tie/transformable.scala:193: error: type arguments [T] do not conform to trait ScaledTransformable's type parameter bounds [TransformingT <: k_k_.graphics.tie.transformable.Transforming[_]]
[INFO]   protected val Scaled: ScaledTransformable[T]
[INFO]                         ^
[ERROR] .../tie/tie/src/main/scala/k_k_/graphics/tie/transformable.scala:213: error: type arguments [T] do not conform to trait RotatedTransformable's type parameter bounds [TransformingT <: k_k_.graphics.tie.transformable.Transforming[_]]
[INFO]   protected val Rotated: RotatedTransformable[T]
[INFO]                          ^
[ERROR] .../tie/tie/src/main/scala/k_k_/graphics/tie/transformable.scala:233: error: type arguments [T] do not conform to trait SkewedHorizTransformable's type parameter bounds [TransformingT <: k_k_.graphics.tie.transformable.Transforming[_]]
[INFO]   protected val SkewedHoriz: SkewedHorizTransformable[T]
[INFO]                              ^
[ERROR] .../tie/tie/src/main/scala/k_k_/graphics/tie/transformable.scala:252: error: type arguments [T] do not conform to trait SkewedVertTransformable's type parameter bounds [TransformingT <: k_k_.graphics.tie.transformable.Transforming[_]]
[INFO]   protected val SkewedVert: SkewedVertTransformable[T]
[INFO]                             ^

   ------------------------------------------------------------------------
this is what happens when x.asInstanceOf[TranslatedT] is used directly:

[WARNING] .../tie/tie/src/main/scala/k_k_/graphics/tie/transformable.scala:18: warning: abstract type TransformableT#TranslatedT in type TranslatedTransformable.this.TranslatedT is unchecked since it is eliminated by erasure
[INFO]     if (x.isInstanceOf[TranslatedT])
[INFO]                       ^
[WARNING] one warning found

trait TranslatedTransformable[TransformableT <: Transformable[TransformableT]] {

  type TranslatedT = TransformableT#TranslatedT

  def unapply(x: TranslatedT): Option[(TransformableT, Double, Double)]

  def unapply[T >: TransformableT](x: T):
      Option[(TransformableT, Double, Double)] =
    if (x.isInstanceOf[TranslatedT]) unapply(x.asInstanceOf[TranslatedT])
    else None

  def apply(tformable: TransformableT, xDist: Double, yDist: Double):
      TransformableT
}

...

trait Transformable[T <: Transformable[T]] { self: T =>

  type TranslatedT <: T

  protected val Translated: TranslatedTransformable[T]

  ...
}

...

object TranslatedDims
    extends TranslatedTransformable[Dims]


   ------------------------------------------------------------------------

   update: (fortunately!) the determination below is not strictly true:
   it is possible for the object extending this class to have a
   companion class which is a case class.  I had been implicitly
   assuming the following:


trait TranslatedTransformable[TransformableT <: Transformable[TransformableT]] {

  def unapply(x TransformableT): Option[(TransformableT, Double, Double)]

  def apply(tformable: TransformableT, xDist: Double, yDist: Double):
      TransformableT
}

...

object TranslatedDims
    extends TranslatedTransformable[Dims] {

  def unapply(orthoRect: Dims) =
    orthoRect match {
       case trans: TranslatedDims =>
         Some((trans.rect, trans.xDist, trans.yDist))
       case _ => None
    }

  def apply(rect: Dims, xDist: Double, yDist: Double) =
    new TranslatedDims(rect, xDist, yDist)
}

final class TranslatedDims(val rect: Dims, val xDist: Double, val yDist: Double)
    extends Dims {
  ...
}


but it is actually possible to limit unapply to TransformableT or a
super type, which then causes no overload resolution error with the
definition of unapply generated by the case class.  thus, the
companion class may remain a case class with a nice unapply method
that may be used directly, and its companion object must only define
another unapply which delegates to the case class version when the
value which is statically-typed as a TransformableT or a super type
is actually dynamically found to be the correct sub type of
TransformableT!


trait TranslatedTransformable[TransformableT <: Transformable[TransformableT]] {

  def unapply[T >: TransformableT](x: T):
      Option[(TransformableT, Double, Double)]

  def apply(tformable: TransformableT, xDist: Double, yDist: Double):
      TransformableT
}

...

object TranslatedDims
    extends TranslatedTransformable[Dims] {

  def unapply[T >: Dims](orthoRect: T): Option[(Dims, Double, Double)] =
    if (orthoRect.isInstanceOf[TranslatedDims])
      unapply(orthoRect.asInstanceOf[TranslatedDims])
    else
      None
}

final case class TranslatedDims(rect: Dims, xDist: Double, yDist: Double)
    extends Dims {
  ...
}


   ------------------------------------------------------------------------

  the approach above requires extra work in that the object extending
  TranslatedTransformable must implement unapply (and apply, if
  wanted), since making its companion class a case class is not an
  possiblity, because doing so leaves the compiler unabile to resolve
  'overloaded unapply' (since the definition above uses the base type
  as the unapply param, whereas the case class uses the exact type of
  itself).  attempting to define the signature of unapply to have the
  exact type does not seem possible due to erasure, which then results
  in runtime failure due to 'unchecked' use (see the study below in
  the comments).


(the code below) causes:
[WARNING] .../tie/tie/src/main/scala/k_k_/graphics/tie/transformable.scala:88: warning: abstract type T#TranslatedT in type pattern T#TranslatedT is unchecked since it is eliminated by erasure
[INFO]       case Translated(inner, existingXDist, existingYDist) =>
[INFO]                      ^
[WARNING] one warning found
...
Tests in error:
  test(k_k_.test.graphics.tie.SvgSierpinskiTriangleTest)
  test_Rendering(k_k_.test.graphics.tie.SvgRendererTest)
  test(k_k_.test.graphics.tie.ink.Svg_Color_Rings_v_Radial_Gradient_Test)
  test(k_k_.test.graphics.tie.ink.SvgVennDiagramOpacityTest)
  test(k_k_.test.graphics.tie.ink.Svg_Color_Stripes_v_Linear_Gradient_Test)

Tests run: 8, Failures: 0, Errors: 5, Skipped: 0
...
of:
-------------------------------------------------------------------------------
Test set: k_k_.test.graphics.tie.ink.SvgVennDiagramOpacityTest
-------------------------------------------------------------------------------
Tests run: 1, Failures: 0, Errors: 1, Skipped: 0, Time elapsed: 0.047 sec <<< FAILURE!
test(k_k_.test.graphics.tie.ink.SvgVennDiagramOpacityTest)  Time elapsed: 0.031 sec  <<< ERROR!
java.lang.ClassCastException: k_k_.graphics.tie.shapes.OriginDims cannot be cast to k_k_.graphics.tie.shapes.TranslatedDims
        at k_k_.graphics.tie.shapes.TranslatedDims$.unapply(shapes.scala:1286)
        at k_k_.graphics.tie.transformable.Transformable$class.move(transformable.scala:48)
        at k_k_.graphics.tie.shapes.Dims.move(shapes.scala:1175)
        at k_k_.graphics.tie.shapes.TranslatedNonPreFormulatedShape.boundingBox(shapes.scala:1055)
        at k_k_.graphics.tie.shapes.CompositeShape.boundingBox(shapes.scala:1141)
        at k_k_.graphics.tie.shapes.CompositeShape.boundingBox(shapes.scala:1141)


trait TranslatedTransformable[TransformableT <: Transformable[TransformableT]] {

  type TranslatedT = TransformableT#TranslatedT

  def unapply(x: TranslatedT): Option[(TransformableT, Double, Double)]

...

trait Transformable[T <: Transformable[T]] { self: T =>

  type TranslatedT <: T

  protected val Translated: TranslatedTransformable[T]

  def move(xDist: Double, yDist: Double): T = {
    this match {
      case Translated(inner, existingXDist, existingYDist) =>

...

sealed abstract class Dims
    extends Transformable[Dims]
       with PresentableShape[Dims]
       with BoundingBoxed with Rectangular {
  ...

  type TranslatedT         = TranslatedDims
  protected val Translated = TranslatedDims

...

object TranslatedDims
    extends TranslatedTransformable[Dims]

final case class TranslatedDims(rect: Dims, xDist: Double, yDist: Double)
    extends Dims {
  ...
}
*/

/*
(the code below) causes:
[ERROR] .../tie/tie/src/main/scala/k_k_/graphics/tie/shapes.scala:1304: error: cannot resolve overloaded unapply
[INFO] case class TranslatedDims(rect: Dims,
[INFO]            ^
[WARNING] .../tie/tie/src/main/scala/k_k_/graphics/tie/transformable.scala:14: warning: abstract type TransformableT#TranslatedT in type TranslatedTransformable.this.TranslatedT is unchecked since it is eliminated by erasure
[INFO]     if (x.isInstanceOf[TranslatedT])
[INFO]                       ^
[WARNING] one warning found
[ERROR] one error found


trait TranslatedTransformable[TransformableT <: Transformable[TransformableT]] {

  type TranslatedT = TransformableT#TranslatedT

  def unapply(x: TranslatedT): Option[(TransformableT, Double, Double)]

  def unapply(x: TransformableT): Option[(TransformableT, Double, Double)] =
    if (x.isInstanceOf[TranslatedT])
      unapply(x.asInstanceOf[TranslatedT])
    else
      None

...

trait Transformable[T <: Transformable[T]] { self: T =>

  type TranslatedT <: T

  protected val Translated: TranslatedTransformable[T]

  def move(xDist: Double, yDist: Double): T = {
    this match {
      case Translated(inner, existingXDist, existingYDist) =>

...

sealed abstract class Dims
    extends Transformable[Dims]
       with PresentableShape[Dims]
       with BoundingBoxed with Rectangular {
  ...

  type TranslatedT         = TranslatedDims
  protected val Translated = TranslatedDims

...

object TranslatedDims
    extends TranslatedTransformable[Dims]

final case class TranslatedDims(rect: Dims, xDist: Double, yDist: Double)
    extends Dims {
  ...
}

*/
