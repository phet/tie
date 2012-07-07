/*
   file: k_k_/graphics/tie/tile.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie

import k_k_.graphics.tie.shapes.{Bounding_Boxed, Point, Shape}


package object tile {

  // conversion for Shape, Ortho_Rectangle
  implicit def Bounding_Boxed_to_Bounding_Box_Points(bboxed: Bounding_Boxed) =
    new Bounding_Box_Points(bboxed)

  // Bounding_Box_Pos aliases:
  val T_Mid = Top_Middle
  val B_Mid = Bottom_Middle
  val L_Mid = Left_Middle
  val R_Mid = Right_Middle
  val T_L   = Top_Left
  val B_R   = Bottom_Right
  val T_R   = Top_Right
  val B_L   = Bottom_Left

  // Alignment_Shift aliases:
  val CW  = Clockwise
  val CCW = Counter_Clockwise
}


package tile {


final class Bounding_Box_Points(bboxed: Bounding_Boxed) {

  def center       : Point = center_pt

  def top_middle   : Point = center_pt move ( 0,       -height/2)
  def bottom_middle: Point = center_pt move ( 0,        height/2)
  def left_middle  : Point = center_pt move (-width/2,  0)
  def right_middle : Point = center_pt move ( width/2,  0)

  def top_left     : Point = center_pt move (-width/2, -height/2)
  def top_right    : Point = center_pt move ( width/2, -height/2)
  def bottom_left  : Point = center_pt move (-width/2,  height/2)
  def bottom_right : Point = center_pt move ( width/2,  height/2)


  private val (center_pt, width, height) = {
    val bbox = Bounding_Boxed(bboxed)
    (bbox.center_pt, bbox.width, bbox.height)
  }
}


sealed abstract class Bounding_Box_Pos(pt_method: Bounding_Box_Points => Point){

  def opposite: Bounding_Box_Pos

  def clockwise        : Bounding_Box_Pos
  def counter_clockwise: Bounding_Box_Pos

  lazy val cw  = clockwise
  lazy val ccw = counter_clockwise


  def apply(obj: Bounding_Box_Points): Point =
    pt_method(obj)

  // adjusts sign of x, y toward direction of pos.
  def apply(pt: Point): Point

  def apply(x: Double, y: Double): Point =
    apply(Point(x, y))


  def of(shape: Shape): pos.Shape_Pos =
    pos.Shape_Pos(shape, this)


  def cycle_cw: Seq[Bounding_Box_Pos] = {
    val start = this
    def walk_cw(from_pos: Bounding_Box_Pos): Stream[Bounding_Box_Pos] = {
      val next_pos = from_pos.cw
      if (next_pos == start) Stream.empty
      else Stream.cons(next_pos, walk_cw(next_pos))
    }
    Stream.cons(start, walk_cw(start))
  }

  def cycle_ccw: Seq[Bounding_Box_Pos] = {
    val start = this
    def walk_ccw(from_pos: Bounding_Box_Pos): Stream[Bounding_Box_Pos] = {
      val next_pos = from_pos.ccw
      if (next_pos == start) Stream.empty
      else Stream.cons(next_pos, walk_ccw(next_pos))
    }
    Stream.cons(start, walk_ccw(start))
  }
}

case object Center        extends Bounding_Box_Pos(_.center)        {
  lazy val opposite           = this
  lazy val clockwise          = this
  lazy val counter_clockwise  = this

  def apply(pt: Point): Point = pt
}
case object Top_Middle    extends Bounding_Box_Pos(_.top_middle)    {
  lazy val opposite           = Bottom_Middle
  lazy val clockwise          = Top_Right
  lazy val counter_clockwise  = Top_Left

  def apply(pt: Point): Point = (pt.x, -pt.y)
}
case object Bottom_Middle extends Bounding_Box_Pos(_.bottom_middle) {
  lazy val opposite           = Top_Middle
  lazy val clockwise          = Bottom_Left
  lazy val counter_clockwise  = Bottom_Right

  def apply(pt: Point): Point = (pt.x, pt.y)
}
case object Left_Middle   extends Bounding_Box_Pos(_.left_middle)   {
  lazy val opposite           = Right_Middle
  lazy val clockwise          = Top_Left
  lazy val counter_clockwise  = Bottom_Left

  def apply(pt: Point): Point = (-pt.x, pt.y)
}
case object Right_Middle  extends Bounding_Box_Pos(_.right_middle)  {
  lazy val opposite           = Left_Middle
  lazy val clockwise          = Bottom_Right
  lazy val counter_clockwise  = Top_Right

  def apply(pt: Point): Point = (pt.x, pt.y)
}
case object Top_Left      extends Bounding_Box_Pos(_.top_left)      {
  lazy val opposite           = Bottom_Right
  lazy val clockwise          = Top_Middle
  lazy val counter_clockwise  = Left_Middle

  def apply(pt: Point): Point = (-pt.x, -pt.y)
}
case object Bottom_Right  extends Bounding_Box_Pos(_.bottom_right)  {
  lazy val opposite           = Top_Left
  lazy val clockwise          = Bottom_Middle
  lazy val counter_clockwise  = Right_Middle

  def apply(pt: Point): Point = (pt.x, pt.y)
}
case object Top_Right     extends Bounding_Box_Pos(_.top_right)     {
  lazy val opposite           = Bottom_Left
  lazy val clockwise          = Right_Middle
  lazy val counter_clockwise  = Top_Middle

  def apply(pt: Point): Point = (pt.x, -pt.y)
}
case object Bottom_Left   extends Bounding_Box_Pos(_.bottom_left)   {
  lazy val opposite           = Top_Right
  lazy val clockwise          = Left_Middle
  lazy val counter_clockwise  = Bottom_Middle

  def apply(pt: Point): Point = (-pt.x, pt.y)
}


sealed abstract class Alignment_Shift(op: Bounding_Box_Pos => Bounding_Box_Pos){

  def opposite: Alignment_Shift

  def apply(pos: Bounding_Box_Pos): Bounding_Box_Pos =
    op(pos)


  def companion: Alignment_Shift = // used to implement twists
    Stationary
}
case object Stationary        extends Alignment_Shift(identity) {
  lazy val opposite = this
}
case object Clockwise         extends Alignment_Shift(_.clockwise) {
  lazy val opposite = Counter_Clockwise
}
case object Counter_Clockwise extends Alignment_Shift(_.counter_clockwise) {
  lazy val opposite = Clockwise
}

case object CW_Twist          extends Alignment_Shift(_.clockwise) {
  lazy val opposite = CCW_Twist

  override val companion = opposite
}
case object CCW_Twist         extends Alignment_Shift(_.counter_clockwise) {
  lazy val opposite = CW_Twist

  override val companion = opposite
}


sealed abstract class Alignment_Relation
case object Inside   extends Alignment_Relation
case object Outside  extends Alignment_Relation
case object Centered extends Alignment_Relation

}


/*****************************************************************************
[the saga of getting `Bounding_Box_Pos.opposite` to work]

---------------
this is the final, working solution:

sealed abstract class Bounding_Box_Pos(pt_method: Bounding_Box_Points => Point){

  val opposite: Bounding_Box_Pos

  def apply(obj: Bounding_Box_Points): Point =
    pt_method(obj)
}

case object Center        extends Bounding_Box_Pos(_.center)        {
  lazy val opposite = this
}
case object Top_Middle    extends Bounding_Box_Pos(_.top_middle)    {
  lazy val opposite = Bottom_Middle
}
case object Bottom_Middle extends Bounding_Box_Pos(_.bottom_middle) {
  lazy val opposite = Top_Middle
}
case object Left_Middle   extends Bounding_Box_Pos(_.left_middle)   {
  lazy val opposite = Right_Middle
}
case object Right_Middle  extends Bounding_Box_Pos(_.right_middle)  {
  lazy val opposite = Left_Middle
}
case object Top_Left      extends Bounding_Box_Pos(_.top_left)      {
  lazy val opposite = Bottom_Right
}
case object Bottom_Right  extends Bounding_Box_Pos(_.bottom_right)  {
  lazy val opposite = Top_Left
}
case object Top_Right     extends Bounding_Box_Pos(_.top_right)     {
  lazy val opposite = Bottom_Left
}
case object Bottom_Left   extends Bounding_Box_Pos(_.bottom_left)   {
  lazy val opposite = Top_Right
}
---------------



1a. the following:

sealed abstract class Bounding_Box_Pos(
    method: Bounding_Box_Points => Point,
    val opposite: Bounding_Box_Pos
  ) {

  def apply(obj: Bounding_Box_Points) =
    method(obj)
}

case object Center        extends Bounding_Box_Pos(_.center, Center)

1b. causes:
-------------------------------------------------------------------------------
Test set: k_k_.test.graphics.tie.tile.Svg_Alignment_Test
-------------------------------------------------------------------------------
Tests run: 1, Failures: 0, Errors: 1, Skipped: 0, Time elapsed: 0.5 sec <<< FAILURE!
test_alignment(k_k_.test.graphics.tie.tile.Svg_Alignment_Test)  Time elapsed: 0.047 sec  <<< ERROR!
java.lang.VerifyError: (class: k_k_/graphics/tie/tile/Center$, method: <init> signature: ()V) Expecting to find object/array on stack
        at k_k_.test.graphics.tie.tile.Svg_Alignment_Test.create_canvas(Svg_Alignment_Test.scala:71)
        at k_k_.test.graphics.tie.tile.Svg_Alignment_Test.test_alignment(Svg_Alignment_Test.scala:36)
        at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
        at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:39)
        at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:25)
        at java.lang.reflect.Method.invoke(Method.java:597)
        at org.junit.internal.runners.TestMethod.invoke(TestMethod.java:59)
        at org.junit.internal.runners.MethodRoadie.runTestMethod(MethodRoadie.java:98)

2a. the following:

sealed abstract class Bounding_Box_Pos(
    method: Bounding_Box_Points => Point,
    val opposite: Bounding_Box_Pos
  ) {

  def this(method: Bounding_Box_Points => Point) =
    this(method, Center)

  def apply(obj: Bounding_Box_Points) =
    method(obj)
}

case object Center        extends Bounding_Box_Pos(_.center)

2b. causes:
-------------------------------------------------------------------------------
Test set: k_k_.test.graphics.tie.tile.Svg_Alignment_Test
-------------------------------------------------------------------------------
Tests run: 1, Failures: 0, Errors: 1, Skipped: 0, Time elapsed: 0.578 sec <<< FAILURE!
test_alignment(k_k_.test.graphics.tie.tile.Svg_Alignment_Test)  Time elapsed: 0.25 sec  <<< ERROR!
java.lang.NullPointerException
        at k_k_.graphics.tie.tile.adjust.Adjustable_Shape.calc_offset$1(adjust.scala:201)
        at k_k_.graphics.tie.tile.adjust.Adjustable_Shape.align_with(adjust.scala:206)
        at k_k_.graphics.tie.tile.adjust.Adjustable_Shape.align_with(adjust.scala:212)
        at k_k_.test.graphics.tie.tile.Svg_Alignment_Test.align_shape(Svg_Alignment_Test.scala:65)
        at k_k_.test.graphics.tie.tile.Svg_Alignment_Test.create_canvas(Svg_Alignment_Test.scala:111)
        at k_k_.test.graphics.tie.tile.Svg_Alignment_Test.test_alignment(Svg_Alignment_Test.scala:36)
        at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
        at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:39)
        at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:25)
        at java.lang.reflect.Method.invoke(Method.java:597)
        at org.junit.internal.runners.TestMethod.invoke(TestMethod.java:59)
        at org.junit.internal.runners.MethodRoadie.runTestMethod(MethodRoadie.java:98)

3a. the following:

sealed abstract class Bounding_Box_Pos(
    method: Bounding_Box_Points => Point,
    val opposite: Bounding_Box_Pos
  ) {

  def this(method: Bounding_Box_Points => Point) =
    this(method, this)

  def apply(obj: Bounding_Box_Points) =
    method(obj)
}

case object Center        extends Bounding_Box_Pos(_.center)

3b. causes:

[ERROR] .../tie/tile/src/main/scala/k_k_/graphics/tie/tile.scala:39: error: this can be used only in a class, object, or template
[INFO]     this(method, this)
[INFO]                  ^

4a. the following:

sealed abstract class Bounding_Box_Pos(
    method: Bounding_Box_Points => Point,
    val opposite: Bounding_Box_Pos
  ) {

  def apply(obj: Bounding_Box_Points) =
    method(obj)
}

case object Center        extends Bounding_Box_Pos(_.center, this)

4b. causes: (the same as 3b.)

5a. the following:

sealed abstract class Bounding_Box_Pos(
    method: Bounding_Box_Points => Point,
    opposite_point: Option[Bounding_Box_Pos]
  ) {

  def this(method: Bounding_Box_Points => Point,
           opposite: Bounding_Box_Pos) =
    this(method, Some(opposite))

  def this(method: Bounding_Box_Points => Point) =
    this(method, None)


  def apply(obj: Bounding_Box_Points) =
    method(obj)

  val opposite: Bounding_Box_Pos =
    opposite_point match {
      case Some(pt) => pt
      case None     => this
    }
}

case object Center        extends Bounding_Box_Pos(_.center)
case object Top_Middle    extends Bounding_Box_Pos((_: Bounding_Box_Points).top_middle,   Bottom_Middle)

5b. causes:

  [it is odd, because the line 'Svg_Alignment_Test.create_canvas(Svg_Alignment_Test.scala:117)' is not the first, but the third alignment shown below:

               (align_shape(Rectangle(80, 50), Center, Outside)
                  -+ (60, 0)) -&
               (align_shape(Equi_Triangle(60), L_Mid, Outside)
                  -+ (200, 0)) -&
               (align_shape(Parallelogram(50, 70, 50), T_Mid, Outside)
                  -+ (340, 0)) -&
  ]

-------------------------------------------------------------------------------
Test set: k_k_.test.graphics.tie.tile.Svg_Alignment_Test
-------------------------------------------------------------------------------
Tests run: 1, Failures: 0, Errors: 1, Skipped: 0, Time elapsed: 0.641 sec <<< FAILURE!
test_alignment(k_k_.test.graphics.tie.tile.Svg_Alignment_Test)  Time elapsed: 0.234 sec  <<< ERROR!
java.lang.NullPointerException
        at k_k_.graphics.tie.tile.adjust.Adjustable_Shape.calc_offset$1(adjust.scala:319)
        at k_k_.graphics.tie.tile.adjust.Adjustable_Shape.align_with(adjust.scala:324)
        at k_k_.graphics.tie.tile.adjust.Adjustable_Shape.align_with(adjust.scala:330)
        at k_k_.test.graphics.tie.tile.Svg_Alignment_Test.align_shape(Svg_Alignment_Test.scala:65)
        at k_k_.test.graphics.tie.tile.Svg_Alignment_Test.create_canvas(Svg_Alignment_Test.scala:117)
        at k_k_.test.graphics.tie.tile.Svg_Alignment_Test.test_alignment(Svg_Alignment_Test.scala:36)
        at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
        at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:39)
        at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:25)
        at java.lang.reflect.Method.invoke(Method.java:597)
        at org.junit.internal.runners.TestMethod.invoke(TestMethod.java:59)
        at org.junit.internal.runners.MethodRoadie.runTestMethod(MethodRoadie.java:98)

6a. the following:

sealed abstract class Bounding_Box_Pos(
    method: Bounding_Box_Points => Point,
    opposite_pt: Bounding_Box_Pos
  ) {

  def this(method: Bounding_Box_Points => Point) =
    this(method, Center)

  // NOTE: must define separate attribute, since 'val' (c'tor) params may not
  // be lazy *and* 'val' params may not be call-by-name
  lazy val opposite = opposite_pt

  def apply(obj: Bounding_Box_Points) =
    method(obj)
}

case object Center        extends Bounding_Box_Pos(_.center,      Center)
case object Top_Middle    extends Bounding_Box_Pos(_.top_middle,  Bottom_Middle)

6b. causes:

-------------------------------------------------------------------------------
Test set: k_k_.test.graphics.tie.tile.Svg_Alignment_Test
-------------------------------------------------------------------------------
Tests run: 1, Failures: 0, Errors: 1, Skipped: 0, Time elapsed: 0.391 sec <<< FAILURE!
test_alignment(k_k_.test.graphics.tie.tile.Svg_Alignment_Test)  Time elapsed: 0.032 sec  <<< ERROR!
java.lang.VerifyError: (class: k_k_/graphics/tie/tile/Center$, method: <init> signature: ()V) Expecting to find object/array on stack
        at k_k_.test.graphics.tie.tile.Svg_Alignment_Test.create_canvas(Svg_Alignment_Test.scala:71)
        at k_k_.test.graphics.tie.tile.Svg_Alignment_Test.test_alignment(Svg_Alignment_Test.scala:36)
        at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
        at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:39)
        at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:25)
        at java.lang.reflect.Method.invoke(Method.java:597)
        at org.junit.internal.runners.TestMethod.invoke(TestMethod.java:59)
        at org.junit.internal.runners.MethodRoadie.runTestMethod(MethodRoadie.java:98)


******************************************************************************/
