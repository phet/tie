/*
   file: k_k_/test/graphics/tie/Svg_Shapes_Arg_Measurements_Test.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2011 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.test.graphics.tie

import org.junit._
import Assert._

import k_k_.graphics.tie._
import k_k_.graphics.tie.ink.{Named_Colors => C, _}
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.path._
import k_k_.graphics.tie.shapes.text._


@Test
class Svg_Shapes_Arg_Measurements_Test extends Svg_Shapes_Test {

  override val filename = "test_shapes_arg_measurements.svg"

  override val title = "Arg Measurements for Pre-Formulated Shapes"

  override
  protected def create_canvas() = {

    def write_on_left(text: String, ink: Ink) = {
      val writing = write(text, ink, Start_Align)
      val Rectangular(w, h) = writing.bounding_box
      writing -+ (w/2, 0)
    }

    val shapes_measurements_canvas = super.create_canvas()
    shapes_measurements_canvas.place(
  (Square(16) -~ Pen.fill(horiz_arg_1_color) -&
   (write_on_left("Horizontal Arg 1 - shape arg 1",          C.black) -+ (12,0))
   -+ (0, 0)) -&
  (Square(16) -~ Pen.fill(horiz_arg_2_color) -&
   (write_on_left("Horizontal Arg 2 - (opt) shape arg 2",    C.black) -+ (12,0))
   -+ (0, 25)) -&
  (Square(16) -~ Pen.fill(vert_arg_1_color) -&
   (write_on_left("Vertical Arg 1 - (opt) shape arg 2 or 3", C.black) -+ (12,0))
   -+ (0, 50)) -&
  (Square(16) -~ Pen.fill(vert_arg_2_color) -&
   (write_on_left("Vertical Arg 2 - (opt) shape arg 3 or 4", C.black) -+ (12,0))
   -+ (0, 75)) -* (.95),
                                    325, 220)
  }

  val center_pen = Pen.stroke(C.blue, 0.4)
  val center_X = (Line(10) -%  45) -&
                 (Line(10) -% -45) -~ center_pen

  override
  protected def label_shape(shape: Drawing_Shape, name: String, ink: Ink):
      Drawing_Shape = {
    super.label_shape(shape, name, ink) -&
    draw_visual_args(shape) -&
    center_X
  }

  val horiz_arg_1_color: Color = C.lawngreen
  val horiz_arg_2_color: Color = C.blue
  val vert_arg_1_color:  Color = C.violet
  val vert_arg_2_color:  Color = C.red

  val horiz_arg_1_pen = Pen.stroke(horiz_arg_1_color, 2)
  val horiz_arg_2_pen = Pen.stroke(horiz_arg_2_color, 2)
  val vert_arg_1_pen  = Pen.stroke(vert_arg_1_color, 2)
  val vert_arg_2_pen  = Pen.stroke(vert_arg_2_color, 2)

  protected def draw_visual_args(shape: Drawing_Shape): Drawing_Shape = {
    shape match {
      case Line(length) =>
        (Line(length) -%  0 -~ horiz_arg_1_pen -+ (0, -5))
      case Hemisphere(rad_width, rad_height, top_?) =>
        (Line(rad_width)  -%  0 -~ horiz_arg_1_pen -+
            (-rad_width/2, if (top_?) rad_height/2 else 0 - rad_height/2)) -&
        (Line(rad_height) -% 90 -~ vert_arg_1_pen  -+ (0, 0))
      case Equi_Triangle(width) =>
        (Line(width) -%  0 -~ horiz_arg_1_pen -+ (0, -(width * math.sqrt(3.0)/2)/2 - 5))
      case Iso_Triangle(width, height) =>
        (Line(width)  -%  0 -~ horiz_arg_1_pen -+ (0, -height/2 - 5)) -&
        (Line(height) -% 90 -~ vert_arg_1_pen  -+ (width/2 + 5, 0))
      case Right_Triangle(width, height) =>
        (Line(width)  -%  0 -~ horiz_arg_1_pen -+ (0, -height/2 - 5)) -&
        (Line(height) -% 90 -~ vert_arg_1_pen  -+ (width/2 + 5, 0))
      case Square(width) =>
        (Line(width) -%  0 -~ horiz_arg_1_pen -+ (0, -width/2 - 5)) -&
        (Line(width) -% 90 -~ horiz_arg_1_pen -+ (width/2 + 5, 0))
      case Rectangle(width, height) =>
        (Line(width)  -%  0 -~ horiz_arg_1_pen -+ (0, -height/2 - 5)) -&
        (Line(height) -% 90 -~ vert_arg_1_pen  -+ (width/2 + 5, 0))
      case Parallelogram(side_width, full_width, height) =>
        (Line(side_width) -%  0 -~ horiz_arg_1_pen -+ (-(full_width - side_width)/2, -height/2 - 10)) -&
        (Line(full_width) -%  0 -~ horiz_arg_2_pen -+ (0, -height/2 - 5)) -&
        (Line(height)     -% 90 -~ vert_arg_1_pen  -+ (full_width/2 + 5, 0))
      case Trapezoid(top_width, bottom_width, height) =>
        (Line(top_width)    -%  0 -~ horiz_arg_1_pen -+ (0, -height/2 - 10)) -&
        (Line(bottom_width) -%  0 -~ horiz_arg_2_pen -+ (0, -height/2 - 5)) -&
        (Line(height)       -% 90 -~ vert_arg_1_pen  -+ (bottom_width/2 + 5, 0))
      case pent @ Pentagon(side_width, full_width, side_height, full_height) =>
        pent match {
          case Reg_Pentagon(shape_width) =>
            (Line(shape_width) -%  0 -~ horiz_arg_1_pen -+ (0, -full_height/2 - 5))
          case _ =>
            (Line(side_width)  -%  0 -~ horiz_arg_1_pen -+ (0, -full_height/2 - 10)) -&
            (Line(full_width)  -%  0 -~ horiz_arg_2_pen -+ (0, -full_height/2 - 5)) -&
            (Line(side_height) -% 90 -~ vert_arg_1_pen  -+ (full_width/2 + 5, (full_height - side_height)/2)) -&
            (Line(full_height) -% 90 -~ vert_arg_2_pen  -+ (full_width/2 + 10, 0))
        }
      case hex @ Hexagon(side_width, full_width, height) =>
        hex match {
          case Reg_Hexagon(shape_width) =>
            (Line(shape_width) -%  0 -~ horiz_arg_1_pen -+ (0, -height/2 - 5))
          case _ =>
            (Line(side_width) -%  0 -~ horiz_arg_1_pen -+ (0, -height/2 - 10)) -&
            (Line(full_width) -%  0 -~ horiz_arg_2_pen -+ (0, -height/2 - 5)) -&
            (Line(height)     -% 90 -~ vert_arg_1_pen  -+ (full_width/2 + 5, 0))
        }
      case oct @ Octagon(side_width, full_width, side_height, full_height) =>
        oct match {
          case Reg_Octagon(shape_width) =>
            (Line(shape_width) -%  0 -~ horiz_arg_1_pen -+ (0, -shape_width/2 - 5)) -&
            (Line(shape_width) -% 90 -~ horiz_arg_1_pen -+ (shape_width/2 + 5, 0))
          case _ =>
            (Line(side_width)  -%  0 -~ horiz_arg_1_pen -+ (0, -full_height/2 - 10)) -&
            (Line(full_width)  -%  0 -~ horiz_arg_2_pen -+ (0, -full_height/2 - 5)) -&
            (Line(side_height) -% 90 -~ vert_arg_1_pen  -+ (full_width/2 + 5, 0)) -&
            (Line(full_height) -% 90 -~ vert_arg_2_pen  -+ (full_width/2 + 10, 0))
        }

// for some reason, the following causes this error (which also appears to not generate the surefire-reports files--so hard to read, at the least!!!!)
// [INFO] Surefire report directory: c:\projects\devel\scripts\tie\tie\target\surefire-reports
// org.apache.maven.surefire.booter.SurefireExecutionException: (class: k_k_/test/graphics/tie/Svg_Shapes_Arg_Measurements_Test, method: draw_visual_args signature: (Lk_k_/graphics/tie/shapes/Drawing_Shape;)Lk_k_/graphics/tie/shapes/Drawing_Shape;) Accessing value from uninitialized register pair 261/262; nested exception is java.lang.VerifyError: (class: k_k_/test/graphics/tie/Svg_Shapes_Arg_Measurements_Test, method: draw_visual_args signature: (Lk_k_/graphics/tie/shapes/Drawing_Shape;)Lk_k_/graphics/tie/shapes/Drawing_Shape;) Accessing value from uninitialized register pair 261/262
// java.lang.VerifyError: (class: k_k_/test/graphics/tie/Svg_Shapes_Arg_Measurements_Test, method: draw_visual_args signature: (Lk_k_/graphics/tie/shapes/Drawing_Shape;)Lk_k_/graphics/tie/shapes/Drawing_Shape;) Accessing value from uninitialized register pair 261/262
//        at java.lang.Class.getDeclaredMethods0(Native Method)
//        at java.lang.Class.privateGetDeclaredMethods(Class.java:2427)
//        at java.lang.Class.getMethod0(Class.java:2670)
//        at java.lang.Class.getMethod(Class.java:1603)
//        at org.junit.internal.requests.ClassRequest.hasSuiteMethod(ClassRequest.java:61)
//        at org.junit.internal.requests.ClassRequest.getRunnerClass(ClassRequest.java:50)
//        at org.junit.internal.requests.ClassRequest.getRunner(ClassRequest.java:28)
//        at org.apache.maven.surefire.junit4.JUnit4TestSet.<init>(JUnit4TestSet.java:45)
//        at org.apache.maven.surefire.junit4.JUnit4DirectoryTestSuite.createTestSet(JUnit4DirectoryTestSuite.java:56)
//        at org.apache.maven.surefire.suite.AbstractDirectoryTestSuite.locateTestSets(AbstractDirectoryTestSuite.java:96)
//        at org.apache.maven.surefire.Surefire.createSuiteFromDefinition(Surefire.java:209)
//        at org.apache.maven.surefire.Surefire.run(Surefire.java:156)
//        at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
//        at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:39)
//        at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:25)
//        at java.lang.reflect.Method.invoke(Method.java:597)
//        at org.apache.maven.surefire.booter.SurefireBooter.runSuitesInProcess(SurefireBooter.java:345)
//        at org.apache.maven.surefire.booter.SurefireBooter.main(SurefireBooter.java:1009)
//      case Circle(rad) =>
//        (Line(rad) -%  0 -~ horiz_arg_1_pen -+ (-rad/2, 0)) -&
//        (Line(rad) -% 90 -~ horiz_arg_1_pen -+ (0, -rad/2))
      case ellipse @ Ellipse(rad_width, rad_height) =>
        ellipse match {
          //???(see above)--why does this match work, but one above doesn't????
          case Diam_Ellipse(diam_width, diam_height) =>
            (Line(diam_width)  -%  0 -~ horiz_arg_1_pen) -&
            (Line(diam_height) -% 90 -~ vert_arg_1_pen)
          case Diam_Circle(diam) =>
            (Line(diam) -%  0 -~ horiz_arg_1_pen) -&
            (Line(diam) -% 90 -~ horiz_arg_1_pen)
          case Circle(rad) =>
            (Line(rad) -%  0 -~ horiz_arg_1_pen -+ (-rad/2, 0)) -&
            (Line(rad) -% 90 -~ horiz_arg_1_pen -+ (0, -rad/2))
          case _ =>
            (Line(rad_width)  -%  0 -~ horiz_arg_1_pen -+ (-rad_width/2, 0)) -&
            (Line(rad_height) -% 90 -~ vert_arg_1_pen  -+ (0, -rad_height/2))
        }
      case free_form @ Free_Form(_) =>
        draw_visual_args(free_form.bounding_box_shape)
      case writing @ Writing(_) =>
        draw_visual_args(writing.bounding_box_shape)
      case Translated_Shape(shape, x_move, y_move) =>
        draw_visual_args(shape).move(x_move, y_move)
      case Scaled_Shape(shape, x_scaling, y_scaling) =>
        draw_visual_args(shape).scale(x_scaling, y_scaling)
      case Rotated_Shape(shape, degrees, x_pivot, y_pivot) =>
        draw_visual_args(shape).rotate(degrees, x_pivot, y_pivot)
      case Composite_Shape(below, above) =>
        draw_visual_args(below)
        draw_visual_args(above)
      case Inked_Shape(shape, pen) =>
        draw_visual_args(shape) // ignore pen as far as measurement concerned...
    }
  }
}
