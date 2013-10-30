/*
   file: k_k_/test/graphics/tie/Svg_Shapes_Arg_Measurements_Test.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2013 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.test.graphics.tie

import org.junit._
import Assert._

import k_k_.graphics.tie._
import k_k_.graphics.tie.ink.{NamedColors => C, _}
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.path._
import k_k_.graphics.tie.shapes.text._


@Test
class SvgShapesArgMeasurementsTest extends SvgShapesTest {

  override val filename = "test_shapes_arg_measurements.svg"

  override val title = "Arg Measurements for Pre-Formulated Shapes"

  override
  protected def createCanvas() = {

    def writeOnLeft(text: String, ink: Ink) = {
      val writing = write(text, ink, TextAlign.Start)
      val Rectangular(w, h) = writing.boundingBox
      writing -+ (w/2, 0)
    }

    val shapesMeasurementsCanvas = super.createCanvas()
    shapesMeasurementsCanvas.place(
  (Square(16) -~ Pen.fill(horizArg1Color) -&
   (writeOnLeft("Horizontal Arg 1 - shape arg 1",          C.black) -+ (12,0))
   -+ (0, 0)) -&
  (Square(16) -~ Pen.fill(horizArg2Color) -&
   (writeOnLeft("Horizontal Arg 2 - (opt) shape arg 2",    C.black) -+ (12,0))
   -+ (0, 25)) -&
  (Square(16) -~ Pen.fill(vertArg1Color) -&
   (writeOnLeft("Vertical Arg 1 - (opt) shape arg 2 or 3", C.black) -+ (12,0))
   -+ (0, 50)) -&
  (Square(16) -~ Pen.fill(vertArg2Color) -&
   (writeOnLeft("Vertical Arg 2 - (opt) shape arg 3 or 4", C.black) -+ (12,0))
   -+ (0, 75)) -* (.95),
        325, 220
      )
  }

  val centerPen = Pen.stroke(C.blue, 0.4)
  val centerX =
      (Line(10) -%  45) -&
      (Line(10) -% -45) -~
      centerPen

  override
  protected def labelShape(shape: Shape, name: String, ink: Ink): Shape = {
    super.labelShape(shape, name, ink) -&
        drawVisualArgs(shape) -&
        centerX
  }

  val horizArg1Color: Color = C.lawnGreen
  val horizArg2Color: Color = C.blue
  val vertArg1Color:  Color = C.violet
  val vertArg2Color:  Color = C.red

  val horizArg1Pen = Pen.stroke(horizArg1Color, 2)
  val horizArg2Pen = Pen.stroke(horizArg2Color, 2)
  val vertArg1Pen  = Pen.stroke(vertArg1Color, 2)
  val vertArg2Pen  = Pen.stroke(vertArg2Color, 2)

  protected def drawVisualArgs(shape: Shape): Shape = {
    shape match {
      case Line(length) =>
        (Line(length) -%  0 -~ horizArg1Pen -+ (0, -5))
      case Hemisphere(radWidth, radHeight, isTop) =>
        (Line(radWidth)  -%  0 -~ horizArg1Pen -+
            (-radWidth/2, if (isTop) radHeight/2 else 0 - radHeight/2)) -&
        (Line(radHeight) -% 90 -~ vertArg1Pen  -+ (0, 0))
      case EquiTriangle(width) =>
        (Line(width) -%  0 -~ horizArg1Pen -+ (0, -(width * math.sqrt(3.0)/2)/2 - 5))
      case IsoTriangle(width, height) =>
        (Line(width)  -%  0 -~ horizArg1Pen -+ (0, -height/2 - 5)) -&
        (Line(height) -% 90 -~ vertArg1Pen  -+ (width/2 + 5, 0))
      case RightTriangle(width, height) =>
        (Line(width)  -%  0 -~ horizArg1Pen -+ (0, -height/2 - 5)) -&
        (Line(height) -% 90 -~ vertArg1Pen  -+ (width/2 + 5, 0))
      case Square(width) =>
        (Line(width) -%  0 -~ horizArg1Pen -+ (0, -width/2 - 5)) -&
        (Line(width) -% 90 -~ horizArg1Pen -+ (width/2 + 5, 0))
      case Rectangle(width, height) =>
        (Line(width)  -%  0 -~ horizArg1Pen -+ (0, -height/2 - 5)) -&
        (Line(height) -% 90 -~ vertArg1Pen  -+ (width/2 + 5, 0))
      case Parallelogram(sideWidth, fullWidth, height) =>
        (Line(sideWidth) -%  0 -~ horizArg1Pen -+ (-(fullWidth - sideWidth)/2, -height/2 - 10)) -&
        (Line(fullWidth) -%  0 -~ horizArg2Pen -+ (0, -height/2 - 5)) -&
        (Line(height)    -% 90 -~ vertArg1Pen  -+ (fullWidth/2 + 5, 0))
      case Trapezoid(topWidth, bottomWidth, height) =>
        (Line(topWidth)    -%  0 -~ horizArg1Pen -+ (0, -height/2 - 10)) -&
        (Line(bottomWidth) -%  0 -~ horizArg2Pen -+ (0, -height/2 - 5)) -&
        (Line(height)      -% 90 -~ vertArg1Pen  -+ (bottomWidth/2 + 5, 0))
      case pent @ Pentagon(sideWidth, fullWidth, sideHeight, fullHeight) =>
        pent match {
          case RegPentagon(shapeWidth) =>
            (Line(shapeWidth) -%  0 -~ horizArg1Pen -+ (0, -fullHeight/2 - 5))
          case _ =>
            (Line(sideWidth)  -%  0 -~ horizArg1Pen -+ (0, -fullHeight/2 - 10)) -&
            (Line(fullWidth)  -%  0 -~ horizArg2Pen -+ (0, -fullHeight/2 - 5)) -&
            (Line(sideHeight) -% 90 -~ vertArg1Pen  -+ (fullWidth/2 + 5, (fullHeight - sideHeight)/2)) -&
            (Line(fullHeight) -% 90 -~ vertArg2Pen  -+ (fullWidth/2 + 10, 0))
        }
      case hex @ Hexagon(sideWidth, fullWidth, height) =>
        hex match {
          case RegHexagon(shapeWidth) =>
            (Line(shapeWidth) -%  0 -~ horizArg1Pen -+ (0, -height/2 - 5))
          case _ =>
            (Line(sideWidth) -%  0 -~ horizArg1Pen -+ (0, -height/2 - 10)) -&
            (Line(fullWidth) -%  0 -~ horizArg2Pen -+ (0, -height/2 - 5)) -&
            (Line(height)    -% 90 -~ vertArg1Pen  -+ (fullWidth/2 + 5, 0))
        }
      case oct @ Octagon(sideWidth, fullWidth, sideHeight, fullHeight) =>
        oct match {
          case RegOctagon(shapeWidth) =>
            (Line(shapeWidth) -%  0 -~ horizArg1Pen -+ (0, -shapeWidth/2 - 5)) -&
            (Line(shapeWidth) -% 90 -~ horizArg1Pen -+ (shapeWidth/2 + 5, 0))
          case _ =>
            (Line(sideWidth)  -%  0 -~ horizArg1Pen -+ (0, -fullHeight/2 - 10)) -&
            (Line(fullWidth)  -%  0 -~ horizArg2Pen -+ (0, -fullHeight/2 - 5)) -&
            (Line(sideHeight) -% 90 -~ vertArg1Pen  -+ (fullWidth/2 + 5, 0)) -&
            (Line(fullHeight) -% 90 -~ vertArg2Pen  -+ (fullWidth/2 + 10, 0))
        }

// for some reason, the following causes this error (which also appears to not generate the surefire-reports files--so hard to read, at the least!!!!)
// [INFO] Surefire report directory: c:\projects\devel\scripts\tie\tie\target\surefire-reports
// org.apache.maven.surefire.booter.SurefireExecutionException: (class: kK_/test/graphics/tie/SvgShapesArgMeasurementsTest, method: drawVisualArgs signature: (LkK_/graphics/tie/shapes/Shape;)LkK_/graphics/tie/shapes/Shape;) Accessing value from uninitialized register pair 261/262; nested exception is java.lang.VerifyError: (class: kK_/test/graphics/tie/SvgShapesArgMeasurementsTest, method: drawVisualArgs signature: (LkK_/graphics/tie/shapes/Shape;)LkK_/graphics/tie/shapes/Shape;) Accessing value from uninitialized register pair 261/262
// java.lang.VerifyError: (class: kK_/test/graphics/tie/SvgShapesArgMeasurementsTest, method: drawVisualArgs signature: (LkK_/graphics/tie/shapes/Shape;)LkK_/graphics/tie/shapes/Shape;) Accessing value from uninitialized register pair 261/262
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
//        (Line(rad) -%  0 -~ horizArg1Pen -+ (-rad/2, 0)) -&
//        (Line(rad) -% 90 -~ horizArg1Pen -+ (0, -rad/2))
      case ellipse @ Ellipse(radWidth, radHeight) =>
        ellipse match {
          //???(see above)--why does this match work, but one above doesn't????
          case DiamEllipse(diamWidth, diamHeight) =>
            (Line(diamWidth)  -%  0 -~ horizArg1Pen) -&
            (Line(diamHeight) -% 90 -~ vertArg1Pen)
          case DiamCircle(diam) =>
            (Line(diam) -%  0 -~ horizArg1Pen) -&
            (Line(diam) -% 90 -~ horizArg1Pen)
          case Circle(rad) =>
            (Line(rad) -%  0 -~ horizArg1Pen -+ (-rad/2, 0)) -&
            (Line(rad) -% 90 -~ horizArg1Pen -+ (0, -rad/2))
          case _ =>
            (Line(radWidth)  -%  0 -~ horizArg1Pen -+ (-radWidth/2, 0)) -&
            (Line(radHeight) -% 90 -~ vertArg1Pen  -+ (0, -radHeight/2))
        }
      case freeForm @ FreeForm(_) =>
        drawVisualArgs(freeForm.boundingBoxShape)
      case writing @ Writing(_) =>
        drawVisualArgs(writing.boundingBoxShape)
      case TranslatedShape(shape, xMove, yMove) =>
        drawVisualArgs(shape).move(xMove, yMove)
      case ScaledShape(shape, xScaling, yScaling) =>
        drawVisualArgs(shape).scale(xScaling, yScaling)
      case RotatedShape(shape, degrees, xPivot, yPivot) =>
        drawVisualArgs(shape).rotate(degrees, xPivot, yPivot)
      case CompositeShape(below, above) =>
        drawVisualArgs(below)
        drawVisualArgs(above)
      case InkedShape(shape, pen) =>
        drawVisualArgs(shape) // ignore pen as far as measurement concerned...
    }
  }
}
