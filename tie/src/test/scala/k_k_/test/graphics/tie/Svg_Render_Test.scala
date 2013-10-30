/*
   file: k_k_/test/graphics/tie/Svg_Render_Test.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2013 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.test.graphics.tie

import java.io.File

import org.junit._
import Assert._
import org.scalatest.junit.JUnitSuite

import k_k_.graphics.tie._
import k_k_.graphics.tie.Attribution._
import k_k_.graphics.tie.fmt.svg.SvgRenderer
import k_k_.graphics.tie.ink.{NamedColors => C, _}
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.path._
import k_k_.graphics.tie.shapes.text._


@Test
class SvgRendererTest extends JUnitSuite {

  @Test
  def testRendering() {
    val testOutDir = "target/test-classes/rendered_out"
    val testFilename = "test_rendering.svg"

    val testFilepath = testOutDir + "/" + testFilename
    val outFile = new File(testFilepath)
    assertTrue(
        outFile.getParentFile.isDirectory || outFile.getParentFile.mkdirs
      )

    /*
       prev attempts at 3d cylinder, as rendered into SVG:

       <!--   <path d="M0,0 A 50 10 0 1 0 -1,0 z m -50,-10 v60 a 50 10 0 0 0 100,0 v-60" /> -->
       <path d="M0,0 A 50 10 0 1 0 100,0 a 50 10 0 1 0 -100, 0 z v60 a 50 10 0 0 0 100,0 v-60" />
    */

    val freeForm3dCylinder = Path.
        from_@(0, 0).
        arc_@(50, 10, ArcChoice.LargeCCW, 100, 0).
        arc  (50, 10, ArcChoice.LargeCCW, -100, 0).
        // close.
        vertical (60).
        arc  (50, 10, ArcChoice.SmallCCW, 100, 0).
        vertical (-60)
    val freeFormVase = Path.
        from(0, 160).
        quadratic(-30, -60, 0, -120).
        quadratic(10, -20, 0, -40).
        horizontal(40).
        quadratic(-10, 20, 0, 40).
        quadratic(30, 60, 0, 120).
        close

    val keyhole = Path.
        arc(10, 10, ArcChoice.LargeCW, 10, 0).
        line(1, 16).
        horiz(-12).
        line(1, -16).
        close -+ (-5, 1)
    /*
      Path.from(0, 0).
           quad(-30, 30, -8, 60).
           horiz(16).
           quad(30, -30, -8, -60).
           close
    */

    val redYellowGrad = new LinearGradient(
        ColorStop(C.red, 20) :: ColorStop(C.yellow, 80) :: Nil
      )

    val checkerDiamondPat = Pattern(Path.
        from(0, 5).
        line(5, 5).
        line(5, -5).
        line(-5, -5).
        close -~
        Pen(C.Black, C.Black, 1)
      )

    val gridFillPen =
        Pen.fill(Pattern(Rectangle(20, 20) -+ (10) -~ Pen.stroke(C.Black, .1)))

    val exampleScene =
        (Rectangle(45, 70) -~ Pen.fill(C.yellow) -+ (0, -3)) -&
        (RightTriangle(30, 40) -~ Pen.fill(C.indigo)   -+ (-15, -19)) -&
        (Circle(20) -~ Pen.fill(redYellowGrad -/- 45) -+ (12, 15))

/*
    val canvas = new Canvas(CanvasProps(500, 500, OriginPos.TopLeft),
                            Rectangle(80, 50).using(Pen.fill(C.red)) -+ 100,
                            Ellipse(40, 30) -+ (200,100),
                            Parallelogram(40, 60, 40) -+ (300, 100)
                              -~ Pen.fill(C.brown),
                            Ellipse(40, 30) -+ (400,100) -~
                              Pen.fill(redYellowGrad -% 90),
                            Octagon(60, 100, 30, 70) -~
                              Pen.fill(redYellowGrad) -+ 200,
                            Octagon(40, 80, 40, 80) -~
                              Pen(C.red, C.yellow) -+ (350, 200),
                            Square(50) -% 45 -~
                              Pen.fill(redYellowGrad) -+ 350,
                            FreeForm(freeForm3dCylinder) -~
                              Pen(C.Black, redYellowGrad -% 60) -+
                              (450, 200),
                            Line(50) -% 45 -+ 300 -~ Pen.stroke(C.green, 2.5)).
                       place(Hexagon(50, 80, 50) -% 90 -* (2, 1.5) -~
                             Pen.fill(C.blue), 200, 350).
                       place(Pentagon(50, 50, 35, 55) -~
                             Pen.dashed(C.orange, 10), 100, 180).
                       place(Pentagon(50, 50, 35, 55) -~ Pen.fill(C.Purple)
                             -% 180 -+ (100, 250)).
                       place(FreeForm(freeFormVase) -* (2, 0.75) -~
                             Pen.stroke(C.Black, 1), 450, 300).
                       place(Pentagon(35, 50, 15, 40) -+ (100, 400)).
                       place(Trapezoid(60, 80, 50) using
                             Pen.fill(C.green), 100, 350)
*/


    val arialFont = Font("Arial", FontSize.Std(12))
    // val trialText = TextBlock(TextLine("testing 1, 2, 3", nameFont))
    val trialText = TextLine("testing 1, 2, 3", arialFont, TextAlign.Middle)


    val centerPen = Pen.stroke(C.gray, 0.4)
    val centerX =
        (Line(10) -%  45) -&
        (Line(10) -% -45) -~
        centerPen
    val measurePen = Pen.stroke(C.Brown, 2)

    val bboxPen = Pen.dashed(C.pink, 0.6, 10, 0)

    val measurementsCanvas = new Canvas(
        CanvasProps(710, 500, OriginPos.TopLeft),
        Rectangle(80, 50).using(Pen.fill(C.red)) -:- (
            IdAttribution("rect"),
            LinkAttribution("http://scala-lang.org")
          ) -&
        (Line(80) -%  0 -~ measurePen -+ (0, -30)) -& 
        (Line(50) -% 90 -~ measurePen -+ (-45, 0))
        -& centerX
        -+ 100,
//??????why isn't this possible????:
// NOTE: there are no 'default argument' methods in PresentableShape, merely:
//  def -:(attribution: Attribution): T =
//    as(attribution)
//
//  def -:(attribution1: Attribution, attribution2: Attribution): T =
//    as(attribution1).as(attribution2)
//
//  def -:(id: String): T =
//    as(IdAttribution(id))
//
//[ERROR] ../tie/tie/src/test/scala/kK_/test/graphics/tie/SvgRenderTest.scala:143: error: overloaded method value -: with alternatives:
//[INFO]   (id: String)kK_.graphics.tie.shapes.DrawingShape <and>
//[INFO]   (attribution: kK_.graphics.tie.Attribution)kK_.graphics.tie.shapes.DrawingShape
//[INFO]  cannot be applied to ((kK_.graphics.tie.IdAttribution, kK_.graphics.tie.LinkAttribution))
//[ERROR] Error occurred in an application involving default arguments.
//[INFO]                             (Ellipse(40, 30) -&
//[INFO]                             ^
//                          (IdAttribution("measured"),
//                           LinkAttribution("http://scala-lang.org",
//                                            TargetBlank)) -:
        IdAttribution("measured") -:
        LinkAttribution("http://scala-lang.org", LinkTarget.Blank) -:
        (Ellipse(40, 30) -&
         (Line(40) -%  0 -~ measurePen -+ (-20, -35)) -&
         (Line(30) -% 90 -~ measurePen -+ (-45, -15)) -&
         (Line(40) -%  0 -~ measurePen -+ (-20, 0)) -&
         (Line(30) -% 90 -~ measurePen -+ (0, -15))
         -& centerX
         -& (Ellipse(40, 30).boundingBox.asShape -~ bboxPen)
         -+ 100 -+ (100,0)),
        Parallelogram(40, 60, 40) -~ Pen.fill(C.brown) -&
        (Line(40) -%  0 -~ measurePen -+ (10, -25)) -&
        (Line(60) -%  0 -~ measurePen -+ (0, 25)) -&
        (Line(40) -% 90 -~ measurePen -+ (-35, 0))
        -& centerX
        -& ((Parallelogram(40, 60, 40) -~ Pen.fill(C.brown)).boundingBoxShape -~
            bboxPen)
        -+ 300 -+ (0, -200),
        ("feds" -: Link("http://us.gov") -: Ellipse(40, 30))
        -+ (400,100) -~
        Pen.fill(redYellowGrad -% 90),
        ("I||eg@L-id" -: Octagon(60, 100, 30, 70)) -~
        Pen.fill(redYellowGrad) -&
        (Line(60) -%  0 -~ measurePen -+ (0, -40)) -&
        (Line(100)-%  0 -~ measurePen -+ (0, 40)) -&
        (Line(30) -% 90 -~ measurePen -+ (-55, 0)) -&
        (Line(70) -% 90 -~ measurePen -+ (55, 0))
        -& centerX
        -+ 200,
        Octagon(40, 80, 40, 80) -~ Pen(C.red, C.yellow) -&
        (Line(40) -%  0 -~ measurePen -+ (0, -45)) -&
        (Line(80) -%  0 -~ measurePen -+ (0, 45)) -&
        (Line(40) -% 90 -~ measurePen -+ (-45, 0)) -&
        (Line(80) -% 90 -~ measurePen -+ (45, 0))
        -& centerX
        -& ((Octagon(40, 80, 40, 80) -~ Pen(C.red, C.yellow)).boundingBoxShape
                -~ bboxPen)
        -+ (350, 200),
        Square(50) -~
        Pen.fill(redYellowGrad) -&
        (Line(50) -%  0 -~ measurePen -+ (0, -30)) -&
        (Line(50) -% 70 -% 20 -~ measurePen -+ (-30, 0))
        -& centerX
        -% 45 
        -& ((Square(50)-~ Pen.fill(redYellowGrad)-% 45).boundingBoxShape -~
                bboxPen)
        -+ 350,
        FreeForm(freeForm3dCylinder) -:- Link("http://www.wikipedia.org")
        -* .5 -* 2 -~
        Pen(C.Black, redYellowGrad -% 80 -% -20)
        -& ((FreeForm(freeForm3dCylinder) -* .5 -* 2 -~
             Pen(C.Black, redYellowGrad -% 80 -% -20)).boundingBoxShape -~
                bboxPen)
        -& centerX
        -+ (440, 200),
        Line(50) -~ Pen.stroke(C.Green, 2.5) -&
        (Line(50) -%  0 -~ measurePen -+ (0, -5))
        -& centerX
        -% 45 -+ 300
      ).place(
        Hexagon(50, 80, 50) -~ Pen.fill(C.blue) -&
        (Line(50) -%  0 -~ measurePen -+ (0, -28)) -&
        (Line(80) -%  0 -~ measurePen -+ (0, 28)) -&
        (Line(50) -% 90 -~ measurePen -+ (-42.5, 0))
        -% 90 -# .5
        -* (2, 1.5)
        -& centerX, 200, 350
      ).place(
        IsoTriangle(30, 40) -~ Pen.fill(C.silver) -&
        (Line(30) -%  0 -~ measurePen -+ (0, 25)) -&
        (Line(40) -% 90 -~ measurePen -+ (-20, 0))
        -& centerX, 300, 400
      ).place(
        IsoTriangle(30, 40) -~ Pen.fill(C.LightGreen) -&
        (Line(30) -%  0 -~ measurePen -+ (0, 25)) -&
        (Line(40) -% 90 -~ measurePen -+ (-20, 0))
        -& centerX
        -% 105 -% -15
        -& ((IsoTriangle(30, 40) -~ Pen.fill(C.LightGreen) -&
             (Line(30) -%  0 -~ measurePen -+ (0, 25)) -&
             (Line(40) -% 90 -~ measurePen -+ (-20, 0))
             -& centerX
             -% 105 -% -15).boundingBoxShape -~ bboxPen),
        350, 410
      ).place(
        RightTriangle(30, 40) -~ Pen.fill(C.indigo) -&
        (Line(30) -%  0 -~ measurePen -+ (0, 25)) -&
        (Line(40) -% 90 -~ measurePen -+ (-20, 0))
        -* 3 -* .5
        -& centerX, 410, 285
      ).place(
        EquiTriangle(50) -~ Pen.fill(C.gold) -&
        (Line(50) -%  0 -~ measurePen -+ (0, 27.5)) -&
        (Line(50) -% 60 -~ measurePen -+ (17.5, 0)) -&
        (Line(50) -% -60-~ measurePen -+ (-17.5, 0))
        -& centerX, 400, 400
      ).place(
        Pentagon(50, 50, 35, 55) -~ Pen.dashed(C.orange, 10) -&
        (Line(50) -%  0 -~ measurePen -+ (0, -32.5)) -&
        (Line(50) -%  0 -~ measurePen -+ (0, 32.5)) -&
        (Line(35) -% 90 -~ measurePen -+ (-30, 10)) -&
        (Line(55) -% 90 -~ measurePen -+ (30, 0))
        -& centerX, 100, 170
      ).place(
        Pentagon(50, 50, 35, 55) -~ Pen.fill(C.purple) -:- "home-plate" -&
        (Line(50) -%  0 -~ measurePen -+ (0, -32.5)) -&
        (Line(50) -%  0 -~ measurePen -+ (0, 32.5)) -&
        (Line(35) -% 90 -~ measurePen -+ (-30, 10)) -&
        (Line(55) -% 90 -~ measurePen -+ (30, 0))
        -% 180 -# .5 -# 1.5
        -& centerX -+ (100, 250)
      ).place(
        FreeForm(freeFormVase).as("vase") -* (2, .75) -~ Pen.stroke(C.Black, 1)
        -& ((FreeForm(freeFormVase) -* (2, 0.75) -~
             Pen.stroke(C.Black, 1)).boundingBoxShape -~ bboxPen)
        -& centerX, 450, 300
      ).place(
        Pentagon(35, 50, 15, 40) -~ Pen.fill(checkerDiamondPat) -&
        (Line(35) -%  0 -~ measurePen -+ (0, -25)) -&
        (Line(50) -%  0 -~ measurePen -+ (0, 25)) -&
        (Line(15) -% 90 -~ measurePen -+ (-30, 12.5)) -&
        (Line(40) -% 90 -~ measurePen -+ (30, 0))
        -& centerX -+ (100, 400)
      ).place(
        Trapezoid(50, 70, 40) using Pen.fill(C.green) combo
        (Line(50) -%  0 -~ measurePen -+ (0, -25)) -&
        (Line(70) -%  0 -~ measurePen -+ (0, 25)) -&
        (Line(40) -% 90 -~ measurePen -+ (-40, 0))
        -& centerX, 100, 330
      ).place(Writing(trialText), 500, 100).
      place(
        Line.between(Point(15, 5), Point(-20, -30)) -&
        (Path.from_@(15, 5).line_@(-20, -30) -+ (6, 0)),
        470, 150
      ).place(
        Line.between(Point(-5, 10), Point(-5, -15)) -&
        (Path.from_@(-5, 10).line_@(-5, -15) -+ (6, 0)),
        470, 160
      ).place(
        Line.between(Point(-5, -15), Point(-5, 10)) -&
        (Path.from_@(-5, -15).line_@(-5, 10) -+ (6, 0)),
        500, 150
      ).place(
        Line.between(Point(10, 15), Point(-20, 15)) -&
        (Path.from_@(10, 15).line_@(-20, 15) -+ (0, 6)),
        470, 160
      ).place(
        Line.between(Point(-20, 15), Point(10, 15)) -&
        (Path.from_@(-20, 15).line_@(10, 15) -+ (0, 6)),
        500, 150
      ).place(
        Line.between(Point(-20, -30), Point(15, 5)) -&
        (Path.from_@(-20, -30).line_@(15, 5) -+ (6, 0)),
        510, 150
      ).place(
        Line.between(Point(-2, 10), Point(15, -10)) -&
        (Path.from_@(-2, 10).line_@(15, -10) -+ (6, 0)),
        470, 120
      ).place(
        Line.between(Point(15, -10), Point(-2, 10)) -&
        (Path.from_@(15, -10).line_@(-2, 10) -+ (6, 0)),
        510, 120
      ).place(
        "ellipse_grid" -: (
            Ellipse(40, 30) -~ gridFillPen.stroke(C.blue) -& centerX
          ), 600, 100
      ).place(
          Ellipse(40, 30) -~ gridFillPen.stroke(C.Blue)
          -& (Ellipse(40, 30).boundingBoxShape -~ gridFillPen.stroke(C.Green))
          -/- 45
          -& ((Ellipse(40, 30) -/- 45).boundingBoxShape -~ bboxPen)
          -& centerX, 605, 170
      ).place(
          Ellipse(40, 30) -~ gridFillPen.stroke(C.blue)
          -& (Ellipse(40, 30).boundingBoxShape -~ gridFillPen.stroke(C.green))
          -/| 45
          -& ((Ellipse(40, 30) -/| 45).boundingBoxShape -~ bboxPen)
          -& centerX, 605, 280
      ).place(exampleScene -& (FreeForm(keyhole) -* 2) -& centerX, 590, 390).
      place(
          exampleScene -<> (FreeForm(keyhole) -* 2 -~ Pen(C.Black, C.blue))
          -& centerX, 640, 390
      ).place(
          exampleScene -<#> (FreeForm(keyhole) -* 2 -~ Pen(C.white,C.royalBlue))
          -& centerX, 685, 390
      )

    assertTrue(
        "unable to render to file '" + testFilepath + "'",
        SvgRenderer.render(measurementsCanvas, testFilepath, true)
      )
  }
}
