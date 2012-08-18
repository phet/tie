/*
   file: k_k_/test/graphics/tie/Svg_Render_Test.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
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
import k_k_.graphics.tie.fmt.svg.Svg_Renderer
import k_k_.graphics.tie.ink.{Named_Colors => C, _}
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.path._
import k_k_.graphics.tie.shapes.text._


@Test
class Svg_Renderer_Test extends JUnitSuite {

  @Test
  def test_Rendering() {
    val test_out_dir = "target/test-classes/rendered_out"
    val test_filename = "test_rendering.svg"

    val test_filepath = test_out_dir + "/" + test_filename
    val out_file = new File(test_filepath)
    assertTrue(out_file.getParentFile.isDirectory ||
               out_file.getParentFile.mkdirs)

    /*
       prev attempts at 3d cylinder, as rendered into SVG:

       <!--   <path d="M0,0 A 50 10 0 1 0 -1,0 z m -50,-10 v60 a 50 10 0 0 0 100,0 v-60" /> -->
       <path d="M0,0 A 50 10 0 1 0 100,0 a 50 10 0 1 0 -100, 0 z v60 a 50 10 0 0 0 100,0 v-60" />
    */

    val free_form_3d_cylinder = Path.from_@(0, 0).
                                arc_@(50, 10, Large_CCW, 100, 0).
                                arc  (50, 10, Large_CCW, -100, 0).
                                // close.
                                vertical (60).
                                arc  (50, 10, Small_CCW, 100, 0).
                                vertical (-60)
    val free_form_vase = Path.from(0, 160).
                         quadratic(-30, -60, 0, -120).
                         quadratic(10, -20, 0, -40).
                         horizontal(40).
                         quadratic(-10, 20, 0, 40).
                         quadratic(30, 60, 0, 120).
                         close

    val keyhole =
      Path.arc(10, 10, Large_CW, 10, 0).
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

    val red_yellow_grad = new Linear_Gradient(Color_Stop(C.red, 20) ::
                                              Color_Stop(C.yellow, 80) ::
                                              Nil)

    val checker_diamond_pat = Pattern(Path.from(0, 5).
                                      line(5, 5).
                                      line(5, -5).
                                      line(-5, -5).
                                      close -~ Pen(C.Black, C.Black, 1))

    val grid_fill_pen =
          Pen.fill(Pattern(Rectangle(20, 20) -+ (10) -~
                             Pen.stroke(C.Black, .1)))

    val example_scene =
          (Rectangle(45, 70) -~ Pen.fill(C.yellow) -+ (0, -3)) -&
          (Right_Triangle(30, 40) -~ Pen.fill(C.indigo)   -+ (-15, -19)) -&
          (Circle(20) -~ Pen.fill(red_yellow_grad -/- 45) -+ (12, 15))

/*
    val canvas = new Canvas(Canvas_Props(500, 500, Origin_Top_Left),
                            Rectangle(80, 50).using(Pen.fill(C.red)) -+ 100,
                            Ellipse(40, 30) -+ (200,100),
                            Parallelogram(40, 60, 40) -+ (300, 100)
                              -~ Pen.fill(C.brown),
                            Ellipse(40, 30) -+ (400,100) -~
                              Pen.fill(red_yellow_grad -% 90),
                            Octagon(60, 100, 30, 70) -~
                              Pen.fill(red_yellow_grad) -+ 200,
                            Octagon(40, 80, 40, 80) -~
                              Pen(C.red, C.yellow) -+ (350, 200),
                            Square(50) -% 45 -~
                              Pen.fill(red_yellow_grad) -+ 350,
                            Free_Form(free_form_3d_cylinder) -~
                              Pen(C.Black, red_yellow_grad -% 60) -+
                              (450, 200),
                            Line(50) -% 45 -+ 300 -~ Pen.stroke(C.green, 2.5)).
                       place(Hexagon(50, 80, 50) -% 90 -* (2, 1.5) -~
                             Pen.fill(C.blue), 200, 350).
                       place(Pentagon(50, 50, 35, 55) -~
                             Pen.dashed(C.orange, 10), 100, 180).
                       place(Pentagon(50, 50, 35, 55) -~ Pen.fill(C.Purple)
                             -% 180 -+ (100, 250)).
                       place(Free_Form(free_form_vase) -* (2, 0.75) -~
                             Pen.stroke(C.Black, 1), 450, 300).
                       place(Pentagon(35, 50, 15, 40) -+ (100, 400)).
                       place(Trapezoid(60, 80, 50) using
                             Pen.fill(C.green), 100, 350)
*/


    val arial_font = Font("Arial", Std_Size(12))
    // val trial_text = Text_Block(Text_Line("testing 1, 2, 3", name_font))
    val trial_text = Text_Line("testing 1, 2, 3", arial_font, Middle_Align)


    val center_pen = Pen.stroke(C.gray, 0.4)
    val center_X = (Line(10) -%  45) -&
                   (Line(10) -% -45) -~ center_pen
    val measure_pen = Pen.stroke(C.Brown, 2)

    val bbox_pen = Pen.dashed(C.pink, 0.6, 10, 0)

    val measurements_canvas =
                 new Canvas(Canvas_Props(710, 500, Origin_Top_Left),
                            Rectangle(80, 50).using(Pen.fill(C.red))
                            -:- (Id_Attribution("rect"),
                                 Link_Attribution("http://scala-lang.org"))
                            -&
                              (Line(80) -%  0 -~ measure_pen -+ (0, -30)) -& 
                              (Line(50) -% 90 -~ measure_pen -+ (-45, 0))
                            -& center_X
                              -+ 100,
//??????why isn't this possible????:
// NOTE: there are no 'default argument' methods in Presentable_Shape, merely:
//  def -:(attribution: Attribution): T =
//    as(attribution)
//
//  def -:(attribution1: Attribution, attribution2: Attribution): T =
//    as(attribution1).as(attribution2)
//
//  def -:(id: String): T =
//    as(Id_Attribution(id))
//
//[ERROR] ../tie/tie/src/test/scala/k_k_/test/graphics/tie/Svg_Render_Test.scala:143: error: overloaded method value -: with alternatives:
//[INFO]   (id: String)k_k_.graphics.tie.shapes.Drawing_Shape <and>
//[INFO]   (attribution: k_k_.graphics.tie.Attribution)k_k_.graphics.tie.shapes.Drawing_Shape
//[INFO]  cannot be applied to ((k_k_.graphics.tie.Id_Attribution, k_k_.graphics.tie.Link_Attribution))
//[ERROR] Error occurred in an application involving default arguments.
//[INFO]                             (Ellipse(40, 30) -&
//[INFO]                             ^
//                          (Id_Attribution("measured"),
//                           Link_Attribution("http://scala-lang.org",
//                                            Target_Blank)) -:
                            Id_Attribution("measured") -:
                            Link_Attribution("http://scala-lang.org",
                                             Target_Blank) -:
                            (Ellipse(40, 30) -&
                               (Line(40) -%  0 -~ measure_pen -+ (-20, -35)) -&
                               (Line(30) -% 90 -~ measure_pen -+ (-45, -15)) -&
                               (Line(40) -%  0 -~ measure_pen -+ (-20, 0)) -&
                               (Line(30) -% 90 -~ measure_pen -+ (0, -15))
                             -& center_X
                             -& (Ellipse(40, 30).
                                   bounding_box.as_shape -~ bbox_pen)
                               -+ 100 -+ (100,0)),
                            Parallelogram(40, 60, 40) -~ Pen.fill(C.brown) -&
                              (Line(40) -%  0 -~ measure_pen -+ (10, -25)) -&
                              (Line(60) -%  0 -~ measure_pen -+ (0, 25)) -&
                              (Line(40) -% 90 -~ measure_pen -+ (-35, 0))
                            -& center_X
                            -& ((Parallelogram(40, 60, 40)-~ Pen.fill(C.brown)).
                                  bounding_box.as_shape -~ bbox_pen)
                              -+ 300 -+ (0, -200),
                            ("feds" -: Link("http://us.gov") -: Ellipse(40, 30))
                               -+ (400,100) -~
                               Pen.fill(red_yellow_grad -% 90),
                            ("I||eg@L-id" -: Octagon(60, 100, 30, 70)) -~
                              Pen.fill(red_yellow_grad) -&
                              (Line(60) -%  0 -~ measure_pen -+ (0, -40)) -&
                              (Line(100)-%  0 -~ measure_pen -+ (0, 40)) -&
                              (Line(30) -% 90 -~ measure_pen -+ (-55, 0)) -&
                              (Line(70) -% 90 -~ measure_pen -+ (55, 0))
                            -& center_X
                              -+ 200,
                            Octagon(40, 80, 40, 80) -~ Pen(C.red, C.yellow) -&
                              (Line(40) -%  0 -~ measure_pen -+ (0, -45)) -&
                              (Line(80) -%  0 -~ measure_pen -+ (0, 45)) -&
                              (Line(40) -% 90 -~ measure_pen -+ (-45, 0)) -&
                              (Line(80) -% 90 -~ measure_pen -+ (45, 0))
                            -& center_X
                            -& ((Octagon(40, 80, 40, 80) -~
                                    Pen(C.red, C.yellow)).
                                  bounding_box.as_shape -~ bbox_pen)
                            -+ (350, 200),
                            Square(50) -~
                              Pen.fill(red_yellow_grad) -&
                              (Line(50) -%  0 -~ measure_pen -+ (0, -30)) -&
                              (Line(50) -% 70 -% 20 -~ measure_pen -+ (-30, 0))
                            -& center_X
                              -% 45 
                            -& ((Square(50)-~ Pen.fill(red_yellow_grad)-% 45).
                                  bounding_box.as_shape -~ bbox_pen)
                            -+ 350,
                            Free_Form(free_form_3d_cylinder) -:-
                                Link("http://www.wikipedia.org") -* .5 -* 2 -~
                              Pen(C.Black, red_yellow_grad -% 80 -% -20)
                            -& ((Free_Form(free_form_3d_cylinder) -* .5 -* 2 -~
                              Pen(C.Black, red_yellow_grad -% 80 -% -20)).
                                   bounding_box.as_shape -~ bbox_pen)
                            -& center_X
                              -+ (440, 200),
                            Line(50) -~ Pen.stroke(C.Green, 2.5) -&
                              (Line(50) -%  0 -~ measure_pen -+ (0, -5))
                            -& center_X
                              -% 45 -+ 300).
                       place(Hexagon(50, 80, 50) -~ Pen.fill(C.blue) -&
                               (Line(50) -%  0 -~ measure_pen -+ (0, -28)) -&
                               (Line(80) -%  0 -~ measure_pen -+ (0, 28)) -&
                               (Line(50) -% 90 -~ measure_pen -+ (-42.5, 0))
                               -% 90 -# .5
                               -* (2, 1.5)
                             -& center_X, 200, 350).
                       place(Iso_Triangle(30, 40) -~ Pen.fill(C.silver) -&
                               (Line(30) -%  0 -~ measure_pen -+ (0, 25)) -&
                               (Line(40) -% 90 -~ measure_pen -+ (-20, 0))
                             -& center_X, 300, 400).
                       place(Iso_Triangle(30, 40) -~ Pen.fill(C.Light_Green) -&
                               (Line(30) -%  0 -~ measure_pen -+ (0, 25)) -&
                               (Line(40) -% 90 -~ measure_pen -+ (-20, 0))
                             -& center_X
                             -% 105 -% -15
                             -& ((Iso_Triangle(30, 40) -~
                                    Pen.fill(C.LightGreen) -&
                                 (Line(30) -%  0 -~ measure_pen -+ (0, 25)) -&
                                 (Line(40) -% 90 -~ measure_pen -+ (-20, 0))
                                 -& center_X
                                 -% 105 -% -15).
                                   bounding_box.as_shape -~ bbox_pen)
                             , 350, 410).
                       place(Right_Triangle(30, 40) -~ Pen.fill(C.indigo) -&
                               (Line(30) -%  0 -~ measure_pen -+ (0, 25)) -&
                               (Line(40) -% 90 -~ measure_pen -+ (-20, 0))
                               -* 3 -* .5
                             -& center_X, 410, 285).
                       place(Equi_Triangle(50) -~ Pen.fill(C.gold) -&
                               (Line(50) -%  0 -~ measure_pen -+ (0, 27.5)) -&
                               (Line(50) -% 60 -~ measure_pen -+ (17.5, 0)) -&
                               (Line(50) -% -60-~ measure_pen -+ (-17.5, 0))
                             -& center_X, 400, 400).
                       place(Pentagon(50, 50, 35, 55) -~
                               Pen.dashed(C.orange, 10) -&
                               (Line(50) -%  0 -~ measure_pen -+ (0, -32.5)) -&
                               (Line(50) -%  0 -~ measure_pen -+ (0, 32.5)) -&
                               (Line(35) -% 90 -~ measure_pen -+ (-30, 10)) -&
                               (Line(55) -% 90 -~ measure_pen -+ (30, 0))
                             -& center_X,
                             100, 170).
                       place(Pentagon(50, 50, 35, 55) -~ Pen.fill(C.purple)
                               -:- "home-plate" -&
                               (Line(50) -%  0 -~ measure_pen -+ (0, -32.5)) -&
                               (Line(50) -%  0 -~ measure_pen -+ (0, 32.5)) -&
                               (Line(35) -% 90 -~ measure_pen -+ (-30, 10)) -&
                               (Line(55) -% 90 -~ measure_pen -+ (30, 0))
                               -% 180 -# .5 -# 1.5
                             -& center_X
                             -+ (100, 250)).
                       place(Free_Form(free_form_vase).as("vase") -* (2, .75) -~
                             Pen.stroke(C.Black, 1)
                            -& ((Free_Form(free_form_vase) -* (2, 0.75) -~
                                 Pen.stroke(C.Black, 1)).
                                   bounding_box.as_shape -~ bbox_pen)
                             -& center_X, 450, 300).
                       place(Pentagon(35, 50, 15, 40) -~
                             Pen.fill(checker_diamond_pat) -&
                               (Line(35) -%  0 -~ measure_pen -+ (0, -25)) -&
                               (Line(50) -%  0 -~ measure_pen -+ (0, 25)) -&
                               (Line(15) -% 90 -~ measure_pen -+ (-30, 12.5)) -&
                               (Line(40) -% 90 -~ measure_pen -+ (30, 0))
                             -& center_X
                              -+ (100, 400)).
                       place(Trapezoid(50, 70, 40) using Pen.fill(C.green)combo
                               (Line(50) -%  0 -~ measure_pen -+ (0, -25)) -&
                               (Line(70) -%  0 -~ measure_pen -+ (0, 25)) -&
                               (Line(40) -% 90 -~ measure_pen -+ (-40, 0))
                             -& center_X,
                             100, 330).
                       place(Writing(trial_text), 500, 100).
                       place(Line.between(Point(15, 5), Point(-20, -30)) -&
                             (Path.from_@(15, 5).line_@(-20, -30) -+ (6, 0)),
                             470, 150).
                       place(Line.between(Point(-5, 10), Point(-5, -15)) -&
                             (Path.from_@(-5, 10).line_@(-5, -15) -+ (6, 0)),
                             470, 160).
                       place(Line.between(Point(-5, -15), Point(-5, 10)) -&
                             (Path.from_@(-5, -15).line_@(-5, 10) -+ (6, 0)),
                             500, 150).
                       place(Line.between(Point(10, 15), Point(-20, 15)) -&
                             (Path.from_@(10, 15).line_@(-20, 15) -+ (0, 6)),
                             470, 160).
                       place(Line.between(Point(-20, 15), Point(10, 15)) -&
                             (Path.from_@(-20, 15).line_@(10, 15) -+ (0, 6)),
                             500, 150).
                       place(Line.between(Point(-20, -30), Point(15, 5)) -&
                             (Path.from_@(-20, -30).line_@(15, 5) -+ (6, 0)),
                             510, 150).
                       place(Line.between(Point(-2, 10), Point(15, -10)) -&
                             (Path.from_@(-2, 10).line_@(15, -10) -+ (6, 0)),
                             470, 120).
                       place(Line.between(Point(15, -10), Point(-2, 10)) -&
                             (Path.from_@(15, -10).line_@(-2, 10) -+ (6, 0)),
                             510, 120).
                       place("ellipse_grid" -:
                             (Ellipse(40, 30) -~ grid_fill_pen.stroke(C.blue)
                              -& center_X), 600, 100).
                       place(Ellipse(40, 30) -~ grid_fill_pen.stroke(C.Blue)
                             -& (Ellipse(40, 30).bounding_box.as_shape
                                 -~ grid_fill_pen.stroke(C.Green))
                             -/- 45
                             -& ((Ellipse(40, 30)
                                  -/- 45).
                                  bounding_box.as_shape -~ bbox_pen)
                             -& center_X, 605, 170).
                       place(Ellipse(40, 30) -~ grid_fill_pen.stroke(C.blue)
                             -& (Ellipse(40, 30).bounding_box.as_shape
                                 -~ grid_fill_pen.stroke(C.green))
                             -/| 45
                             -& ((Ellipse(40, 30)
                                  -/| 45).
                                  bounding_box.as_shape -~ bbox_pen)
                             -& center_X, 605, 280).
                       place(example_scene -& (Free_Form(keyhole) -* 2)
                             -& center_X, 590, 390).
                       place(example_scene -<> (Free_Form(keyhole) -* 2
                                                 -~ Pen(C.Black, C.blue))
                             -& center_X, 640, 390).
                       place(example_scene -<#> (Free_Form(keyhole) -* 2
                                                  -~ Pen(C.white, C.royal_blue))
                             -& center_X, 685, 390)

    assertTrue("unable to render to file '" + test_filepath + "'",
               Svg_Renderer.render(measurements_canvas, test_filepath, true))
  }
}
