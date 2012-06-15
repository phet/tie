/*
   file: k_k_/test/graphics/tie/Svg_Shapes_Test.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
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
import k_k_.graphics.tie.shapes.text._


@Test
class Svg_Shapes_Test extends Svg_Test_Base {

  val filename = "test_shapes.svg"

  val title = "Pre-Formulated Shapes"


  protected def create_canvas() = {
    new Canvas(new Canvas_Props(600, 450, Origin_Top_Left, title),
               (label_shape(Line(50),                 "line")
                  -+ (0, 30)) -&
               (label_shape(Hemisphere(35, 25),       "hemisphere")
                  -+ (0, 70)) -&
               (label_shape(Iso_Triangle(35, 50),     "iso_triangle")
                  -+ (0, 150)) -&
               (label_shape(Right_Triangle(40, 50),   "right_triangle")
                  -+ (0, 250)) -&
               (label_shape(Equi_Triangle(60),        "equi_triangle",   C.gray)
                  -+ (0, 350))
               -+ (50, 0),

               (label_shape(Rectangle(80, 50),        "rectangle")
                  -+ (0, 50)) -&
               (label_shape(Parallelogram(50, 70, 50),"parallelogram")
                  -+ (0, 150)) -&
               (label_shape(Trapezoid(55, 80, 50),    "trapezoid")
                  -+ (0, 250)) -&
               (label_shape(Square(60),               "square",          C.gray)
                  -+ (0, 350))
               -+ (150, 0),

               (label_shape(Pentagon(50, 50, 35, 55), "pentagon")
                  -+ (0, 50)) -&
               (label_shape(Hexagon(40, 70, 50),      "hexagon")
                  -+ (0, 150)) -&
               (label_shape(Octagon(35, 70, 20, 50),  "octagon")
                  -+ (0, 250)) -&
               (label_shape(Reg_Pentagon(60),         "reg_pentagon",    C.gray)
                  -+ (0, 350))
               -+ (250, 0),

               (label_shape(Pentagon(40, 60, 20, 50), "{pentagon}")
                  -+ (0, 50)) -&
               (label_shape(Invis_Rectangle(80, 50),  "invis_rectangle", C.gray)
                  -+ (0, 150)) -&
               (label_shape(Reg_Hexagon(60),          "reg_hexagon",     C.gray)
                  -+ (0, 350))
               -+ (350, 0),

               (label_shape(Ellipse(35, 25),          "ellipse")
                  -+ (0, 50)) -&
               (label_shape(Circle(25),               "circle",          C.gray)
                  -+ (0, 150)) -&
               (label_shape(Reg_Octagon(60),          "reg_octagon",     C.gray)
                  -+ (0, 350))
               -+ (450, 0),

               (label_shape(Diam_Ellipse(35, 25),     "diam_ellipse",    C.gray)
                  -+ (0, 50)  -+ (0, 12.5)) -&
               (label_shape(Diam_Circle(25),          "diam_circle",     C.gray)
                  -+ (0, 150) -+ (0, 12.5))
               -+ (550, 0)
              )
  }
}
