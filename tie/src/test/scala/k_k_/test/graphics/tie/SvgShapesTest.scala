/*
   file: k_k_/test/graphics/tie/Svg_Shapes_Test.scala

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
import k_k_.graphics.tie.shapes.text._


@Test
class SvgShapesTest extends SvgTestBase {

  val filename = "test_shapes.svg"

  val title = "Pre-Formulated Shapes"


  protected def createCanvas() = new Canvas(
      new CanvasProps(600, 450, OriginPos.TopLeft, title),
      (labelShape(Line(50),                  "line")
           -+ (0, 30)) -&
      (labelShape(Hemisphere(35, 25),        "hemisphere")
           -+ (0, 70)) -&
      (labelShape(IsoTriangle(35, 50),       "isoTriangle")
           -+ (0, 150)) -&
      (labelShape(RightTriangle(40, 50),     "rightTriangle")
           -+ (0, 250)) -&
      (labelShape(EquiTriangle(60),          "equiTriangle",   C.gray)
           -+ (0, 350))
      -+ (50, 0),

      (labelShape(Rectangle(80, 50),         "rectangle")
           -+ (0, 50)) -&
      (labelShape(Parallelogram(50, 70, 50), "parallelogram")
           -+ (0, 150)) -&
      (labelShape(Trapezoid(55, 80, 50),     "trapezoid")
           -+ (0, 250)) -&
      (labelShape(Square(60),                "square",         C.gray)
           -+ (0, 350))
      -+ (150, 0),

      (labelShape(Pentagon(50, 50, 35, 55),  "pentagon")
           -+ (0, 50)) -&
      (labelShape(Hexagon(40, 70, 50),       "hexagon")
           -+ (0, 150)) -&
      (labelShape(Octagon(35, 70, 20, 50),   "octagon")
           -+ (0, 250)) -&
      (labelShape(RegPentagon(60),           "regPentagon",    C.gray)
           -+ (0, 350))
      -+ (250, 0),

      (labelShape(Pentagon(40, 60, 20, 50),  "{pentagon}")
           -+ (0, 50)) -&
      (labelShape(InvisRectangle(80, 50),    "invisRectangle", C.gray)
           -+ (0, 150)) -&
      (labelShape(RegHexagon(60),            "regHexagon",     C.gray)
           -+ (0, 350))
      -+ (350, 0),

      (labelShape(Ellipse(35, 25),           "ellipse")
           -+ (0, 50)) -&
      (labelShape(Circle(25),                "circle",         C.gray)
           -+ (0, 150)) -&
      (labelShape(RegOctagon(60),            "regOctagon",     C.gray)
           -+ (0, 350))
      -+ (450, 0),

      (labelShape(DiamEllipse(35, 25),       "diamEllipse",    C.gray)
           -+ (0, 50)  -+ (0, 12.5)) -&
      (labelShape(DiamCircle(25),            "diamCircle",     C.gray)
           -+ (0, 150) -+ (0, 12.5))
      -+ (550, 0)
    )
}
