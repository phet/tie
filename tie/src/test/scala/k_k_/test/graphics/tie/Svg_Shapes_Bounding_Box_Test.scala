/*
   file: k_k_/test/graphics/tie/Svg_Shapes_Bounding_Box_Test.scala

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
import k_k_.graphics.tie.ink._
import k_k_.graphics.tie.ink.{NamedColors => C, _}
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.path._
import k_k_.graphics.tie.shapes.text._


@Test
class SvgShapesBoundingBoxTest extends SvgShapesTest {

  override val filename = "test_shapes_bounding_box.svg"

  override val title = "Bounding-Boxed Pre-Formulated Shapes"


  val bboxPen = Pen.dashed(C.Pink, 1.5, 10, 0)

  val centerPen = Pen.stroke(C.Blue, 0.4)
  val centerX =
      (Line(10) -%  45) -&
      (Line(10) -% -45) -~
      centerPen

  override
  protected def labelShape(shape: Shape, name: String, ink: Ink): Shape = {
    super.labelShape(shape, name, ink) -&
        (shape.boundingBox.asShape -~ bboxPen) -&
        centerX
  }
}
