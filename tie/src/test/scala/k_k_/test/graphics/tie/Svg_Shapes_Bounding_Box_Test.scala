/*
   file: k_k_/test/graphics/tie/Svg_Shapes_Bounding_Box_Test.scala

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
import k_k_.graphics.tie.ink._
import k_k_.graphics.tie.ink.{Named_Colors => C, _}
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.path._
import k_k_.graphics.tie.shapes.text._


@Test
class Svg_Shapes_Bounding_Box_Test extends Svg_Shapes_Test {

  override val filename = "test_shapes_bounding_box.svg"

  override val title = "Bounding-Boxed Pre-Formulated Shapes"


  val bbox_pen = Pen.dashed(C.Pink, 1.5, 10, 0)

  val center_pen = Pen.stroke(C.Blue, 0.4)
  val center_X = (Line(10) -%  45) -&
                 (Line(10) -% -45) -~ center_pen

  override
  protected def label_shape(shape: Drawing_Shape, name: String, ink: Ink):
      Drawing_Shape = {
    super.label_shape(shape, name, ink) -&
    (shape.bounding_box.as_drawing_shape -~ bbox_pen) -&
    center_X
  }
}
