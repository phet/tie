/*
   file: k_k_/test/graphics/tie/Svg_Test_Base.scala

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
import org.scalatest.junit.JUnitSuite

import java.io.File

import k_k_.graphics.tie._
import k_k_.graphics.tie.fmt.svg.Svg_Renderer
import k_k_.graphics.tie.ink.{Named_Colors => C, _}
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.text._


abstract class Svg_Test_Base extends JUnitSuite {

  val filename: String

  val out_dir = "target/test-classes/rendered_out"


  val title: String

  val desc_font = new Font("Arial", 12, weight = Bold)

  val renderer: Renderer = Svg_Renderer


  @Test
  def test() {
    val filepath = out_dir + "/" + filename
    val out_file = new File(filepath)
    assertTrue(out_file.getParentFile.isDirectory ||
               out_file.getParentFile.mkdirs)

    val shapes_canvas = create_canvas()

    assertTrue("unable to render to file '" + out_file.getPath + "'",
               renderer.render(shapes_canvas, out_file, true))
  }

  protected def create_canvas(): Canvas

  protected def label_shape(shape: Drawing_Shape, name: String, ink: Ink):
      Drawing_Shape = {
      
    val name_offset_y = shape.bounding_box.height / 2 + 10

//    val bbox_pen = Pen.dashed(C.Pink, 0.6, 10, 0)

    (shape -~ Pen.stroke(ink)) -&
    (write(name, ink)

//-& (write(name, ink).bounding_box.as_drawing_shape -~ bbox_pen)

 -+ (0, name_offset_y))

  }

  protected def label_shape(shape: Drawing_Shape, name: String): Drawing_Shape =
    label_shape(shape, name, C.Black)

  protected def write(text: String, ink: Ink,
                      align: Text_Align = Middle_Align) =
    Text_Line(text, desc_font, align) -~ Pen.fill(ink)
}
