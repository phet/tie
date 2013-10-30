/*
   file: k_k_/test/graphics/tie/Svg_Test_Base.scala

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
import org.scalatest.junit.JUnitSuite

import java.io.File

import k_k_.graphics.tie._
import k_k_.graphics.tie.fmt.svg.SvgRenderer
import k_k_.graphics.tie.ink.{NamedColors => C, _}
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.text._


abstract class SvgTestBase extends JUnitSuite {

  val filename: String

  val outDir = "target/test-classes/rendered_out"


  val title: String

  val descFont = new Font("Arial", 12, weight = FontWeight.Bold)

  val renderer: Renderer = SvgRenderer


  @Test
  def test() {
    val filepath = outDir + "/" + filename
    val outFile = new File(filepath)
    assertTrue(outFile.getParentFile.isDirectory ||outFile.getParentFile.mkdirs)

    val shapesCanvas = createCanvas()

    assertTrue(
        "unable to render to file '" + outFile.getPath + "'",
        renderer.render(shapesCanvas, outFile, true)
      )
  }

  protected def createCanvas(): Canvas

  protected def labelShape(shape: Shape, name: String, ink: Ink): Shape = {
      
    val nameOffsetY = shape.boundingBox.height / 2 + 10

//    val bboxPen = Pen.dashed(C.Pink, 0.6, 10, 0)

    (shape -~ Pen.stroke(ink)) -&
    (write(name, ink)

//-& (write(name, ink).boundingBox.asDrawingShape -~ bboxPen)

     -+ (0, nameOffsetY))

  }

  protected def labelShape(shape: Shape, name: String): Shape =
    labelShape(shape, name, C.Black)

  protected def write(
      text: String, ink: Ink, align: TextAlign = TextAlign.Middle
    ) =
    TextLine(text, descFont, align) -~ Pen.fill(ink)
}
