/*
   file: k_k_/test/graphics/tie/ink/Svg_Venn_Diagram_Opacity_Test.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2013 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.test.graphics.tie

package ink {

import org.junit._

import k_k_.graphics.tie._
import k_k_.graphics.tie.ink._
import k_k_.graphics.tie.ink.Color._
import k_k_.graphics.tie.ink.palette._
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.text._


@Test
class SvgVennDiagramOpacityTest extends SvgTestBase {

  val filename = "test_venn_diagram_opacity.svg"

  val title = "Tri-Color Venn Diagram Showing Opacity"


  val colors = new ColorPaletteFromColorNameSeq {
    override protected val colorNameSeq = Seq("red", "green", "blue")
  }.colors


  protected def createCanvas() = {
    val ellipseCtor = DiamEllipse(_, _)
    val hexCtor = (w: Double, h: Double) => Hexagon(.6*h, h, w) -% (90)
    val octCtor = (w: Double, h: Double) => Octagon(.4*w, w, .5*h, h)
    val pentCtor = (w: Double, h: Double) => Pentagon(.7*w, w, .6*h, h)
    val trapCtor = (w: Double, h: Double) => Trapezoid(.4*w, w, h)

    val (eachW, eachH) = (200, 160)

    val venn1 = drawVennDiagram(pentCtor)(eachW, eachH)(colors, .15)
    val venn2 = drawVennDiagram(hexCtor)(eachW, eachH)(colors, .25)
    val venn3 = drawVennDiagram(ellipseCtor)(eachW, eachH)(colors, .5)
    val venn4 = drawVennDiagram(octCtor)(eachW, eachH)(colors, .75)
    val venn5 = drawVennDiagram(Rectangle(_, _))(eachW, eachH)(colors, .85)
    val venn6 = drawVennDiagram(trapCtor)(eachW, eachH)(colors, 2)

    new Canvas(
        CanvasProps(700, 450, title = title),
        ((labelShape(venn1, "opacity = .15") -+ (-(eachW + 15), 0)) -&
         (labelShape(venn2, "opacity = .25")) -&
         (labelShape(venn3, "opacity = .5")  -+ (  eachW + 15,  0))
         -+ (0, -(eachH/2 + 20))
         ) -&
        ((labelShape(venn4, "opacity = .75") -+ (-(eachW + 15), 0)) -&
         (labelShape(venn5, "opacity = .85")) -&
         (labelShape(venn6, "opacity = 1.0") -+ (  eachW + 15,  0))
         -+ (0, (eachH/2 + 20))
         )
      )
  }

  // triple-overlapping venn diagram
  def drawVennDiagram(
      S: (Double, Double) => Shape
    )(
      w: Double, h: Double
    )(
      colors: Seq[Color], opacity: Double
    ): Shape = {
    val eachShapeW = w * 2/3
    val eachShapeH = h * 2/3
    val diagramColors = colors.take(3)
    val coloredShapes = diagramColors.map { _ -# opacity }.map {
      S(eachShapeW, eachShapeH) -~ Pen.fill(_)
    }
    val (xShift, yShift) = 
        (w/2 - w/3,
         h/2 - h/3)
    (coloredShapes(0) -+ (      0, -yShift)) -&
    (coloredShapes(1) -+ (-xShift,  yShift)) -&
    (coloredShapes(2) -+ ( xShift,  yShift))
  }
}

}
