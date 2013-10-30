/*
   file: k_k_/test/graphics/tie/Svg_Sierpinski_Triangle_Test.scala

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

import scala.util.Random

import k_k_.graphics.tie._
import k_k_.graphics.tie.ink._
import k_k_.graphics.tie.ink.Color._    
import k_k_.graphics.tie.ink.palette._
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.text._


@Test
class SvgSierpinskiTriangleTest extends SvgTestBase {

  val filename = "test_sierpinski_triangle.svg"

  val title = "Sierpinski Triangle Iterations"


  val colors = AllNamedColors.colors

  protected def createCanvas() = {
    val (triW, triH) = (200, 180)
    val origTri: Shape = IsoTriangle(triW, triH)

    val noopPen = (s: Shape, iterNum: Int, triNum: Int) => s

    val randFillPen = {
      val rand = new Random
      def randInt = rand.nextInt(colors.length)
      def randColor = colors(randInt)

      (s: Shape, iterNum: Int, triNum: Int) => s -~ Pen.fill(randColor)
    }

    /*
      NOTE: the scanning fold below is equivalent to the following:

      iterations(0) = origTri
      iterations(1) = sierpinskify(iterations(0),    1, randFillPen)
      iterations(2) = sierpinskify(iterations(1),    2, randFillPen)
      iterations(3) = sierpinskify(iterations(2),    3, randFillPen)
      iterations(4) = sierpinskify(iterations(3),    4, randFillPen)
      iterations(5) = sierpinskify(iterations(4),    5, randFillPen)
      iterations(6) = sierpinskify(iterations(5),    6, randFillPen)
    */
    val iterations = ((1 to 6) scanLeft origTri) {
      sierpinskify(_, _, randFillPen)
    }

    new Canvas(
        CanvasProps(700, 450, title = title),
        ((labelShape(iterations(0), "original") -+ (-(triW + 15), 0)) -&
             (labelShape(iterations(1), "first iteration")) -&
             (labelShape(iterations(2), "second iteration") -+ ((triW + 15), 0))
             -+ (0, -(triH/2 + 20))
        ) -&
        ((labelShape(iterations(3), "third iteration") -+ (-(triW + 15), 0)) -&
             (labelShape(iterations(4), "fourth iteration")) -&
             (labelShape(iterations(5), "fifth iteration") -+ (triW + 15, 0))
             -+ (0, (triH/2 + 20))
        )
    )
  }

  // 'sierpinskify' every IsoTriangle found within, mapping each component of
  // result with `inkFunc(_, iteration, n)`, for n = {0,1,2} counted TB-LR
  def sierpinskify(
      s: Shape, iteration: Int, inkFunc: (Shape, Int, Int) => Shape
    ): Shape = {

    def sierpinskiRewrite(isoTri: IsoTriangle): Shape = {
      val IsoTriangle(baseWidth, height) = isoTri
      val halfTri = IsoTriangle(baseWidth/2, height/2)
      (inkFunc(halfTri, iteration, 0) -+ ( 0,           -height/4)) -&
      (inkFunc(halfTri, iteration, 1) -+ (-baseWidth/4, height/4)) -&
      (inkFunc(halfTri, iteration, 2) -+ ( baseWidth/4, height/4))
    }

    s mappedWhen { case iso @ IsoTriangle(_, _) => sierpinskiRewrite(iso) }
  }
}


/*
    orig. impl. (which inspired ShapeOp trait hierarchy:

  def sierpinskify(
      s: Shape, iteration: Int, inkFunc: (Shape, Int, Int) => Shape
    ): Shape = {

    def sierpinskifyShape(s: Shape): Shape = {

      def sierpinskiRewrite(isoTri: IsoTriangle): Shape = {
        val IsoTriangle(baseWidth, height) = isoTri
        val halfTri = IsoTriangle(baseWidth/2, height/2)
        (inkFunc(halfTri, iteration, 0) -+ ( 0,           -height/4)) -&
        (inkFunc(halfTri, iteration, 1) -+ (-baseWidth/4, height/4)) -&
        (inkFunc(halfTri, iteration, 2) -+ ( baseWidth/4, height/4))
      }

      s match {
        case TranslatedShape(shape, xMove, yMove) =>
          TranslatedShape(sierpinskifyShape(shape), xMove, yMove)
        case ScaledShape(shape, xScaling, yScaling) =>
          ScaledShape(sierpinskifyShape(shape), xScaling, yScaling)
        case RotatedShape(shape, degrees, xPivot, yPivot) =>
          RotatedShape(sierpinskifyShape(shape), degrees, xPivot, yPivot)
        case CompositeShape(below, above) =>
          CompositeShape(sierpinskifyShape(below), sierpinskifyShape(above))
        case InkedShape(shape, pen) =>
          InkedShape(sierpinskifyShape(shape), pen)
        case isoTri @ IsoTriangle(_, _) =>
          sierpinskiRewrite(isoTri)
        case x @ ( _ : Line      | _ : Hemisphere    | _ : RightTriangle |
                   _ : Rectangle | _ : Parallelogram | _ : Trapezoid     |
                   _ : Pentagon  | _ : Hexagon       | _ : Octagon       |
                   _ : Ellipse   | _ : FreeForm      | _ : Writing ) =>  x
      }
    }

    sierpinskifyShape(s)
  }
*/
