/*
   file: k_k_/test/graphics/tie/Svg_Sierpinski_Triangle_Test.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2011 by Corbin "Kip" Kohn
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
class Svg_Sierpinski_Triangle_Test extends Svg_Test_Base {

  val filename = "test_sierpinski_triangle.svg"

  val title = "Sierpinski Triangle Iterations"


  val colors = All_Named_Colors.colors

  protected def create_canvas() = {
    val (tri_w, tri_h) = (200, 180)
    val orig_tri: Drawing_Shape = Iso_Triangle(tri_w, tri_h)

    val noop_pen_func = (a: Drawing_Shape, iter_num: Int, tri_num: Int) => a

    val rand_fill_pen_func = {
      val rand = new Random
      def rand_int = rand.nextInt(colors.length)
      def rand_color = colors(rand_int)

      (a: Drawing_Shape, iter_num: Int, tri_num: Int) =>
        a -~ Pen.fill(rand_color)
    }

    /*
      NOTE: the scanning fold below is equivalent to the following:

      iterations(0) = orig_tri
      iterations(1) = sierpinskify(orig_tri, 1, pen_func)
      iterations(2) = sierpinskify(iter1,    2, pen_func)
      iterations(3) = sierpinskify(iter2,    3, pen_func)
      iterations(4) = sierpinskify(iter3,    4, pen_func)
      iterations(5) = sierpinskify(iter4,    5, pen_func)
      iterations(6) = sierpinskify(iter5,    6, pen_func)
    */
    val iterations = ((1 to 6) scanLeft orig_tri) {
                       sierpinskify(_, _, rand_fill_pen_func)
                     }

    new Canvas(Canvas_Props(700, 450, title = title),
               ((label_shape(iterations(0), "original")
                   -+ (-(tri_w + 15), 0)) -&
                (label_shape(iterations(1), "first iteration")) -&
                (label_shape(iterations(2), "second iteration")
                   -+ ((tri_w + 15), 0))
                  -+ (0, -(tri_h/2 + 20))) -&

               ((label_shape(iterations(3), "third iteration")
                   -+ (-(tri_w + 15), 0)) -&
                (label_shape(iterations(4), "fourth iteration")) -&
                (label_shape(iterations(5), "fifth iteration")
                   -+ (tri_w + 15, 0))
                  -+ (0, (tri_h/2 + 20)))
              )
  }

  // 'sierpinskify' every Iso_Triangle found within, mapping each component of
  // result with `ink_func(_, iteration, n)`, for n = {0,1,2} counted TB-LR
  def sierpinskify(s: Drawing_Shape, iteration: Int,
                   ink_func: (Drawing_Shape, Int, Int) => Drawing_Shape):
      Drawing_Shape = {

    def sierpinski_rewrite(iso_tri: Iso_Triangle): Drawing_Shape = {
      val Iso_Triangle(base_width, height) = iso_tri
      val half_tri = Iso_Triangle(base_width/2, height/2)
      (ink_func(half_tri, iteration, 0) -+ ( 0,           -height/4)) -&
      (ink_func(half_tri, iteration, 1) -+ (-base_width/4, height/4)) -&
      (ink_func(half_tri, iteration, 2) -+ ( base_width/4, height/4))
    }

    s mapped_when { case iso @ Iso_Triangle(_, _) => sierpinski_rewrite(iso) }
  }
}


/*
    orig. impl. (which inspired Shape_Op trait hierarchy:

  def sierpinskify(s: Drawing_Shape, iteration: Int,
                   ink_func: (Drawing_Shape, Int, Int) => Drawing_Shape):
      Drawing_Shape = {

    def sierpinskify_shape(s: Drawing_Shape): Drawing_Shape = {

      def sierpinski_rewrite(iso_tri: Iso_Triangle): Drawing_Shape = {
        val Iso_Triangle(base_width, height) = iso_tri
        val half_tri = Iso_Triangle(base_width/2, height/2)
        (ink_func(half_tri, iteration, 0) -+ ( 0,           -height/4)) -&
        (ink_func(half_tri, iteration, 1) -+ (-base_width/4, height/4)) -&
        (ink_func(half_tri, iteration, 2) -+ ( base_width/4, height/4))
      }

      s match {
        case Translated_Shape(shape, x_move, y_move) =>
          Translated_Shape(sierpinskify_shape(shape), x_move, y_move)
        case Scaled_Shape(shape, x_scaling, y_scaling) =>
          Scaled_Shape(sierpinskify_shape(shape), x_scaling, y_scaling)
        case Rotated_Shape(shape, degrees, x_pivot, y_pivot) =>
          Rotated_Shape(sierpinskify_shape(shape), degrees, x_pivot, y_pivot)
        case Composite_Shape(below, above) =>
          Composite_Shape(sierpinskify_shape(below), sierpinskify_shape(above))
        case Inked_Shape(shape, pen) =>
          Inked_Shape(sierpinskify_shape(shape), pen)
        case iso_tri @ Iso_Triangle(_, _) =>
          sierpinski_rewrite(iso_tri)
        case x @ ( _ : Line      | _ : Hemisphere    | _ : Right_Triangle |
                   _ : Rectangle | _ : Parallelogram | _ : Trapezoid      |
                   _ : Pentagon  | _ : Hexagon       | _ : Octagon        |
                   _ : Ellipse   | _ : Free_Form     | _ : Writing ) =>   x
      }
    }

    sierpinskify_shape(s)
  }
*/
