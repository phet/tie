/*
   file: k_k_/test/graphics/tie/tile/Svg_Bounding_Box_Points_Test.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.test.graphics.tie.tile

import org.junit._
import Assert._

import k_k_.graphics.tie._
import k_k_.graphics.tie.ink.{Named_Colors => C, _}
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.text._
/*
import k_k_.graphics.tie._ , ink.{Named_Colors => C, _} , shapes._ , text._
import k_k_.graphics.tie._,
                         ink.{Named_Colors => C, _},
                         shapes._,
                                text._
*/

import k_k_.graphics.tie.tile._


@Test
class Svg_Rectangle_Bounding_Box_Points_Test
    extends Svg_Bounding_Box_Points_Test_Base {

  val filename = "test_bounding_box_points_[rectangle].svg"

  val title = "(Rectangle) Bounding Box Named Points"

  val shape = Rectangle(320, 240)
}

@Test
class Svg_Ellipse_Bounding_Box_Points_Test
    extends Svg_Bounding_Box_Points_Test_Base {

  val filename = "test_bounding_box_points_[ellipse].svg"

  val title = "(Ellipse) Bounding Box Named Points"

  val shape = Diam_Ellipse(320, 240)
}

abstract class Svg_Bounding_Box_Points_Test_Base extends Svg_Test_Base {

  val shape: Pre_Formulated_Shape

  lazy val placed_shape = shape -~ Pen.stroke(C.black) -+ (200, 150)

  //????not clear why PartialFunction.cond can infer type of PartialFunction
  // arg, but Drawing_Shape_Traversal.collect can not... precipitates ugly type
  // and essentially precludes inline definition:
  val ignore_rectangular: PartialFunction[Drawing_Shape, Drawing_Shape] =
        { case Rectangular(_, _) => Null_Shape }
  lazy val bboxed_placed_shape = placed_shape -&
           (placed_shape.collect(ignore_rectangular).headOption.
              getOrElse(placed_shape.bounding_box_shape -~ bbox_pen))
/*
  // code works, but requires foreknowledge of level of nesting... not optimal
  lazy val bboxed_placed_shape = placed_shape -& (placed_shape match {
          case Unary_Shape_Op(Unary_Shape_Op(Rectangular(_, _))) => Null_Shape
          case _ => placed_shape.bounding_box_shape -~ bbox_pen
        })

  // alternative to using collect (above), although perhaps less clear
  lazy val bboxed_placed_shape = placed_shape -&
           (if (placed_shape.exists(PartialFunction.cond(_) {
                                        case Rectangular(_, _) => true }))
              Null_Shape
            else
              placed_shape.bounding_box_shape -~ bbox_pen)
*/


  val X = (Line(16) -%  45) -&
          (Line(16) -% -45)


  protected def create_canvas() = {
    new Canvas(Canvas_Props(500, 300, Origin_Top_Left, title),
               (bboxed_placed_shape -&
               label_pt(_.center,        "center",        C.black,    0, -15) -&
               label_pt(_.top_middle,    "top middle",    C.red,      0, -15) -&
               label_pt(_.bottom_middle, "bottom middle", C.orange,   0,  10) -&
               label_pt(_.left_middle,   "left middle",   C.yellow, -38, -15) -&
               label_pt(_.right_middle,  "right middle",  C.green,   42, -15) -&
               label_pt(_.top_left,      "top left",      C.blue,   -30, -15) -&
               label_pt(_.top_right,     "top right",     C.indigo,  32, -15) -&
               label_pt(_.bottom_left,   "bottom left",   C.violet, -40,  10) -&
               label_pt(_.bottom_right,  "bottom right",  C.brown,   45,  10))
                 -+ (0, 0)
/*
               bboxed_placed_shape,
               label_pt(_.center,        "center",        C.black,    0, -15),
               label_pt(_.top_middle,    "top middle",    C.red,      0, -15),
               label_pt(_.bottom_middle, "bottom middle", C.orange,   0,  10),
               label_pt(_.left_middle,   "left middle",   C.yellow, -38, -15),
               label_pt(_.right_middle,  "right middle",  C.green,   42, -15),
               label_pt(_.top_left,      "top left",      C.blue,   -30, -15),
               label_pt(_.top_right,     "top right",     C.indigo,  32, -15),
               label_pt(_.bottom_left,   "bottom left",   C.violet, -40,  10),
               label_pt(_.bottom_right,  "bottom right",  C.brown,   45,  10)
*/
              )
  }

  def label_pt(named_pt_method: Drawing_Shape => Point, name: String,
               ink: Ink, name_offset_x: Double, name_offset_y: Double) = {

    (write(name, ink) -+ named_pt_method(placed_shape)
       -+ (name_offset_x, name_offset_y)) -&
    (X -+ named_pt_method(placed_shape) -~ Pen.stroke(ink,  2.5))
  }
}
