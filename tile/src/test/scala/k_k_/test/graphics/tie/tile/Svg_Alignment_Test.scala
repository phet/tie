/*
   file: k_k_/test/graphics/tie/tile/Svg_Alignment_Test.scala

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

import k_k_.graphics.tie.tile._
import k_k_.graphics.tie.tile.conversions._


@Test
class Svg_Alignment_Test extends Svg_Test_Base {

  val filename = "test_alignment.svg"

  val title = "Alignment with Bounding Box"


  val bbox_shape = Rectangle(100, 80) -~ bbox_pen


  protected def create_canvas() = {
    new Canvas(Canvas_Props(1280, 500, Origin_Top_Left, title),
               (align_shape(Rectangle(80, 50),               Center, Inside)
                  -+ (60, 0)) -&
               (align_shape(Right_Triangle(40, 50),     Left_Middle, Inside)
                  -+ (200, 0)) -&
               (align_shape(Ellipse(35, 25),             Top_Middle, Inside)
                  -+ (340, 0)) -&
               (align_shape(Iso_Triangle(35, 50),             R_Mid, Inside)
                  -+ (480, 0)) -&
               (align_shape(Circle(35),                       B_Mid, Inside)
                  -+ (620, 0)) -&
               (align_shape(Pentagon(50, 50, 35, 55),   Bottom_Left, Inside)
                  -+ (760, 0)) -&
               (align_shape(Hexagon(30, 50, 50) -% 90,          T_L, Inside)
                  -+ (900, 0)) -&
               (align_shape(Parallelogram(50, 70, 50),          T_R, Inside)
                  -+ (1040, 0)) -&
               (align_shape(Trapezoid(55, 80, 50),              B_R, Inside)
                  -+ (1180, 0))
               -+ (0, 50),

               (align_shape(Rectangle(80, 50),             Center, Centered)
                  -+ (60, 0)) -&
               (align_shape(Parallelogram(50, 70, 50),      L_Mid, Centered)
                  -+ (200, 0)) -&
               (align_shape(Iso_Triangle(35, 50),           T_Mid, Centered)
                  -+ (340, 0)) -&
               (align_shape(Ellipse(35, 25),         Right_Middle, Centered)
                  -+ (480, 0)) -&
               (align_shape(Right_Triangle(40, 50), Bottom_Middle, Centered)
                  -+ (620, 0)) -&
               (align_shape(Pentagon(50, 50, 35, 55),         B_L, Centered)
                  -+ (760, 0)) -&
               (align_shape(Hexagon(30, 50, 50) -% 90,   Top_Left, Centered)
                  -+ (900, 0)) -&
               (align_shape(Trapezoid(55, 80, 50),      Top_Right, Centered)
                  -+ (1040, 0)) -&
               (align_shape(Octagon(25, 50, 25, 50), Bottom_Right, Centered)
                  -+ (1180, 0))
               -+ (0, 210),

               (align_shape(Rectangle(80, 50),              Center, Outside)
                  -+ (60, 0)) -&
               (align_shape(Equi_Triangle(60),               L_Mid, Outside)
                  -+ (200, 0)) -&
               (align_shape(Parallelogram(50, 70, 50),       T_Mid, Outside)
                  -+ (340, 0)) -&
               (align_shape(Pentagon(50, 50, 35, 55), Right_Middle, Outside)
                  -+ (480, 0)) -&
               (align_shape(Ellipse(35, 25),         Bottom_Middle, Outside)
                  -+ (620, 0)) -&
               (align_shape(Hexagon(30, 50, 50) -% 90,         B_L, Outside)
                  -+ (760, 0)) -&
               (align_shape(Right_Triangle(40, 50),       Top_Left, Outside)
                  -+ (900, 0)) -&
               (align_shape(Octagon(25, 50, 25, 50),     Top_Right, Outside)
                  -+ (1040, 0)) -&
               (align_shape(Square(50),               Bottom_Right, Outside)
                  -+ (1180, 0))
               -+ (0, 380)
              )
  }

  protected def align_shape(shape: Shape, where: Bounding_Box_Pos,
                            how: Alignment_Relation): Shape = {

    def prepare(shape: Shape) =
      shape -~ shape_pen -& center_X

    val name = fmt_name _ tupled calc_name(where, how)
    val name_offset_y = bbox_shape.bounding_box.height / 2 + 30

    val scale_factor = (how, where) match {
      case (Inside, _)
         | (_, Center)   => 1
      case (Centered, _) => .85
      case (Outside, _)  => .7
    }

    (write(name, C.black) -+ (0, name_offset_y)) -&
    (prepare(shape).to(bbox_shape, where, how) -& bbox_shape -* scale_factor)
  }

  protected def calc_name(where: Bounding_Box_Pos, how: Alignment_Relation):
      (String, String) = {

    val where_str = where match {
      case Center                => "center"
      case Top_Middle    | T_Mid => "top middle"
      case Bottom_Middle | B_Mid => "bottom middle"
      case Left_Middle   | L_Mid => "left middle"
      case Right_Middle  | R_Mid => "right middle"
      case Top_Left      | T_L   => "top left"
      case Top_Right     | T_R   => "top right"
      case Bottom_Left   | B_L   => "bottom left"
      case Bottom_Right  | B_R   => "bottom right"
    }

    val how_str = how match {
      case Inside   => "inside"
      case Centered => "centered"
      case Outside  => "outside"
    }

    (where_str, how_str)
  }

  protected def fmt_name(where_str: String, how_str: String): String =
    how_str + ": " + where_str
}
