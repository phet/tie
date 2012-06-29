/*
   file: k_k_/test/graphics/tie/tile/Svg_Image_Transformations_Test.scala

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

import k_k_.graphics.tie._
import k_k_.graphics.tie.ink.{Named_Colors => C, _}
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.text._

import k_k_.graphics.tie.tile._
import k_k_.graphics.tie.tile.conversions._


@Test
class Svg_Image_Transformations_Test extends Svg_Test_Base {

  val filename = "test_image_transforms.svg"

  val title = "Image (Shape) Transformations"

  val img_orig_dir_root = "src/test/resources"
  val img_mapped_dir_root = ".."

  val img_path_mapper = (s: String) =>
                          s.replace(img_orig_dir_root, img_mapped_dir_root)

  val img_path = img_orig_dir_root + "/images/transform_test_img.jpg"
  val actual_img_dims = (375.0, 500.0)

  val arrow = Iso_Triangle(35, 50) -% 90 -&
              (Line(30) -+ (-40, -8)) -&
              (Line(30) -+ (-40, 8)) -~ center_pen


  protected def create_canvas() = {
    val img = Image(img_path, actual_img_dims._1 / 8, actual_img_dims._2 / 8,
                    img_path_mapper)

    val orig_img = box_image(img).at(R_Mid).align_under(arrow.pad(20, 0)->L_Mid)

    val transformations = Seq(
                               (img -# .5 -&
                                 (_: Shape).move(20, 20) -&
                                    (Circle(5) -~ Pen.fill(C.Yellow)),
                                "move(20, 20)"),
                               ((_: Shape).scale(1.5),
                                "scale(1.5)"),
                               ((_: Shape).scale(1, 1.5),
                                "scale(1, 1.5)"),
                               (img -# .5 -&
                                  (_: Shape).rotate(45),
                                "rotate(45)"),
                               (img -# .5 -&
                                 (_: Shape).rotate(45, 0, 30) -&
                                   (Circle(5) -~ Pen.fill(C.Yellow) -+ (0, 30)),
                                "rotate(45, 0, 30)"),
                               (img -# .5 -&
                                 (_: Shape).skew_horiz(30),
                                "skew_horiz(30)"),
                               (img -# .5 -&
                                  (_: Shape).reflect(60),
                                "reflect(60)"),
                               (img -# .5 -&
                                 (_: Shape).reflect(60, 5, 20) -&
                                   (Circle(5) -~ Pen.fill(C.Yellow) -+ (5, 20)),
                                "reflect(60, 5, 20)"),
                               (img -# .5 -&
                                 (_: Shape).skew_vert(30),
                                "skew_vert(30)") )
    val transformed_images =
          transformations.map( p => (p._1(img), Text_Line(p._2, desc_font)) ).
                          map( p => (box_image(p._1), Writing(p._2).pad(0, 4))).
                          map( p => (B_Mid of p._1).align_under(p._2, Inside) )

    new Canvas(Canvas_Props(620, 450, title = title),
               (orig_img->R_Mid).align_under(layout_grid(transformed_images, 3),
                                             Outside)
               -+@ (0, 0)
              )
  }

  protected def layout_grid(shapes: Seq[Shape], row_size: Int): Shape = {
    val (horiz_pad, vert_pad) = (8, 4)
    val shape_groups_it = shapes.grouped(row_size)
    shape_groups_it.map { shape_group =>
      shape_group.map( _.pad(Center, horiz_pad, vert_pad) ).join(R_Mid)
    }.join(B_Mid)
  }

  def box_image(img: Shape): Shape = {
    val box = Rectangle(actual_img_dims._1 * .28, actual_img_dims._2 * .28)
    img -& box
  }
}
