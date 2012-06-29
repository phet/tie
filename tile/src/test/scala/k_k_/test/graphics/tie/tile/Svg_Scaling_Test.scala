/*
   file: k_k_/test/graphics/tie/tile/Svg_Scaling_Test.scala

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
import k_k_.graphics.tie.tile.adjust._


@Test
class Svg_Scaling_Test extends Svg_Test_Base {

  val filename = "test_scaling.svg"

  val title = "Scaling to Bounding Box"


  val arrow = Iso_Triangle(35, 50) -% 90 -&
              (Line(30) -+ (-40, -8)) -&
              (Line(30) -+ (-40, 8)) -~ center_pen

  val bbox = Origin_Ortho_Rectangle(100, 80)


  protected def create_canvas() = {
    new Canvas(new Canvas_Props(1280, 500, Origin_Top_Left, title),
               (exhibit_scaling(Ellipse(35, 25), bbox)
                  -+ (80, 50)) -&
               (exhibit_scaling(Right_Triangle(40, 50), bbox)
                  -+ (80, 180)),
               (exhibit_scaling(Octagon(25, 50, 25, 50) -* 2.25, bbox)
                  -+ (80, 330)),
               (exhibit_scaling(Hexagon(30, 50, 50) -% 90 -* (2.4, 1.2), bbox)
                  -+ (80, 460))
              )

/* (other shapes to consider testing (not necessarily with these dimensions))
               Rectangle(80, 50)
               Iso_Triangle(35, 50)
               Pentagon(50, 50, 35, 55)
               Parallelogram(50, 70, 50)
               Trapezoid(55, 80, 50)
*/
  }

  protected def exhibit_scaling(shape: Shape, bounds: Ortho_Rectangle):
      Shape = {
    val boxed_shape = shape -~ shape_pen -&
                      (bounds.as_drawing_shape -~ bbox_pen)

    val arrow_offset_x = boxed_shape.bounding_box.width + 30
    val box_and_arrow = boxed_shape -& 
                        (arrow -+ (arrow_offset_x, 0))

    val scaled_shapes =
      List(scale_shape(shape, Orig_Scale),
           scale_shape(shape, Scale_To(bounds)),
           scale_shape(shape, Scale_To_Asym(bounds)),
           scale_shape(shape, Scale_To_Max(bounds)),
           scale_shape(shape, Scale_To_Max_Asym(bounds)),
           scale_shape(shape, Scale_To_Min(bounds)),
           scale_shape(shape, Scale_To_Min_Asym(bounds)))

    // align each shape horizontally, according to its own dimensions
    val scaling_offsets_x = scaled_shapes.map { scaled_shape =>
        val (shape, name) = scaled_shape
        shape.bounding_box.width
      }.foldLeft(List((0.0, arrow.bounding_box.width))) { (sums, shape_width) =>
        val (prev_sum, prev_shape_width) = sums.head
        val layout_width = (shape_width + prev_shape_width) / 2 + 20
        (prev_sum + layout_width, shape_width) :: sums
      }.map { 
         _._1 + arrow_offset_x // use only '._1' (the running sums)
      }.reverse.tail // ignore head, since its 0.0 val was only placeholder

/* NOTE: this approach does not work, because it does not center each shape in
   the area allotted for it; instead, each is set off to the left of the space:

    val first_scaling_offset_x = arrow.bounding_box.width + 20

    val scaling_offsets_x =
      (List(first_scaling_offset_x) /: scaled_shapes) { (sums, scaled_shape) =>
          val (shape, name) = scaled_shape
          sums.head + shape.bounding_box.width + 20 :: sums
      }.map {
         _ + arrow_offset_x
      }.reverse
*/

    // align the name beneath each shape, according to the largest dimensions:
    val scaling_offset_y = {
      val max_y = (0.0 /: scaled_shapes) { (max_y, scaled_shape) =>
          val (shape, name) = scaled_shape
          max_y max shape.bounding_box.height
        }
      max_y / 2 + 10
    }

    val named_scaled_shapes = scaled_shapes map { (scaled_shape) =>
      val (shape, name) = scaled_shape
      shape -& (name -+ (0, scaling_offset_y))
      }

    val translated_shapes = named_scaled_shapes.zip(scaling_offsets_x).map {
        (shape_offset_pair) =>
           val (scaled_shape, offset_x) = shape_offset_pair
           scaled_shape -+ (offset_x, 0)
      }

    //?????why does the following not work (in the map func. above)??????
    //[ERROR] C:\projects\devel\scripts\tie\tile\src\test\scala\k_k_\test\graphics\tietile\Svg_Scaling_Test.scala:69: error: not a legal formal parameter
    //  ((scaled_shape, offset_x): Tuple2[Shape, Double]) => scaled_shape -+ (offset_x, 0)
    //  (scaled_shape, offset_x): Tuple2[Shape, Double] => scaled_shape -+ (offset_x, 0)
    //  Tuple2[Shape, Double](scaled_shape, offset_x) => scaled_shape -+ (offset_x, 0)


    box_and_arrow -&
    translated_shapes.reduceLeft(_ -& _)
  }

  protected def scale_shape(shape: Shape, how: Scaling_Strategy):
      (Shape, Shape) = {

    def prepare(shape: Shape) =
      shape -~ shape_pen -& center_X

    def add_bounding_box(prepared_shape: Shape): Shape = {
      val bbox_shape = how match {
        case Orig_Scale              => None
        case Scale_To(w, h)          => Some(Origin_Ortho_Rectangle(w, h))
        case Scale_To_Asym(w, h)     => Some(Origin_Ortho_Rectangle(w, h))
        case Scale_To_Max(w, h)      => Some(Origin_Ortho_Rectangle(w, h))
        case Scale_To_Max_Asym(w, h) => Some(Origin_Ortho_Rectangle(w, h))
        case Scale_To_Min(w, h)      => Some(Origin_Ortho_Rectangle(w, h))
        case Scale_To_Min_Asym(w, h) => Some(Origin_Ortho_Rectangle(w, h))
        case _: Collection_Scaling_Strategy=> None
      }

      bbox_shape match {
        case Some(bbox) => prepared_shape -& (bbox.as_drawing_shape -~ bbox_pen)
        case None       => prepared_shape
      }
    }

    val name = fmt_name(how)

    (add_bounding_box(prepare(shape).scale_to(how)),
     write(name, C.black))
  }

  protected def fmt_name(scaling_strategy: Scaling_Strategy): String = {
    val strategy_str = scaling_strategy match {
      case Orig_Scale =>
        "orig. scale"
      case Scale_To(w, h) =>
        "scale to" // - " + w + " x " + h
      case Scale_To_Asym(w, h) =>
        "scale to (asym)" // - " + w + " x " + h
      case Scale_To_Max(w, h) =>
        "scale to *max*" // - " + w + " x " + h
      case Scale_To_Max_Asym(w, h) =>
        "scale to *max* (asym)" // - " + w + " x " + h
      case Scale_To_Min(w, h) =>
        "scale to *min*" // - " + w + " x " + h
      case Scale_To_Min_Asym(w, h) =>
        "scale to *min* (asym)" // - " + w + " x " + h
      case _: Collection_Scaling_Strategy =>
        "collection scaling..."
    }
    strategy_str
  }
}
