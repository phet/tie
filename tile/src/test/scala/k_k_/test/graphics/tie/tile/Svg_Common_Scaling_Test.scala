/*
   file: k_k_/test/graphics/tie/tile/Svg_Common_Scaling_Test.scala

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

import java.io.File

import scala.io.Source

import k_k_.graphics.tie._
import k_k_.graphics.tie.ink.{Named_Colors => C, _}
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.text._

import k_k_.graphics.tie.tile._
import k_k_.graphics.tie.tile.conversions._


sealed abstract class OO_Entity { val name: String }

case class Class_Entity(name: String, abstract_? : Boolean = false)
    extends OO_Entity

object Root_Class extends Class_Entity("Object")

case class Instance_Entity(name: String, clazz: Class_Entity = Root_Class)
    extends OO_Entity

case class Interface_Entity(name: String)
    extends OO_Entity


@Test
class Svg_Common_Scaling_Test extends Svg_Test_Base {

  val filename = "test_common_scaling.svg"

  val title = "Common Shape Scaling"


  val entities_fpath =
          "target/test-classes/data/entities/tie_shape_entities.list"
  val entities_file = new File(entities_fpath)

  @Test
  def entities_file_exists {
    assert(entities_file.exists)
  }

  protected def get_entities: Iterator[OO_Entity] = {

/*
    val entities_source = Source.fromFile(entities_file)
    val entities = try { parse_entities(entities_source.getLines)
                   } finally { entities_source.close }
*/

    val source_lines = Source.fromFile(entities_file).getLines
    parse_entities(source_lines)
  }

  
  protected def create_canvas() = {
    val entities = get_entities.toIterable
    assert(!entities.isEmpty)

    type Shape_Combo = (Shape, Shape) => Shape

    val naive_combo: Shape_Combo = ( _ combo _ )
    val fit_combo  : Shape_Combo = ( _ scale_up_asym_combo _.pad(5, 0) )

    val uniform_scaling = ( _: Iterable[Shape] ).scale_up_to_uniform

    val (render_naive_fit, render_true_fit, render_uniform_fit) = 
          (render_entities( render_entity(naive_combo) _ )(identity) _ ,
           render_entities( render_entity(fit_combo)   _ )(identity) _ ,
           render_entities( render_entity(fit_combo)   _ )(uniform_scaling) _ )

    val naive_fitting :: true_fitting :: uniform_fitting :: _ =
            List(render_naive_fit(entities).
                   over_bounding_box(bbox_pen -~& Pen.stroke(C.red, 4).
                                                      fill(C.firebrick -# .25)),
                 render_true_fit(entities).
                   over_bounding_box(bbox_pen -~& Pen.stroke(C.goldenrod, 4).
                                                      fill(C.yellow -# .5)),
                 render_uniform_fit(entities).
                   over_bounding_box(bbox_pen -~& Pen.stroke(C.green, 6).
                                                      fill(C.green -# .4))).
              map ( _ -* .75 )

    val horiz_div = Line(600).pad(0, 10)

    new Canvas(Canvas_Props(1200, 600, title = title),
               Seq(naive_fitting,
                     horiz_div,
                   true_fitting,
                     horiz_div,
                   uniform_fitting).join(Bottom_Middle)
/*
               naive_fitting.align_combo(horiz_div, Bottom_Middle, Outside).
                             align_combo(true_fitting, Bottom_Middle, Outside).
                             align_combo(horiz_div, Bottom_Middle, Outside).
                             align_combo(uniform_fitting, Bottom_Middle, Outside)
*/
               -+@ (0, 0)
              )
  }


  protected def render_entities(render_entity: OO_Entity => Shape)
                               (shapes_transform: Iterable[Shape] =>
                                                  Iterable[Shape])
                               (entities: Iterable[OO_Entity]):
      Shape = {
    val (horiz_pad, vert_pad) = (10, 10)
    val (n, n_groups) = (entities.size, 4)
    val group_size = n / n_groups + (if (n % n_groups == 0) 0 else 1)
    val entity_shapes_it = shapes_transform( entities.map(render_entity(_)) ).
                             grouped(group_size)
    entity_shapes_it.map { entity_shapes =>
      entity_shapes.map( _.pad(Center, horiz_pad, vert_pad) ).
                    join(Right_Middle)
    }.join(Bottom_Middle)

/* (pre-join):
    entity_shapes_it.map { entity_shapes =>
      (Null_Shape /: entity_shapes) { (l, r) =>
        l.align_combo(r.pad(Center, horiz_pad, vert_pad), Right_Middle, Outside)
      }
    }.foldLeft(Null_Shape) { (top, bottom) => 
        top.align_combo(bottom, Bottom_Middle, Outside)
    }
*/
  }


  protected def parse_entities(lines: Iterator[String]): Iterator[OO_Entity] = {
    //????how to make this lazy????views????
    // NOTE: \w+ is incorrect way to parse scala identifiers: match punct chars!
    val Parse_Entity_Line = """(?x) ^ \s* (?: (?: sealed     \s+ )? |
                                              (?: (abstract) \s+ )? |
                                              (?: final      \s+ )? |
                                              (?: public     \s+ )? |
                                              (?: protected  \s+ )? |
                                              (?: private    \s+ )? )*
                                        (?: (class)  \s+ |
                                              (object) \s+ |
                                            (trait)  \s+ ) ( \w+ )
                                  .* $ """.r
    lines.collect {
        // NOTE: clearer, but less efficient to repeat pattern match
        case Parse_Entity_Line(abstract_, class_, object_, trait_, name)
               if class_ ne null =>
        Class_Entity(name, abstract_ ne null)
        case Parse_Entity_Line(_, _, object_, _, name)
               if object_ ne null =>
        Instance_Entity(name)
        case Parse_Entity_Line(_, _, _, trait_, name)
               if trait_ ne null =>
          Interface_Entity(name)
      }
  }

  // argument order: combine_shapes(bounding_rect, caption_writing)
  protected def render_entity(combine_shapes: (Shape, Shape) => Shape)
                             (entity: OO_Entity): Shape = {

    val font_family_name = "Arial"
  
    val name_font       = Font(font_family_name, 12)
    val stereotype_font = Font(font_family_name, 12, Italic)

    def stereotype(name: String) =
      Text_Line("<<" + name + ">>",  stereotype_font, Middle_Align)

    val abstract_line  = stereotype("abstract")
    val interface_line = stereotype("interface")
  
    val caption = entity match {
      case Class_Entity(name, abstract_?) =>
        val name_line = Text_Line(name, name_font, Middle_Align)
        Text_Block(if (abstract_?) abstract_line :: name_line :: Nil
                   else                             name_line :: Nil)
      case Instance_Entity(name, Class_Entity(class_name, _)) =>
        Text_Line(name + " : " + class_name, name_font, Underline)
      case Interface_Entity(name) =>
        Text_Block(interface_line ::
                   Text_Line(name, name_font, Middle_Align) :: Nil)
    }

    combine_shapes(
                   Rectangle(50, 35) -~ Pen(C.black, C.white),
                   (caption -~ Default_Writing_Pen)
                  )
  }
}
