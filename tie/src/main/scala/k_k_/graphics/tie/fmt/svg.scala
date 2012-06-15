/*
   file: k_k_/graphics/tie/fmt/svg.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie

package fmt.svg {

import java.io.Writer

import scala.xml.{Utility => XML_Util}

import k_k_.graphics.tie.effects.Filter
import k_k_.graphics.tie.ink._
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.path._
import k_k_.graphics.tie.shapes.text._


object Script_Type {

  val ecmascript_mime = "application/ecmascript"
}

class Script_Type(val mime: String)

object ECMA_Script extends Script_Type(Script_Type.ecmascript_mime)
object Javascript  extends Script_Type(Script_Type.ecmascript_mime)


sealed abstract class Svg_Renderer_Base extends Char_Output_Renderer {

  protected case class Univ_Attrs(id: Option[String] = None,
                                  opacity: Option[Double] = None) {

    def isDefined = id.isDefined || opacity.isDefined
  }


  protected def fmt_prologue(view_box: Visible_Area, title: String,
                             desc: Option[String]): String

  protected def fmt_epilogue(): String


  protected def script_uris:              List[(Script_Type, String)] = Nil
  protected def script_content:         Option[(Script_Type, String)] = None
  protected def script_post_content_uris: List[(Script_Type, String)] = Nil
}

trait Catenated_Transforms { self: Svg_Content_Renderer =>

  override
  protected def write_transformed_shape(defs: Defs, os: Writer,
                                        ordered_transforms: List[String],
                                        shape: Drawing_Shape,
                                        attrs: Univ_Attrs): Defs = {
    val transform_attr = ordered_transforms.mkString("transform=\"", " ", "\"")
    write_group(defs, os, fmt(attrs) + transform_attr, shape)
  }
}

//!!!to truly reduce size, replace String.format %f (.000000 on whole nums)!!!
trait Small_File_Size
    extends Catenated_Transforms{ self: Svg_Content_Renderer =>
}


object Svg_Renderer extends Svg_Renderer

object Size_Optimized_Svg_Renderer extends Svg_Renderer with Small_File_Size

class Svg_Renderer extends Svg_Content_Renderer {

  protected def fmt_prologue(view_box: Visible_Area, title: String,
                             desc: Option[String]): String = {
    val result = """<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" 
                     "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg viewBox="%f %f %f %f" version="1.1"
     xmlns="http://www.w3.org/2000/svg"
     xmlns:xlink="http://www.w3.org/1999/xlink"
     xmlns:tie="%s"
     tie:version="%s">

  <title>%s</title>

""".format(view_box.upper_left.x, view_box.upper_left.y, // (dumb emacs mode)"
           view_box.width,        view_box.height,
           Version.xmlns_uri,     Version.toString,
           title)
    desc match {
      case Some(str) => result + "  <desc>%s</desc>\n".format(str)
      case None      => result
    }
  }

  protected def fmt_epilogue(): String = {
    "</svg>\n"
  }
}


object Svg_Elem_Renderer extends Svg_Elem_Renderer

class Svg_Elem_Renderer extends Svg_Content_Renderer {

  protected def fmt_prologue(view_box: Visible_Area, title: String,
                             desc: Option[String]): String = {
    val result =
"""<svg viewBox="%f %f %f %f" xmlns="http://www.w3.org/2000/svg"
                           xmlns:xlink="http://www.w3.org/1999/xlink"
                           xmlns:tie="%s"
                           tie:version="%s">

  <title>%s</title>

""".format(view_box.upper_left.x, view_box.upper_left.y, // (dumb emacs mode)"
           view_box.width,        view_box.height,
           Version.xmlns_uri,     Version.toString,
           title)
    desc match {
      case Some(str) => result + "  <desc>%s</desc>\n".format(str)
      case None      => result
    }
  }

  protected def fmt_epilogue(): String =
    "</svg>\n"
}


object Svg_Frag_Renderer extends Svg_Frag_Renderer

class Svg_Frag_Renderer extends Svg_Content_Renderer {

  protected def fmt_prologue(view_box: Visible_Area, title: String,
                             desc: Option[String]): String =
    ""

  protected def fmt_epilogue(): String =
    ""
}


sealed abstract class Svg_Content_Renderer extends Svg_Renderer_Base {

  protected final def do_render(canvas: Canvas, os: Writer): Boolean = {
    val view_box = canvas.visible_area
    os.write(fmt_prologue(view_box, canvas.title, canvas.desc))
    os.write(fmt_view_box_debug_str(view_box))

    script_uris.             foreach( p => write_script_uri(os, p._1, p._2) )
    script_content.          foreach( p => write_script_content(os, p._1, p._2))
    script_post_content_uris.foreach( p => write_script_uri(os, p._1, p._2) )
    
    // NOTE: render in reverse shapes order, so last-added is drawn last
    val defs = (canvas.shapes foldRight new Defs(view_box)){ (shape, defs) =>
      render_shape(defs, os, ensure_ink(shape))
    }
    render_license_stamp(defs, os, view_box)
    render_defs(os, defs)
    os.write(fmt_epilogue())
    return true
  }


  val gradient_id_prefix  = "tie-Gradient"
  val pattern_id_prefix   = "tie-Pattern"
  val clip_path_id_prefix = "tie-Clip"
  val mask_id_prefix      = "tie-Mask"
  val filter_id_prefix    = "tie-Filter"
  val license_stamp_id    = "tie-l-i-c-e-n-s-e-s-t-a-m-p"

  protected val text_ruler_factory = Writing.text_ruler_factory


  protected abstract class Def {
    val id: String
  }
  protected case class Gradient_Def(gradient: Gradient, id: String)
      extends Def
  protected case class Pattern_Def(pattern: Pattern, id: String)
      extends Def
  protected case class Clip_Path_Def(clipping: Drawing_Shape,
                                     rule: Clip_Rule, id: String)
      extends Def
  protected case class Rng_Def(rng: scala.util.Random, id: String)
      extends Def
  protected case class Mask_Def(mask: Drawing_Shape, id: String)
      extends Def
  protected case class Filter_Def(filter: Filter, id: String)
      extends Def


  protected sealed class Defs(val defs: List[Def],
                              protected[Svg_Content_Renderer]
                                val next_gradient_num: Int,
                              protected[Svg_Content_Renderer]
                                val next_pattern_num: Int,
                              protected[Svg_Content_Renderer]
                                val next_clip_path_num: Int,
                              protected[Svg_Content_Renderer]
                                val next_mask_num: Int,
                              protected[Svg_Content_Renderer]
                                val next_filter_num: Int) {

    def this(area: Visible_Area) =
      this(Defs.init(area), 1, 1, 1, 1, 1)

    def reverse: Defs =
      create_defs(defs.reverse)      

    def calc_url(g: Gradient): (String, Defs) = {
      find_matching_gradient(g) match {
        case Some(definition) =>
          ("#" + definition.id, this)
        case None =>
          val id = fmt_gradient_id(next_gradient_num)
          ("#" + id, create_defs(Gradient_Def(g, id) :: defs,
                                 next_gradient_num = next_gradient_num + 1))
      }
    }

    def calc_url(p: Pattern): (String, Defs) = {
      find_matching_pattern(p) match {
        case Some(definition) =>
          ("#" + definition.id, this)
        case None =>
          val id = fmt_pattern_id(next_pattern_num)
          ("#" + id, create_defs(Pattern_Def(p, id) :: defs,
                                 next_pattern_num = next_pattern_num + 1))
      }
    }

    def calc_clip_path_url(shape: Drawing_Shape, rule: Clip_Rule):
        (String, Defs) = {
      find_matching_clip_path(shape, rule) match {
        case Some(definition) =>
          ("#" + definition.id, this)
        case None =>
          val id = fmt_clip_path_id(next_clip_path_num)
          ("#" + id, create_defs(Clip_Path_Def(shape, rule, id) :: defs,
                                 next_clip_path_num = next_clip_path_num + 1))
      }
    }

    def calc_mask_url(shape: Drawing_Shape): (String, Defs) = {
      find_matching_mask(shape) match {
        case Some(definition) =>
          ("#" + definition.id, this)
        case None =>
          val id = fmt_mask_id(next_mask_num)
          ("#" + id, create_defs(Mask_Def(shape, id) :: defs,
                                 next_mask_num = next_mask_num + 1))
      }
    }

    def calc_url(filter: Filter): (String, Defs) = {
      find_matching_filter(filter) match {
        case Some(definition) =>
          ("#" + definition.id, this)
        case None =>
          val id = fmt_filter_id(next_filter_num)
          ("#" + id, create_defs(Filter_Def(filter, id) :: defs,
                                 next_filter_num = next_filter_num + 1))
      }
    }


    protected def find_in(the_defs: List[Def])
                         (discriminator: PartialFunction[Def, Boolean]):
        Option[Def] =
      the_defs.find( PartialFunction.cond(_)(discriminator) )

    // NOTE: use `==`, not `eq` in discriminators since looking for equivalent
    // vals, even when programmatically assembled identically multiple times

    protected def find_matching_gradient(g: Gradient): Option[Def] =
      find_in(defs){ case Gradient_Def(grad, _)      if grad == g  => true }

    protected def find_matching_pattern(p: Pattern): Option[Def] =
      find_in(defs){ case Pattern_Def(pat, _)        if pat == p   => true }

    protected def find_matching_clip_path(s: Drawing_Shape, rle: Clip_Rule):
        Option[Def] =
      find_in(defs){ case Clip_Path_Def(shape, rule, _) if shape == s &&
                                                           rule == rle => true }

    protected def find_matching_rng(p: Option[Pattern]): Option[Def] =
      find_in(defs){ case Rng_Def(rng, _)            if None == p => true }

    protected def find_matching_mask(s: Drawing_Shape): Option[Def] =
      find_in(defs){ case Mask_Def(shape, _)         if shape == s => true }

    protected def find_matching_filter(f: Filter): Option[Def] =
      find_in(defs){ case Filter_Def(filter, _)      if filter == f => true }

    protected def create_defs(defs: List[Def],
                              next_gradient_num: Int  = next_gradient_num,
                              next_pattern_num: Int   = next_pattern_num,
                              next_clip_path_num: Int = next_clip_path_num,
                              next_mask_num: Int      = next_mask_num,
                              next_filter_num: Int    = next_filter_num): Defs =
      new Defs(defs, next_gradient_num, next_pattern_num,
               next_clip_path_num, next_mask_num, next_filter_num)


    protected val fmt_gradient_id  = fmt_id(gradient_id_prefix)  _
    protected val fmt_pattern_id   = fmt_id(pattern_id_prefix)   _
    protected val fmt_clip_path_id = fmt_id(clip_path_id_prefix) _
    protected val fmt_mask_id      = fmt_id(mask_id_prefix)      _
    protected val fmt_filter_id    = fmt_id(filter_id_prefix)    _

    def ? = or_false(find_matching_rng(None).collect{case r:Rng_Def => bool(r)})
    protected def fmt_id(base_name: String)(num: Int) =
      "%s%02d".format(base_name, num)
  }

  protected final class Define_Defs(protected[Svg_Content_Renderer]
                                      val prev_defs: List[Def],
                                    override val defs: List[Def],
                                    next_gradient_num: Int,
                                    next_pattern_num: Int,
                                    next_clip_path_num: Int,
                                    next_mask_num: Int,
                                    next_filter_num: Int)
      extends Defs(defs, next_gradient_num, next_pattern_num,
                   next_clip_path_num, next_mask_num, next_filter_num) {

    def this(parent_defs: Defs) =
      this(parent_defs match {
             case def_defs: Define_Defs =>
               parent_defs.defs ::: def_defs.prev_defs
             case _ =>
               parent_defs.defs
           }, Nil,
           parent_defs.next_gradient_num,
           parent_defs.next_pattern_num,
           parent_defs.next_clip_path_num,
           parent_defs.next_mask_num,
           parent_defs.next_filter_num)

    override
    protected def find_matching_gradient(g: Gradient) =
      super.find_matching_gradient(g).orElse(
            find_in(defs){ case Gradient_Def(grad, _)  if grad == g => true } )

    override
    protected def find_matching_pattern(p: Pattern) =
      super.find_matching_pattern(p).orElse(
            find_in(defs){ case Pattern_Def(pat, _)    if pat == p => true } )

    override
    protected def find_matching_clip_path(s: Drawing_Shape, rle: Clip_Rule) =
      super.find_matching_clip_path(s, rle).orElse(
            find_in(defs){ case Clip_Path_Def(shape, rule, _) if shape == s &&
                                                                 rule == rle =>
                                                                   true } )

    override
    protected def find_matching_rng(p: Option[Pattern]): Option[Def] =
      super.find_matching_rng(p).orElse(
            find_in(defs){ case Rng_Def(rng, _)        if None == p => true } )

    override
    protected def find_matching_mask(s: Drawing_Shape) =
      super.find_matching_mask(s).orElse( 
            find_in(defs){ case Mask_Def(shape, _)     if shape == s => true } )

    override
    protected def find_matching_filter(f: Filter) =
      super.find_matching_filter(f).orElse( 
            find_in(defs){ case Filter_Def(filter, _)  if filter == f => true })

    override
    protected def create_defs(defs: List[Def],
                              next_gradient_num: Int  = next_gradient_num,
                              next_pattern_num: Int   = next_pattern_num,
                              next_clip_path_num: Int = next_clip_path_num,
                              next_mask_num: Int      = next_mask_num,
                              next_filter_num: Int    = next_filter_num): Defs =
      new Define_Defs(prev_defs, defs, next_gradient_num, next_pattern_num,
                      next_clip_path_num, next_mask_num, next_filter_num)
  }

  protected object Defs {
    def init(i: Int) = Rng_Def(new scala.util.Random(i), "tie-r_") :: Nil
  }


  protected def render_defs(os: Writer, orig_defs: Defs) {
    def write_defs(defs: Defs) {
      val def_defs: Defs = new Define_Defs(defs)
      val additional_defs = (def_defs /: defs.defs){ render_def(_, os, _) }
      if (!additional_defs.defs.isEmpty) {
        write_defs(additional_defs.reverse) // repeat... so long as new defs
      }
    }

    os.write("\n  <defs>\n")
    write_defs(orig_defs.reverse)
    os.write("  </defs>\n")
  }

  protected def render_def(defs: Defs, os: Writer, definition: Def): Defs = {
    definition match {
      case Gradient_Def(gradient, id) =>
        define_gradient(defs, os, id, gradient)
      case Pattern_Def(pattern, id) =>
        define_pattern(defs, os, id, pattern)
      case Clip_Path_Def(clipping, clip_rule, id) =>
        define_clip_path(defs, os, id, clipping, clip_rule)
      case Rng_Def(rng, id) => defs
      case Mask_Def(mask, id) =>
        define_mask(defs, os, id, mask)
      case Filter_Def(filter, id) =>
        define_filter(defs, os, id, filter)
    }
  }

  protected val default_shape_pen   = Default_Shape_Pen
  protected val default_writing_pen = Default_Writing_Pen

  protected def ensure_ink(shape: Drawing_Shape): Drawing_Shape = {

    // returns Option of updated shape 'modified' internally; iff not modified
    // (i.e. Option is None), Boolean indicates whether orig. shape needs ink.
    //
    // Yes, this is much more complicated than merely rebuilding the whole tree,
    // but the expectation is that most shapes will have Ink, and that it would
    // be cheaper to merely traverse, while looking for Ink, than to reconstruct
    // every shape.  furthermore, the Boolean is used to add Ink, not at the
    // terminal, but as high above as would not effect another shape (i.e. below
    // a Binary_Shape_Op).
    //!!!TRULY, THIS PREMISE SHOULD BE VALIDATED BY PROFILING!!!

    //!!!!!also, this heavily-recursive method is not tail-recursive; perhaps a
    // different algo is in order!!!!!!!
    def add_missing_ink(shape: Drawing_Shape, pen_tform: Option[Pen_Transform]):
        (Option[Drawing_Shape], Boolean) =
      shape match {
        // neither invisible shape, nor an image requires Ink
        case Invis_Rectangle(_, _)
           | Image(_, _, _)    => (None, false)

        case Inked_Shape(s, pen_tf: Pen_Transform) =>
          val new_pen_tform = pen_tform.map( _.compose(pen_tf) ).
                                  getOrElse(           pen_tf  )
          add_missing_ink(s, Some(new_pen_tform)) match {
            case (Some(mod_shape), _) =>
              if (new_pen_tform.is_replacement)
                (Some(Inked_Shape(mod_shape, new_pen_tform)), false)
              else
                (Some(mod_shape), false)
            case (None, true) => // request from child to add ink higher above
                if (new_pen_tform.is_replacement)
                  (Some(Inked_Shape(s, new_pen_tform)), false)
                else
                  (Some(Inked_Shape(s, new_pen_tform(default_shape_pen))),false)
            case (None, false) =>
              // NOTE: defensive programming--should never happen!: either a Pen
              // beneath would be transformed by new_pen_tform and result in new
              // Inked_Shape returned, or flag would indicate that ink needed
              (Some(Inked_Shape(s, new_pen_tform)), false)
            }

        case Inked_Shape(s, simple_pen) =>
          pen_tform.map( _(simple_pen) ) match {
            case Some(transformed_pen) =>
              (Some(Inked_Shape(s, transformed_pen)), false)
            case None =>
              (None, false) // if no transformation, leave as is
          }

        case ushape @ Unary_Shape_Op(s) =>
          add_missing_ink(s, pen_tform) match {
            case (Some(mod_shape), _) =>
              (Some(ushape.child = mod_shape), false)
            case (None, needs_ink_?) =>
              (None, needs_ink_?)
          }

        case comp_shape @ Composite_Shape(below, above) =>
          val final_shape =
            (add_missing_ink(below, pen_tform),
             add_missing_ink(above, pen_tform)) match {
              case ((Some(mod_below_shape), _), (Some(mod_above_shape), _)) =>
                Some(Composite_Shape(mod_below_shape, mod_above_shape))
              case ((Some(mod_below_shape), _), (None, true)) =>
                Some(Composite_Shape(mod_below_shape,
                                     Inked_Shape(above, default_shape_pen)))
              case ((Some(mod_below_shape), _), (None, false)) =>
                Some(comp_shape.left = mod_below_shape)
              case ((None, true), (Some(mod_above_shape), _)) =>
                Some(Composite_Shape(Inked_Shape(below, default_shape_pen),
                                     mod_above_shape))
              case ((None, false), (Some(mod_above_shape), _)) =>
                Some(comp_shape.right = mod_above_shape)
              case ((None, true), (None, true)) =>
                Some(Inked_Shape(comp_shape, default_shape_pen))
              case ((None, true), (None, false)) =>
                Some(comp_shape.left = Inked_Shape(below, default_shape_pen))
              case ((None, false), (None, true)) =>
                Some(comp_shape.right = Inked_Shape(above, default_shape_pen))
              case ((None, false), (None, false)) =>
                None
            }
          (final_shape, false)

        // NOTE: no need to ensure the [right] `clipping` (shape) has ink
        //     : add no ink to the [right] `mask` (shape), as it might interfere
        case bin_shape @ Binary_Shape_Op(left, right) =>
          add_missing_ink(left, pen_tform) match {
            case (Some(mod_shape), _) =>
              (Some(bin_shape.replace_left(mod_shape)), false)
            case (None, needs_ink_?) =>
              (None, needs_ink_?)
          }

        // NOTE: unlike 'true' shapes, ink Writing now, since using special kind
        case writing @ Writing(_) =>
          (Some(Inked_Shape(writing, default_writing_pen)), false)

        case Nullary_Shape_Op() => (None, true)
      }

    add_missing_ink(shape, None) match {
      case (Some(mod_shape), _) => mod_shape
      case (None, true)         => Inked_Shape(shape, default_shape_pen)
      case (None, false)        => shape
    }
  }

  protected final def render_shape(defs: Defs, os: Writer, shape: Drawing_Shape,
                                   attrs: Univ_Attrs = Univ_Attrs()): Defs = {
    def write_shape(s: Drawing_Shape, transforms: List[String]): Defs =
      s match {
        case Translated_Shape(shape, x_move, y_move) =>
          val transform = "translate(%f, %f)".format(x_move, y_move)
          write_shape(shape, transform :: transforms)
        case Scaled_Shape(shape, x_scaling, y_scaling) =>
          val transform = "scale(%f, %f)".format(x_scaling, y_scaling)
          write_shape(shape, transform :: transforms)
        case Rotated_Shape(shape, degrees, x_pivot, y_pivot) =>
          val transform = "rotate(%f, %f, %f)".format(degrees, x_pivot, y_pivot)
          write_shape(shape, transform :: transforms)
        case Reflected_Shape(shape, degrees, x_pivot, y_pivot) =>
          val transform = fmt_reflection_transform(degrees, x_pivot, y_pivot)
          write_shape(shape, transform :: transforms)
        case Skewed_Horiz_Shape(shape, degrees) =>
          val transform = "skewX(%f)".format(degrees)
          write_shape(shape, transform :: transforms)
        case Skewed_Vert_Shape(shape, degrees) =>
          val transform = "skewY(%f)".format(degrees)
          write_shape(shape, transform :: transforms)
        case transformed_shape if !transforms.isEmpty =>
          // NOTE: cons`ing to transforms on pre-order traversal reverses them
          write_transformed_shape(defs, os, transforms.reverse,
                                  transformed_shape, attrs)
        case non_transform_shape =>
          write_non_transformed_shape(non_transform_shape)
      }

    def write_non_transformed_shape(s: Drawing_Shape): Defs =
      s match {
        case composite_shape @ Composite_Shape(below, above) =>
          if (attrs.isDefined) // create group for attrs around entire composite
            write_group(defs, os, fmt(attrs).trim, composite_shape)
          else {
            val new_defs = render_shape(defs, os, below)
            render_shape(new_defs, os, above)
          }

        case Clipped_Shape(clipped, clipping, clip_rule) =>
          do_clipped_shape(defs, os, clipped, clipping, clip_rule, attrs)
        case Inked_Shape(shape, pen) =>
          do_ink(defs, os, shape, pen, attrs)

        case non_opaque_shape @ Non_Opaque_Shape(shape, opacity) =>
          if (clamp_opacity(opacity) == 1.0) // the default; skip writing value
            render_shape(defs, os, shape, attrs)
          else if (attrs.opacity.isDefined) // create group to write exist opcty
            write_group(defs, os, fmt(attrs).trim, non_opaque_shape)
          else // carry `opacity` until next draw_* invocation
            render_shape(defs, os, shape, attrs.copy(opacity = Some(opacity)))

        case Masked_Shape(masked, mask) =>
          do_masked_shape(defs, os, masked, mask, attrs)
        case Filtered_Shape(shape, filter) =>
          do_filtered_shape(defs, os, shape, filter, attrs)

        case attrib_shape @ Attributed_Shape(shape, attribution) =>
          attribution match {
            case Id_Attribution(id) if XML_Util.isName(id) =>
              if (attrs.id.isDefined) // create group to write before new `id`
                write_group(defs, os, fmt(attrs).trim, attrib_shape)
              else // carry `id` until next draw_* invocation
                render_shape(defs, os, shape, attrs.copy(id = Some(id)))
            case Id_Attribution(_) => // ignore mal-formed ids
              render_shape(defs, os, shape, attrs) // pass existing attrs if any
            case Link_Attribution(uri, target) =>
              do_link(defs, os, uri, target, shape, attrs)
          }

        case Free_Form(path) =>
          draw_path(defs, os, path, attrs)
        case Writing(text) =>
          draw_text(defs, os, text, attrs)
        case image @ Image(_, width, height) =>
          draw_image(defs, os, image.mapped_fpath, width, height, attrs)
  
        case Invis_Rectangle(width, height) =>
          // NOTE: no `attrs` for Invis_Rectangle--it's ID'ably invisible too!
          hide_invis_rectangle(defs, os, width, height)
  
        case true_shape: True_Drawing_Shape =>
          draw_path(defs, os, true_shape.as_path, attrs)
        case _ => // peferred to using @unchecked on match!
          // WARNING: mutual recursion via unconstrained (final) match holds
          // potential for infinite loop
          write_shape(shape, Nil)
      }
    write_shape(shape, Nil)
  }

  protected def define_gradient(defs: Defs, os: Writer,
                                id: String, gradient: Gradient): Defs = {

    def write_gradient_def(gradient: Gradient, transforms: List[String]):
        Defs = {

      def calc_spread_name(color_spread: Color_Spread): String =
        color_spread.toString.takeWhile(_ != '_').toLowerCase

      def calc_interp_name(color_interp: Color_Interpolation): String =
        color_interp match {
          // NOTE: since class name begins w/ lc, extractor'd be mistaken as val
          case `sRGB_Interpolation`     => "sRGB"
          case Linear_RGB_Interpolation => "linearRGB"
        }

      gradient match {
        case Translated_Gradient(grad, x_move, y_move) =>
          val transform = "translate(%f, %f)".format(x_move, y_move)
          write_gradient_def(grad, transform :: transforms)
        case Scaled_Gradient(grad, x_scaling, y_scaling) =>
          val transform = "scale(%f, %f)".format(x_scaling, y_scaling)
          write_gradient_def(grad, transform :: transforms)
        case Rotated_Gradient(grad, degrees, x_pivot, y_pivot) =>
          val transform = "rotate(%f, %f, %f)".format(degrees, x_pivot, y_pivot)
          write_gradient_def(grad, transform :: transforms)
        case Reflected_Gradient(grad, degrees, x_pivot, y_pivot) =>
          val transform = fmt_reflection_transform(degrees, x_pivot, y_pivot)
          write_gradient_def(grad, transform :: transforms)
        case Skewed_Horiz_Gradient(grad, degrees) =>
          val transform = "skewX(%f)".format(degrees)
          write_gradient_def(grad, transform :: transforms)
        case Skewed_Vert_Gradient(grad, degrees) =>
          val transform = "skewY(%f)".format(degrees)
          write_gradient_def(grad, transform :: transforms)
        case Linear_Gradient(color_stops, color_spread, color_interp) =>
          // NOTE: cons`ing to transforms on pre-order traversal reverses them
          val transform_attr =
            if (transforms.isEmpty) ""
            else transforms.reverse.mkString(" gradientTransform=\"", " ", "\"")
          os.write("    <linearGradient id=\"" + id + "\"" + transform_attr +
                   " spreadMethod=\"" + calc_spread_name(color_spread) + "\"" +
                   " color-interpolation=\"" + calc_interp_name(color_interp) +
                   "\">\n")
          val new_defs =
            (color_stops foldLeft defs){ render_color_stop(_, os, _) }
          os.write("    </linearGradient>\n")
          new_defs
        case Radial_Gradient(color_stops, color_spread, color_interp) =>
          // NOTE: cons`ing to transforms on pre-order traversal reverses them
          val transform_attr =
            if (transforms.isEmpty) ""
            else transforms.reverse.mkString(" gradientTransform=\"", " ", "\"")
          os.write("    <radialGradient id=\"" + id + "\"" + transform_attr +
                   " spreadMethod=\"" + calc_spread_name(color_spread) + "\"" +
                   " color-interpolation=\"" + calc_interp_name(color_interp) +
                   "\">\n")
          val new_defs =
            (color_stops foldLeft defs){ render_color_stop(_, os, _) }
          os.write("    </radialGradient>\n")
          new_defs
      }
    }
    write_gradient_def(gradient, Nil)
  }

  protected def render_color_stop(defs: Defs, os: Writer, cs: Color_Stop):
      Defs = {
    os.write(("      <stop offset=\"%f%%\" stop-color=\"%s\" " +
                          "stop-opacity=\"%f\"/>\n").format
                (cs.stop_offset_pct, fmt_color_name(cs.color),
                 clamp_opacity(cs.opacity)))
    defs
  }

  protected def define_pattern(defs: Defs, os: Writer,
                               id: String, pattern: Pattern): Defs = {

    def write_pattern_def(pattern: Pattern, transforms: List[String]): Defs = {
      pattern match {
        case Translated_Pattern(pat, x_move, y_move) =>
          val transform = "translate(%f, %f)".format(x_move, y_move)
          write_pattern_def(pat, transform :: transforms)
        case Scaled_Pattern(pat, x_scaling, y_scaling) =>
          val transform = "scale(%f, %f)".format(x_scaling, y_scaling)
          write_pattern_def(pat, transform :: transforms)
        case Rotated_Pattern(pat, degrees, x_pivot, y_pivot) =>
          val transform = "rotate(%f, %f, %f)".format(degrees, x_pivot, y_pivot)
          write_pattern_def(pat, transform :: transforms)
        case Reflected_Pattern(pat, degrees, x_pivot, y_pivot) =>
          val transform = fmt_reflection_transform(degrees, x_pivot, y_pivot)
          write_pattern_def(pat, transform :: transforms)
        case Skewed_Horiz_Pattern(pat, degrees) =>
          val transform = "skewX(%f)".format(degrees)
          write_pattern_def(pat, transform :: transforms)
        case Skewed_Vert_Pattern(pat, degrees) =>
          val transform = "skewY(%f)".format(degrees)
          write_pattern_def(pat, transform :: transforms)
        case Shape_Pattern(shape, width, height) =>
          // NOTE: cons`ing to transforms on pre-order traversal reverses them
          val transform_attr =
            if (transforms.isEmpty) ""
            else transforms.reverse.mkString(" patternTransform=\"", " ", "\"")
          os.write("    <pattern id=\"" + id + "\"" + transform_attr +
                   " width=\"%f\" height=\"%f\"".format(width, height) +
                   " patternUnits=\"userSpaceOnUse\">\n")
          val new_defs = render_shape(defs, os, shape)
          os.write("    </pattern>\n")
          new_defs
      }
    }
    write_pattern_def(pattern, Nil)
  }

  protected def define_clip_path(defs: Defs, os: Writer,
                                 id: String, clipping: Drawing_Shape,
                                 clip_rule: Clip_Rule): Defs = {
    val clip_rule_attr = "clip-rule=\"%s\"".format( clip_rule match {
      case Non_Zero_Clip => "nonzero"
      case Even_Odd_Clip => "evenodd"
      case Inherit_Clip  => "inherit"
    } )
    os.write("    <clipPath id=\"" + id + "\"" +
             " clipPathUnits=\"userSpaceOnUse\">\n")
    val new_defs = write_group(defs, os,
                               clip_rule_attr,
                               clipping)
    os.write("    </clipPath>\n")
    new_defs
  }

  protected def define_mask(defs: Defs, os: Writer,
                            id: String, mask: Drawing_Shape): Defs = {
    os.write("    <mask id=\"" + id + "\"" +
             " maskUnits=\"userSpaceOnUse\">\n")
    val new_defs = render_shape(defs, os, mask)
    os.write("    </mask>\n")
    new_defs
  }

  protected def define_filter(defs: Defs, os: Writer,
                              id: String, filter: Filter): Defs = {
    os.write("    <filter id=\"" + id + "\"" +
             " filterUnits=\"userSpaceOnUse\">\n")
    val new_defs = render_filter(defs, os, filter)
    os.write("    </filter>\n")
    new_defs
  }

  // NOTE: essentially a noop until next release, when Filter class hierarchy
  // elaborated (completed); may be overridden in this release by the Impatient
  protected def render_filter(defs: Defs, os: Writer, filter: Filter): Defs = {
    defs
  }

  protected def hide_invis_rectangle(defs: Defs, os: Writer,
                                     width: Double, height: Double): Defs = {
    os.write("  <!-- 'invisible' rectangle (%f x %f):\n".format(width, height))
    val unused_defs =
            draw_path(defs, os, Rectangle(width, height).as_path, Univ_Attrs())
    os.write("  -->\n")
    defs
  }

  protected final def draw_text(defs: Defs, os: Writer, text: Text,
                                attrs: Univ_Attrs): Defs = {

    def write_text_block(block: Text_Block) {

      val Rectangular(block_w, block_h) =
                block.text_bounding_box(text_ruler_factory)
      val upper_left: Point = (0 - block_w/2, 0 - block_h/2)
      val upper_right = upper_left -+ (block_w, 0)


      val calc_line_start = block.mode match {
        case LR_TB | Left_Right__Top_Bottom =>
          (align: Text_Align, line_w: Double, line_h: Double, prev_pt: Point) =>
            val x_offset = align match {
              case Start_Align  => 0 // parent Text_Block x-Start_Align`d for LR
              case Middle_Align => (block_w - line_w)/2
              case End_Align    =>  block_w - line_w
            }
            (upper_left.x + x_offset, prev_pt.y + line_h)
        case RL_TB | Right_Left__Top_Bottom =>
          (align: Text_Align, line_w: Double, line_h: Double, prev_pt: Point) =>
            val x_offset = align match {
              case Start_Align  => -block_w
              case Middle_Align => -block_w + (block_w - line_w)/2
              case End_Align    => -line_w
            }
            (upper_left.x + x_offset, prev_pt.y + line_h)
        case TB_RL | Top_Bottom__Right_Left =>
          (align: Text_Align, line_w: Double, line_h: Double, prev_pt: Point) =>
            val y_offset = align match {
              case Start_Align  => 0 // parent Text_Block y-Start_Align`d for TB
              case Middle_Align => (block_h - line_h)/2
              case End_Align    =>  block_h - line_h
            }
            (prev_pt.x - line_w, upper_left.y + y_offset)
      }


      def calc_line_w_offsets(prev: (Text_Line, Point, Ortho_Rectangle),
                              curr: (Text_Line, Ortho_Rectangle)):
          (Text_Line, Point, Ortho_Rectangle) = {
        val (_, prev_pt, _) = prev
        val (curr_line, curr_line_bb @ Rectangular(w, h)) = curr
        val line_start = calc_line_start(curr_line.align, w, h, prev_pt)
        (curr_line, line_start, curr_line_bb)
      }

      // NOTE: extra complication due to fact that <tspan>.{x,y} pos is baseline
      // (for Horizontal orientation--ceter-line, for Vertical orientation)
      // not text extent: descender chars (e.g. {p,g,j,...} still hang below
      // baseline for unbalanced appearance.  for simplicity, use simple, font-
      // independent ratio of 'ascender reach' (e.g. {l,k,(,...}) to 'descender
      // drop' of:
      //   3/4 (above baseline) :: 1/4 (below baseline)
      // then, to compensate for misaligned appearance when initial char is not
      // descender (no latin-1 caps are), split the difference to 1/8 down
      def balance_ascent_descent(line_w_offsets_w_bb:
                                   (Text_Line, Point, Ortho_Rectangle)):
          (Text_Line, Point) = {
        val (line, offset_point, Rectangular(bb_w, bb_h)) = line_w_offsets_w_bb
        (line, offset_point -+ (0, -(bb_h/8)))
      }

      // NOTE: analogous complication due to fact that <tspan>.{x,y} pos is
      // center-line (for Vertical orientation); simpler solution, here: shift
      // all chars left by half line width
      def shift_center_line(line_w_offsets_w_bb:
                              (Text_Line, Point, Ortho_Rectangle)):
          (Text_Line, Point) = {
        val (line, offset_point, Rectangular(bb_w, bb_h)) = line_w_offsets_w_bb
        (line, offset_point -+ (-(bb_w/2), 0))
      }


      val line_w_bbs = block.content map { line =>
                (line, line.text_bounding_box(text_ruler_factory, block.mode)) }
      // init. scan with a dummy line and starting_pt, later dropped w/ `tail`
      val (starting_pt, start_pt_compensation) = block.mode.orientation match {
        case Horizontal => (upper_left,  balance_ascent_descent _ )
        case Vertical   => (upper_right, shift_center_line      _ )
      }
      val dummy_init = (Text_Line("", Default_Font), starting_pt,
                        Origin_Ortho_Rectangle(1, 1): Ortho_Rectangle)
      val line_w_offsets =
            ((line_w_bbs scanLeft dummy_init) { calc_line_w_offsets(_, _) }).
              tail.map ( start_pt_compensation(_) )

      val layout_attrs = calc_block_layout_attrs(Start_Align, block.mode,
                                                 Some(upper_left.x),
                                                 Some(upper_left.y))
      os.write("    <text " + fmt(attrs) + layout_attrs.mkString(" ") + ">\n")
      for ((line, Point(x_offset, y_offset)) <- line_w_offsets) {
        write_text_line(line, x_offset, y_offset)
      }
      os.write("    </text>\n")
    }

    def write_text_line(line: Text_Line, x_offset: Double, y_offset: Double) {
      val layout_attrs =
              calc_line_layout_attrs(line.dir, Some(x_offset), Some(y_offset))
        os.write("      <tspan " + layout_attrs.mkString(" ") + ">\n")
        line.content.foreach { write_text_span(_, Nil) }
        os.write("      </tspan>\n")
    }

    def write_text_span(span: Text_Span, more_attrs: List[String]) {
      val preserve_spaces_? = !span.compress_spaces_?

      def write_basic_span(span: Basic_Text_Span) {
        val presentation_attrs =
              calc_presentation_attrs(span.font, span.decor, preserve_spaces_?)
        val attrs_str = (more_attrs ::: presentation_attrs).mkString(" ")
        val (span_start, span_content, span_end) = ("<tspan " + attrs_str + ">",
                                                    escape_xml_text(span.text),
                                                    "</tspan>")
        if (preserve_spaces_?) // take care to add no extra, unrequested spaces
          os.write(span_start + span_content + span_end)
        else // format to be more legible, perhaps for debugging
          os.write(  "        " + span_start + "\n" +
                      span_content           +
                   "\n        " + span_end   + "\n")
      }

      span match {
        case basic_span: Basic_Text_Span =>
          write_basic_span(basic_span)
        case Baseline_Shifted_Text_Span(child_span, shift) =>
          val attrs_str = (more_attrs ::: calc_shift_attrs(shift)).mkString(" ")
          val (span_start, span_end) = ("<tspan " + attrs_str + ">", "</tspan>")
          if (preserve_spaces_?) { // take care to add no extra spaces
            os.write(span_start)
            write_text_span(child_span, more_attrs) // pass attrs to child
            os.write(span_end)
          } else { // format to be more legible, perhaps for debugging
            os.write("        " + span_start + "\n")
            write_text_span(child_span, more_attrs) // pass attrs for child also
            os.write("        " + span_end + "\n")
          }
      }
    }

    val text_block = text match {
      case span:  Text_Span  => Text_Block(Text_Line(span))
      case line:  Text_Line  => Text_Block(line)
      case block: Text_Block => block
    }
    write_text_block(text_block)
    defs
  }

  private def bool(r: Rng_Def) =
    r.rng.nextBoolean

  protected def escape_xml_text(s: String): String = {
    val xml_reserved = "(?x)  (   <   |   &   |   >   ) ".r
    xml_reserved.replaceAllIn(s, re_match => re_match.group(1) match {
                                   case "<" => "&lt;"
                                   case "&" => "&amp;"
                                   case ">" => "&gt;"
                                   case _   => "" // defensive catch-all
                                 })
  }

  protected def calc_presentation_attrs(font: Font, decor: Text_Decoration,
                                        preserve_spaces_? : Boolean):
      List[String] = {

    val family_attr = "font-family=\"%s\"".format(font.family)
    val size_attr = font.size match {
      case Std_Size(size) => "font-size=\"%s\"".format(size)
    }
    val style_attr = "font-style=\"%s\"".format(font.style match {
      case Plain   => "normal"
      case Italic  => "italic"
      case Oblique => "oblique"
    })
    val weight_attr = "font-weight=\"%d\"".format(font.weight.css2_equiv)
    val decoration_attr = "text-decoration=\"%s\"".format(decor match {
      case No_Decoration => "none"
      case Underline     => "underline"
      case Overline      => "overline"
      case Strike        => "line-through"
      case Blinking      => "blink"
    })
    val spaces_attr = if (preserve_spaces_?) "xml:space=\"preserve\"" else ""

    List(spaces_attr, family_attr, size_attr, style_attr, weight_attr,
         decoration_attr) filter (_ != "")
  }

  protected def calc_shift_attrs(shift: Baseline_Shift): List[String] = {
    val shift_attr = "baseline-shift=\"%s\"".format(shift match {
      case Above_Baseline => "super"
      case Below_Baseline => "sub"
    } )

    List(shift_attr)
  }

  protected def calc_block_layout_attrs(align: Text_Align, mode: Writing_Mode,
                                        x_pos: Option[Double] = None,
                                        y_pos: Option[Double] = None):
      List[String] = {
    val anchor_attr = "text-anchor=\"%s\"".format(align match {
      case Start_Align  => "start"
      case Middle_Align => "middle"
      case End_Align    => "end"
    })
    val writing_mode_attr = "writing-mode=\"%s\"".format(mode match {
      case LR_TB | Left_Right__Top_Bottom => "lr-tb"
      case RL_TB | Right_Left__Top_Bottom => "rl-tb"
      case TB_RL | Top_Bottom__Right_Left => "tb-rl"
    })

    val x_attr = x_pos.map("x=\"%f\"".format(_)).getOrElse("")
    val y_attr = y_pos.map("y=\"%f\"".format(_)).getOrElse("")
    List(anchor_attr, writing_mode_attr, x_attr, y_attr) filter (_ != "")
  }

  protected def calc_line_layout_attrs(dir: Text_Direction,
                                       x_pos: Option[Double] = None,
                                       y_pos: Option[Double] = None):
      List[String] = {
    val direction_attr = "direction=\"%s\"".format(dir match {
      case L_To_R | Left_To_Right => "ltr"
      case R_To_L | Right_To_Left => "rtl"
    })
    val x_attr = x_pos.map("x=\"%f\"".format(_)).getOrElse("")
    val y_attr = y_pos.map("y=\"%f\"".format(_)).getOrElse("")
    List(x_attr, y_attr, direction_attr) filter (_ != "")
  }

  protected final def draw_image(defs: Defs, os: Writer, fpath: String,
                                 width: Double, height: Double,
                                 attrs: Univ_Attrs):
      Defs = {
    val upper_left: Point = (-width / 2, -height / 2)
    os.write(("  <image %sxlink:href=\"%s\" x=\"%f\" y=\"%f\" width=\"%f\" " +
                      "height=\"%f\"/>\n").format(fmt(attrs),
                                                  XML_Util.escape(fpath),
                                                  upper_left._1, upper_left._2,
                                                  width, height))
    defs
  }

  protected final def draw_path(defs: Defs, os: Writer, path: Path,
                                attrs: Univ_Attrs): Defs = {
    def encode_arc_choice(arc_kind: Arc_Choice): (Int, Int) =
      arc_kind match {
        case Small_CW  => (0, 1)
        case Large_CW  => (1, 1)
        case Small_CCW => (0, 0)
        case Large_CCW => (1, 0)
      }

    val path_cmds = path.get_cmds_backwards
    val path_cmd_strs = (path_cmds foldLeft (Nil: List[String])) {
      (cmd_strs, path_cmd) =>
        val path_cmd_str = path_cmd match {
          case Move_Abs(x, y) => "M" + x + "," + y
          case Move_Rel(x, y) => "m" + x + "," + y
          case Line_Abs(x, y) => "L" + x + "," + y
          case Line_Rel(x, y) => "l" + x + "," + y
          case Horizontal_Abs(x) => "H" + x
          case Horizontal_Rel(x) => "h" + x
          case Vertical_Abs(y) => "V" + y
          case Vertical_Rel(y) => "v" + y
          case Elliptical_Arc_Abs(rad_width, rad_height, x_rotate_degrees,
                                  arc_kind, x, y) =>
            val (large_arc_flag, pos_sweep_flag) = encode_arc_choice(arc_kind)
            "A" + rad_width + "," + rad_height + " " + x_rotate_degrees + " " +
              large_arc_flag + " " + pos_sweep_flag + " " + x + "," + y
          case Elliptical_Arc_Rel(rad_width, rad_height, x_rotate_degrees,
                                  arc_kind, x, y) =>
            val (large_arc_flag, pos_sweep_flag) = encode_arc_choice(arc_kind)
            "a" + rad_width + "," + rad_height + " " + x_rotate_degrees + " " +
              large_arc_flag + " " + pos_sweep_flag + " " + x + "," + y
          case Quad_Bezier_Abs(x_ctl1, y_ctl1, x, y) =>
            "Q" + x_ctl1 + "," + y_ctl1 + " " + x + "," + y
          case Quad_Bezier_Rel(x_ctl1, y_ctl1, x, y) =>
            "q" + x_ctl1 + "," + y_ctl1 + " " + x + "," + y
          case Tangent_Quad_Bezier_Abs(x, y) => "T" + x + "," + y
          case Tangent_Quad_Bezier_Rel(x, y) => "t" + x + "," + y
          case Cubic_Bezier_Abs(x_ctl1, y_ctl1, x_ctl2, y_ctl2, x, y) =>
            "C" + x_ctl1 +","+ y_ctl1 +" "+ x_ctl2 +","+ y_ctl2 +" "+ x +","+ y
          case Cubic_Bezier_Rel(x_ctl1, y_ctl1, x_ctl2, y_ctl2, x, y) =>
            "c" + x_ctl1 +","+ y_ctl1 +" "+ x_ctl2 +","+ y_ctl2 +" "+ x +","+ y
          case Tangent_Cubic_Bezier_Abs(x_ctl1, y_ctl1, x, y) =>
            "S" + x_ctl1 + "," + y_ctl1 + " " + x + "," + y
          case Tangent_Cubic_Bezier_Rel(x_ctl1, y_ctl1, x, y) =>
            "s" + x_ctl1 + "," + y_ctl1 + " " + x + "," + y
          case Close => if (defs?) "z" else "Z"
        }
        path_cmd_str :: cmd_strs
    }
    os.write(path_cmd_strs.mkString("  <path " + fmt(attrs) + "d=\"",
                                    " ",
                                    "\"/>\n"))
    defs
  }

  protected def do_clipped_shape(defs: Defs, os: Writer,
                                 clipped: Drawing_Shape,
                                 clipping: Drawing_Shape,
                                 clip_rule: Clip_Rule,
                                 attrs: Univ_Attrs): Defs = {
    val (url, new_defs) = defs.calc_clip_path_url(clipping, clip_rule)
    write_group(new_defs, os,
                "%sclip-path=\"url(%s)\"".format(fmt(attrs), url),
                clipped)
  }

  protected def do_ink(defs: Defs, os: Writer, shape: Drawing_Shape,
                       pen: Pen, attrs: Univ_Attrs): Defs = {
    def fmt_ink_attrs(ink: Ink, defs: Defs): (String, Option[Double], Defs) =
      ink match {
        // NOTE: must precede `c: Color` since unapply matches Non_Opaque_Color
        case Non_Opaque_Ink(ink, opacity) =>
          val (stroke_attr, None, new_defs) = fmt_ink_attrs(ink, defs)
          (stroke_attr, Some(opacity), new_defs)
        case Null_Ink    => ("none", None, defs)
        case c: Color    => (fmt_color_name(c), None, defs)
        case g: Gradient =>
          val (url, new_defs) = defs.calc_url(g)
          ("url(" + url + ")", None, new_defs)
        case p: Pattern  =>
          val (url, new_defs) = defs.calc_url(p)
          ("url(" + url + ")", None, new_defs)
      }

    // SVG default: stroke,fill="black"; tie default: stroke="black",fill="none"
    val (stroke, stroke_opacity, defs1) =
          pen.stroke.map( fmt_ink_attrs(_, defs) ).
               getOrElse( ("", None, defs) )
    val (fill, fill_opacity, defs2) =
          pen.fill.  map( fmt_ink_attrs(_, defs1) ).
               getOrElse( ("none", None, defs) )

    val stroke_attr = if (stroke == "") "" else "stroke=\"%s\"".format(stroke)
    val fill_attr   =                           "fill=\"%s\"".  format(fill)
    val stroke_opacity_attr =
          stroke_opacity.map( clamp_opacity(_) ).
                         map( "stroke-opacity=\"%f\"".format(_) ).getOrElse("")
    val fill_opacity_attr =
          fill_opacity.  map( clamp_opacity(_) ).
                         map( "fill-opacity=\"%f\"".format(_)   ).getOrElse("")

    val stroke_width_attr = pen.stroke_width match {
      case Some(width) => "stroke-width=\"%f\"".format(width)
      case None        => ""
    }

    val stroke_dash_pattern_attr = pen.stroke_dash_pattern match {
      case None
         | Some(Nil) => ""
      case Some(width :: Nil) =>
        "stroke-dasharray=\"%f %f\"".format(width, width)
      case Some(pattern_widths) =>
        "stroke-dasharray=\"%s\"".format(pattern_widths.mkString(" "))
    }
    val stroke_dash_offset_attr = pen.stroke_dash_offset match {
      case Some(dash_offset) => "stroke-dashoffset=\"%f\"".format(dash_offset)
      case None              => ""
    }

    val stroke_linecap_attr = pen.stroke_ends.map( _ match {
            case Exact_Ends   => "butt"
            case Round_Ends   => "round"
            case Extend_Ends  => "square"
            case Inherit_Ends => "inherit"
          } ).
            map( "stroke-linecap=\"%s\"".format(_) ).getOrElse("")
    val stroke_linejoin_attr = pen.stroke_corners.map( _ match {
            case Exact_Corners | Exact_Corners(None) => "miter"
            case Exact_Corners(Some(clip_under))     =>
              // cheat by returning value with one more attr and its value
              "miter\" stroke-miterlimit=\"%f".format(clip_under)
            case Round_Corners                       => "round"
            case Clip_Corners                        => "bevel"
            case Inherit_Corners                     => "inherit"
          } ).
            map( "stroke-linejoin=\"%s\"".format(_) ).getOrElse("")

    val fill_rule_attr = pen.fill_rule.map( _ match {
            case Non_Zero_Fill      => "nonzero"
            case Even_Odd_Fill      => "evenodd"
            case Winding_Count_Fill => "nonzero" // [SVG 2.0] "winding-count"
            case Inherit_Fill       => "inherit"
          } ).
            map( "fill-rule=\"%s\"".format(_) ).getOrElse("")

    write_group(defs2, os,
                List(fmt(attrs).trim, stroke_attr, fill_attr, stroke_width_attr,
                     stroke_dash_pattern_attr, stroke_dash_offset_attr,
                     stroke_linecap_attr, stroke_linejoin_attr,
                     stroke_opacity_attr, fill_opacity_attr, fill_rule_attr)
                  filter (_ != ""),
                shape)
  }

  private implicit def conv(visible: Visible_Area): Int =
    visible.hashCode

  protected def do_masked_shape(defs: Defs, os: Writer,
                                masked: Drawing_Shape,
                                mask: Drawing_Shape,
                                attrs: Univ_Attrs): Defs = {
    val (url, new_defs) = defs.calc_mask_url(mask)
    write_group(new_defs, os,
                "%smask=\"url(%s)\"".format(fmt(attrs), url),
                masked)
  }

  protected def do_filtered_shape(defs: Defs, os: Writer,
                                  shape: Drawing_Shape,
                                  filter: Filter,
                                  attrs: Univ_Attrs): Defs = {
    val (url, new_defs) = defs.calc_url(filter)
    write_group(new_defs, os,
                "%sfilter=\"url(%s)\"".format(fmt(attrs), url),
                shape)
  }

  protected def do_link(defs: Defs, os: Writer,
                        uri: String, target: Link_Target,
                        child: Drawing_Shape, attrs: Univ_Attrs): Defs = {
    val target_str = target match {
      case Target_Blank                         => "_blank"
      case Target_Replace                       => "_replace"
      case Target_Parent                        => "_parent"
      case Target_Top                           => "_top"
      case Target_Id(id) if XML_Util.isName(id) => id
      case Target_Self | _                      => "_self"
    }

    os.write("    <a xlink:href=\"%s\" target=\"%s\">\n".
                     format(XML_Util.escape(uri), target_str))
    // NOTE: apply `attrs` not to link, but to child element
    val new_defs = render_shape(defs, os, child, attrs)
    os.write("    </a>\n")
    new_defs
  }

  protected def write_transformed_shape(defs: Defs, os: Writer,
                                        ordered_transforms: List[String],
                                        shape: Drawing_Shape,
                                        attrs: Univ_Attrs): Defs = {
    def indent(s: String, level: Int) =
      ("  " * level) + s

    def indent_nesting(levels: Seq[String]) =
      levels.zipWithIndex.map( indent _ tupled )

    val group_opens = {
      val trans_grps = ordered_transforms.map("    <g transform=\""+ _ +"\">\n")
      if (attrs.isDefined) "    <g %s>\n".format(fmt(attrs).trim) :: trans_grps
      else                                                           trans_grps
    }
    os.write(group_opens.mkString(""))
    // os.write(indent_nesting(group_opens).mkString(""))
    val new_defs = render_shape(defs, os, shape)
    os.write("    </g>\n" * group_opens.length)
    // val group_closes = List.fill(group_opens.length)("    </g>\n")
    // os.write(indent_nesting(group_closes).reverse.mkString(""))
    new_defs
  }

  protected def write_group(defs: Defs, os: Writer, group_attrs: List[String],
                            child: Drawing_Shape): Defs = {
    os.write("    <g" + group_attrs.mkString(" ", " ", "") + ">\n")
    val new_defs = render_shape(defs, os, child)
    os.write("    </g>\n")
    new_defs
  }

  protected def write_group(defs: Defs, os: Writer, group_attr: String,
                            child: Drawing_Shape): Defs = {
    write_group(defs, os, List(group_attr), child)
  }


  protected def write_script_uri(os: Writer, script_type: Script_Type,
                                 uri: String) {
    os.write("    <script type=\"%s\" xlink:href=\"%s\"/>\n".format(
                                                       script_type.mime, 
                                                       XML_Util.escape(uri)))
  }

  protected def write_script_content(os: Writer, script_type: Script_Type,
                                     content: String) {
    os.write("    <script type=\"%s\"> <![CDATA[\n%s\n    ]]> </script>\n".
               format(script_type.mime, content))
  }


  protected def or_false(opt: Option[Boolean]): Boolean =
    opt.getOrElse(false)

  protected def fmt(attrs: Univ_Attrs): String =
    attrs.id.filter(XML_Util.isName(_)).map("id=\"" + _ + "\" ").getOrElse("") +
      attrs.opacity.map("opacity=\"" + clamp_opacity(_) + "\" ").getOrElse("")

  protected def fmt_color_name(color: Color): String =
    color match {
      case Named_Color(name, _, _, _) => name.toLowerCase
      case Described_Color(desc)      => desc
      case _                          => color.as_rgb_string
    }

  protected def fmt_reflection_transform(degrees: Double,
                                         about_x: Double, about_y: Double):
      String = {
    val (cos_val, sin_val) = (math.cos(2 * math.toRadians(degrees)),
                              math.sin(2 * math.toRadians(degrees)))
    "matrix(%f, %f, %f, %f, %f, %f)".format(cos_val, sin_val, sin_val, -cos_val,
                                            about_x, about_y)
  }

  protected def clamp_opacity(opacity: Double): Double =
    (opacity max 0.0) min 1.0

  private def fmt_view_box_debug_str(view_box: Visible_Area): String =
    "    <!-- viewBox: " + fmt_view_box_dims(view_box) + " -->\n"

  private def fmt_view_box_dims(view_box: Visible_Area): String =
    "(%f, %f) -+ %f x %f".format(view_box.upper_left.x, view_box.upper_left.y,
                                 view_box.width,        view_box.height)

  private def render_license_stamp(defs: Defs, os: Writer,
                                   view_box: Visible_Area) {

    def fmt_stamp_text: Text = {
      val stamp_font = Font("Arial", 12)
      val (license_str, date_str) = this.formatted_license_timestamp
      val caption_lines = "Prepared by tie, " + Version + ":" ::
                          "  " + Version.info_url             ::
                          "rendered: " + date_str             ::
                          "under " + license_str + " // see:" ::
                          "  " + Version.license_url          :: Nil
      Text_Block(caption_lines, stamp_font, false, 1.4).content :::
      Text_Block("   " + fmt_view_box_dims(view_box), stamp_font * .6, false, 3)
    }

    def place_stamp(stamp: Drawing_Shape, size_to_view_box_ratio: Double,
                    lower_left_offset_to_view_box_ratio: Double):
        Drawing_Shape = {
      val Rectangular(w, h) = stamp.bounding_box
      val (view_w, view_h) = (view_box.width, view_box.height)
      val view_lower_left = view_box.upper_left -+ (0, view_h)

      val avg_dims = ((view_w + view_h)/2) max (10.0 / size_to_view_box_ratio)
      val small_dim_targ_size = avg_dims * size_to_view_box_ratio

      val lower_left_offset   = avg_dims * lower_left_offset_to_view_box_ratio

      val oriented_stamp = (view_w < view_h, w < h) match {
        case (true,  true)  => stamp
        case (true,  false) => stamp -% -90
        case (false, false) => stamp
        case (false, true)  => stamp -% -90
      }
      val scaled_stamp = oriented_stamp -* (small_dim_targ_size / (w min h))

      val Rectangular(scaled_w, scaled_h) = scaled_stamp.bounding_box
      val stamp_targ_center_pt = view_lower_left -+
                                   (lower_left_offset, -lower_left_offset) -+
                                   (scaled_w/2, -scaled_h/2)
      scaled_stamp -+@ stamp_targ_center_pt
    }

    val stamp_text = fmt_stamp_text
    val Rectangular(w, h) = stamp_text.text_bounding_box(text_ruler_factory)
    val stamp_caption = stamp_text -~ default_writing_pen
    val (stroke_color, fill_color) = license_string match {
      case Some(named) => (Named_Colors.Lavender,    Named_Colors.Papaya_Whip)
      case None        => (Named_Colors.Papaya_Whip, Named_Colors.Lavender)
    }
    val blot = Hexagon(w, w*1.15, h*1.8) -~ Pen(stroke_color, fill_color, 8)
    val stamp = place_stamp(blot -& stamp_caption, 1.0/16, 1.0/8) -# 0.0
    render_shape(defs, os, license_stamp_id -: stamp)
  }
}

}
