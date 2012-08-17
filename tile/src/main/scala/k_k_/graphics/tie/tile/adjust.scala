/*
   file: k_k_/graphics/tie/tile/adjust.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie.tile

package adjust {

import k_k_.graphics.tie.shapes.{Bounding_Boxed, Point, Shape, Simple_Shape,
                                 Rectangular, Invis_Rectangle}

import k_k_.graphics.tie.tile.pos.Shape_Pos
import conversions._


sealed abstract class Scaling_Strategy

protected abstract class Scaling_Strategy_Construction[T <: Scaling_Strategy] {

  def apply(bboxed: Bounding_Boxed): T = {
    val Rectangular(width, height) = Bounding_Boxed(bboxed)
    apply(width, height)
  }

  def apply(width: Double, height: Double): T
}


case object Orig_Scale
    extends Scaling_Strategy


object Scale_To
    extends Scaling_Strategy_Construction[Scale_To]

case class Scale_To(width: Double, height: Double)
    extends Scaling_Strategy


object Scale_To_Asym
    extends Scaling_Strategy_Construction[Scale_To_Asym]

case class Scale_To_Asym(width: Double, height: Double)
    extends Scaling_Strategy


object Scale_To_Max
    extends Scaling_Strategy_Construction[Scale_To_Max]

case class Scale_To_Max(width: Double, height: Double)
    extends Scaling_Strategy


object Scale_To_Max_Asym
    extends Scaling_Strategy_Construction[Scale_To_Max_Asym]

case class Scale_To_Max_Asym(width: Double, height: Double)
    extends Scaling_Strategy


object Scale_To_Min
    extends Scaling_Strategy_Construction[Scale_To_Min]

case class Scale_To_Min(width: Double, height: Double)
    extends Scaling_Strategy


object Scale_To_Min_Asym
    extends Scaling_Strategy_Construction[Scale_To_Min_Asym]

case class Scale_To_Min_Asym(width: Double, height: Double)
    extends Scaling_Strategy


sealed abstract class Collection_Scaling_Strategy
    extends Scaling_Strategy

//??????????what is the largest/smallest???does it go by area, width or height????????
sealed abstract class Scale_On_Largest_Item  extends Collection_Scaling_Strategy
sealed abstract class Scale_On_Smallest_Item extends Collection_Scaling_Strategy

case object Scale_To_Largest
    extends Scale_On_Largest_Item
case class  Scale_To_Largest_Plus(width: Double, height: Double)
    extends Scale_On_Largest_Item
case class  Scale_To_Largest_Minus(width: Double, height: Double)
    extends Scale_On_Largest_Item

case object Scale_To_Largest_Asym
    extends Scale_On_Largest_Item
case class  Scale_To_Largest_Plus_Asym(width: Double, height: Double)
    extends Scale_On_Largest_Item
case class  Scale_To_Largest_Minus_Asym(width: Double, height: Double)
    extends Scale_On_Largest_Item

case object Scale_To_Smallest
    extends Scale_On_Smallest_Item
case class  Scale_To_Smallest_Plus(width: Double, height: Double)
    extends Scale_On_Smallest_Item
case class  Scale_To_Smallest_Minus(width: Double, height: Double)
    extends Scale_On_Smallest_Item

case object Scale_To_Smallest_Asym
    extends Scale_On_Smallest_Item
case class  Scale_To_Smallest_Plus_Asym(width: Double, height: Double)
    extends Scale_On_Smallest_Item
case class  Scale_To_Smallest_Minus_Asym(width: Double, height: Double)
    extends Scale_On_Smallest_Item


final case class Padding_Instruction(where: Bounding_Box_Pos,
                                     width: Double, height: Double)

object Padding_Strategy {

  implicit def from_Padding_Instruction(instruction: Padding_Instruction) =
    apply(instruction)

  implicit def from_Tuple4(padding: (Double, Double, Double, Double)) =
    new Padding_Strategy(padding._1, padding._2, padding._3, padding._4)

  def apply(left_padding: Double, right_padding: Double,
            top_padding: Double,  bottom_padding: Double) =
    new Padding_Strategy(left_padding,right_padding,top_padding,bottom_padding)

  def apply(instruction: Padding_Instruction) =
    new Padding_Strategy(instruction)

  def apply(uniform_padding: Double) =
    new Padding_Strategy(uniform_padding)

  def apply(horizontal_padding: Double, vertical_padding: Double) =
    new Padding_Strategy(horizontal_padding, vertical_padding)


  //!!!!!!! protected[Padding_Strategy] doesn't seem to work to allow access to the class Padding_Strategy--however, a similar thing does seem to work for protected[Free_Form] in shapes.scala!!!--is it because I'm trying to call it from a constructor in Padding_Strategy???  is it because the protected item in the Free_Form object is a class, not a def???!!!!!!!!
  protected[adjust] def formulate_instructions(l: Double, r: Double,
                                               t: Double, b: Double):
      List[Padding_Instruction] = {
    // using match/case may not be safe, due to floating-pt. imprecision
    if      (l <= 0.0 && r <= 0.0 && t <= 0.0 && b <= 0.0)
      Nil
    else if (            r <= 0.0 && t <= 0.0 && b <= 0.0)
      Padding_Instruction(Left_Middle, l, 0.0)   :: Nil
    else if (l <= 0.0 &&             t <= 0.0 && b <= 0.0)
      Padding_Instruction(Right_Middle, r, 0.0)  :: Nil
    else if (l <= 0.0 && r <= 0.0 &&             b <= 0.0)
      Padding_Instruction(Top_Middle, 0.0, t)    :: Nil
    else if (l <= 0.0 && r <= 0.0 && t <= 0.0            )
      Padding_Instruction(Bottom_Middle, 0.0, b) :: Nil
    else if (            r <= 0.0 &&             b <= 0.0)
      Padding_Instruction(Top_Left, l, t)        :: Nil
    else if (            r <= 0.0 && t <= 0.0            )
      Padding_Instruction(Bottom_Left, l, b)     :: Nil
    else if (l <= 0.0 &&                         b <= 0.0)
      Padding_Instruction(Top_Right, r, t)       :: Nil
    else if (l <= 0.0 &&             t <= 0.0            )
      Padding_Instruction(Bottom_Right, r, b)    :: Nil
    else {
      val (common_horiz, common_vert) = (math.max(0.0, math.min(l, r)),
                                         math.max(0.0, math.min(t, b)))
      Padding_Instruction(Center, common_horiz, common_vert) ::
      formulate_instructions(l - common_horiz, r - common_horiz,
                             t - common_vert,  b - common_vert)
    }

    /******essentialy, same algo, expressed as match/case, and, hence, unsafe,
           given FP imprecision:

      (l, r, t, b) match {
        case (0.0, 0.0, 0.0, 0.0) =>
          Nil

        case (_, 0.0, 0.0, 0.0) =>
          Padding_Instruction(Left_Middle, l, 0.0)   :: Nil
        case (0.0, _, 0.0, 0.0) =>
          Padding_Instruction(Right_Middle, r, 0.0)  :: Nil
        case (0.0, 0.0, _, 0.0) =>
          Padding_Instruction(Top_Middle, 0.0, t)    :: Nil
        case (0.0, 0.0, 0.0, _) =>
          Padding_Instruction(Bottom_Middle, 0.0, b) :: Nil

        // these next four can be dropped, since final case results in equiv.
        case (0.0, 0.0, _, _) if t == b =>
          Padding_Instruction(Left_Middle, 0.0, t)   :: Nil
        case (0.0, 0.0, _, _) =>
          val common_vert = t min b
          Padding_Instruction(Left_Middle, 0.0, common_vert) ::
          formulate_instructions(l, r, t - common_vert, b - common_vert)

        case (_, _, 0.0, 0.0) if l == r =>
          Padding_Instruction(Top_Middle, l, 0.0)    :: Nil
        case (_, _, 0.0, 0.0) =>
          val common_horiz = l min r
          Padding_Instruction(Top_Middle, common_horiz, 0.0) ::
          formulate_instructions(l - common_horiz, r - common_horiz, t, b)

        case (_, 0.0, _, 0.0) =>
          Padding_Instruction(Top_Left, l, t)        :: Nil
        case (_, 0.0, 0.0, _) =>
          Padding_Instruction(Bottom_Left, l, b)     :: Nil

        case (0.0, _, _, 0.0) =>
          Padding_Instruction(Top_Right, r, t)       :: Nil
        case (0.0, _, 0.0, _) =>
          Padding_Instruction(Bottom_Right, r, b)    :: Nil


        case _ =>
          val (common_horiz, common_vert) = (math.max(0.0, math.min(l, r))
                                             math.max(0.0, math.min(t, b)))
          Padding_Instruction(Center, common_horiz, common_vert) ::
          formulate_instructions(l - common_horiz, r - common_horiz,
                                 t - common_vert, b - common_vert)
      }
    */
  }
}

final class Padding_Strategy(val instructions: List[Padding_Instruction]) {

  def this(instruction: Padding_Instruction) =
    this(List(instruction))

  def this(horizontal_padding: Double, vertical_padding: Double) =
    this(Padding_Instruction(Center, horizontal_padding, vertical_padding))

  def this(uniform_padding: Double) =
    this(uniform_padding, uniform_padding)

  def this(left_padding: Double, right_padding: Double,
           top_padding: Double,  bottom_padding: Double) =
    this(Padding_Strategy.
           formulate_instructions(left_padding, right_padding,
                                  top_padding,  bottom_padding).reverse)
}


//!!!!!!!!!!!!!!!move to another package!!!!!!!!!!!!
//???????????does it even belong in tide/tidee????????????

import k_k_.graphics.tie.ink.Pen

/*  moved to tile.conversions package object:

object Renderable_Shape {

  implicit def Shape_to_Renderable_Shape(shape: Shape) =
    new Renderable_Shape(shape)
}
*/
class Renderable_Shape(self: Shape) {

  def under_bounding_box(bbox_pen: Pen): Shape =
    under_bounding_box(Some(bbox_pen))

  def under_bounding_box(bbox_pen: Option[Pen] = None): Shape =
    bbox_pen match {
      case Some(pen) => self -& (self.bounding_box_shape -~ pen)
      case None      => self -&  self.bounding_box_shape
    }

  def over_bounding_box(bbox_pen: Pen): Shape =
    over_bounding_box(Some(bbox_pen))

  def over_bounding_box(bbox_pen: Option[Pen] = None): Shape =
    bbox_pen match {
      case Some(pen) => (self.bounding_box_shape -~ pen) -& self
      case None      =>  self.bounding_box_shape         -& self
    }

  def with_bounding_box(bbox_pen: Pen): Shape =
    under_bounding_box(Some(bbox_pen))

  def with_bounding_box(bbox_pen: Option[Pen] = None): Shape =
    under_bounding_box(bbox_pen)


  // aliases:
  def under_bbox(bbox_pen: Pen) = under_bounding_box(bbox_pen)
  def under_bbox(bbox_pen: Option[Pen] = None) = under_bounding_box(bbox_pen)

  def over_bbox(bbox_pen: Pen) = over_bounding_box(bbox_pen)
  def over_bbox(bbox_pen: Option[Pen] = None) = over_bounding_box(bbox_pen)

  def with_bbox(bbox_pen: Pen) = with_bounding_box(bbox_pen)
  def with_bbox(bbox_pen: Option[Pen] = None) = with_bounding_box(bbox_pen)
}



/*
object Expandable_Shape {

  implicit def Simple_Shape_to_Expandable_Shape(shape: Simple_Shape) =
    new Expandable_Shape(shape)
}

class Expandable_Shape(self: Simple_Shape) {

  import k_k_.graphics.tie.shapes.{

  def expand_to(scaling_strategy: Scaling_Strategy): Simple_Shape = {

Iso_Triangle(val base_width: Double, val height: Double)
  Equi_Triangle(length: Double)
Right_Triangle(base_width: Double, height: Double)
Rectangle(val width: Double, val height: Double)
  Square(length: Double)
    Invis_Rectangle(w: Double, h: Double)
Parallelogram(side_width: Double, full_width: Double,
                               height: Double)
Trapezoid(top_width: Double, bottom_width: Double,
                           height: Double)
Pentagon(val side_width: Double, val full_width: Double,
                      val side_height: Double, val full_height: Double)
  Reg_Pentagon(shape_width: Double)
Hexagon(val side_width: Double, val full_width: Double,
                     val height: Double)
  Reg_Hexagon(shape_width: Double)
Octagon(val side_width: Double, val full_width: Double,
                     val side_height: Double, val full_height: Double)
  Reg_Octagon(shape_width: Double)
Ellipse(val rad_width: Double, val rad_height: Double)
    Diam_Ellipse(diam_width: Double, diam_height: Double)
  Circle(val rad: Double)
    Diam_Circle(diam: Double)

Translated_Simple_Shape
Scaled_Simple_Shape
Rotated_Simple_Shape


  def expand_up_combo(over: Shape): Shape =
    self.expand_to(Scale_To_Min(over)) combo over

  def expand_up_asym_combo(over: Shape): Shape =
    self.expand_to(Scale_To_Min_Asym(over)) combo over

  def expand_down_combo(over: Shape): Shape =
    self.expand_to(Scale_To_Max(over)) combo over

  def expand_down_asym_combo(over: Shape): Shape =
    self.expand_to(Scale_To_Max_Asym(over)) combo over
}
*/


object Adjustable_Shape {

  def calc_scaling_factors(shape: Bounding_Boxed,
                           scaling_strategy: Scaling_Strategy):
      (Double, Double) = {
    val Rectangular(shape_width, shape_height) = Bounding_Boxed(shape)
    scaling_strategy match {
      case Orig_Scale =>
        (1.0, 1.0)
      case Scale_To(width, height) =>
        val (scale_x, scale_y) = (width  / shape_width,
                                  height / shape_height)
        val min_scale = scale_x min scale_y
        (min_scale, min_scale)
      case Scale_To_Asym(width, height) =>
        val (scale_x, scale_y) = (width  / shape_width,
                                  height / shape_height)
        (scale_x, scale_y)
      case Scale_To_Max(width, height) =>
        val (scale_x, scale_y) = (width  / shape_width,
                                  height / shape_height)
        val min_scale = scale_x min scale_y
        if (min_scale < 1.0)
          (min_scale, min_scale)
        else
          (1.0, 1.0)
      case Scale_To_Max_Asym(width, height) =>
        val (scale_x, scale_y) = (width  / shape_width,
                                  height / shape_height)
        (if (scale_x < 1.0) scale_x else 1.0,
         if (scale_y < 1.0) scale_y else 1.0)
      case Scale_To_Min(width, height) =>
        val (scale_x, scale_y) = (width  / shape_width,
                                  height / shape_height)
        val max_scale = scale_x max scale_y
        if (max_scale > 1.0)
          (max_scale, max_scale)
        else
          (1.0, 1.0)
      case Scale_To_Min_Asym(width, height) =>
        val (scale_x, scale_y) = (width  / shape_width,
                                  height / shape_height)
        (if (scale_x > 1.0) scale_x else 1.0,
         if (scale_y > 1.0) scale_y else 1.0)
      // since func calcs scale for one shape only, consider orig scale ok as is
      case _: Collection_Scaling_Strategy =>
        (1.0, 1.0)
    }
  }
}

final class Adjustable_Shape(self: Shape) {

  def recenter: Shape =
    self move_@ (0, 0)


  def to(other_bboxed: Bounding_Boxed, where_on_other: Bounding_Box_Pos,
         how: Alignment_Relation = Centered): Shape = {

    val other_bbox = Bounding_Boxed(other_bboxed)

    def calc_offset: (Double, Double) = {
      val bbox = self.bounding_box
      (how, where_on_other) match {
        case (Inside,   where) => where(bbox)          - where(other_bbox)
        case (Outside,  where) => where.opposite(bbox) - where(other_bbox)
        case (Centered, where) => bbox.center          - where(other_bbox)
      }
    }

    val (x_offset, y_offset) = calc_offset
    self.move(-x_offset, -y_offset)
  }

  def to(shape_pos: Shape_Pos, how: Alignment_Relation): Shape =
    to(shape_pos.shape, shape_pos.pos, how)

  def to(shape_pos: Shape_Pos): Shape =
    to(shape_pos.shape, shape_pos.pos)

  def -@(other_bboxed: Bounding_Boxed, where_on_other: Bounding_Box_Pos,
         how: Alignment_Relation = Centered): Shape =
    to(other_bboxed, where_on_other, how)

  def -@(shape_pos: Shape_Pos, how: Alignment_Relation): Shape =
    to(shape_pos, how)

  def -@(shape_pos: Shape_Pos): Shape =
    to(shape_pos)


  def scale_to(scaling_strategy: Scaling_Strategy): Shape = {
    val (x_factor, y_factor) =
        Adjustable_Shape.calc_scaling_factors(self.bounding_box,
                                              scaling_strategy)
    self.scale(x_factor, y_factor)
  }

  def scale_combo(scaling_strategizer: Shape => Scaling_Strategy)
                 (over: Shape): Shape =
    self.scale_to(scaling_strategizer(over)) combo over

  val scale_up_combo        = scale_combo( Scale_To_Min     (_) ) _
  val scale_up_asym_combo   = scale_combo( Scale_To_Min_Asym(_) ) _
  val scale_down_combo      = scale_combo( Scale_To_Max     (_) ) _
  val scale_down_asym_combo = scale_combo( Scale_To_Max_Asym(_) ) _

//??????????????
//  def -*&(over: Shape): Shape =
//    scale_up_combo(over)



  def pad(strategy: Padding_Strategy): Shape =
    (self /: strategy.instructions) { (shape, padding_instruction) =>
      shape.pad(padding_instruction)
    }

  def pad(instruction: Padding_Instruction): Shape =
    pad(instruction.where, instruction.width, instruction.height)

  def pad(left_padding: Double, right_padding: Double,
          top_padding: Double,  bottom_padding: Double): Shape =
    pad(new Padding_Strategy(left_padding, right_padding,
                             top_padding,  bottom_padding))

  // uses same 'counter-clockwise-from-top' param order as W3C CSS
  def pad_css(top_padding: Double, right_padding: Double,
              bottom_padding: Double,  left_padding: Double): Shape =
    pad(left_padding, right_padding, top_padding,  bottom_padding)

  def pad(uniform_padding: Double): Shape =
    pad(Center, uniform_padding, uniform_padding)

  def pad(where: Bounding_Box_Pos, uniform_padding: Double): Shape =
    pad(where, uniform_padding, uniform_padding)

  def pad(width: Double, height: Double): Shape =
    pad(Center, width, height)

  def pad(where: Bounding_Box_Pos, width: Double, height: Double): Shape = {
    def calc_new_bbox_size: (Double, Double) = {
      val (width_increase, height_increase) = where match {
          case Center                => (width * 2, height * 2)
          case Top_Middle    | T_Mid => (width * 2, height)
          case Bottom_Middle | B_Mid => (width * 2, height)
          case Left_Middle   | L_Mid => (width,     height * 2)
          case Right_Middle  | R_Mid => (width,     height * 2)
          case Top_Left      | T_L   => (width,     height)
          case Top_Right     | T_R   => (width,     height)
          case Bottom_Left   | B_L   => (width,     height)
          case Bottom_Right  | B_R   => (width,     height)
        }
      val Rectangular(self_width, self_height) = self.bounding_box
      (self_width + width_increase, self_height + height_increase)
    }

    val (new_bbox_width, new_bbox_height) = calc_new_bbox_size
    val invis_padding_rect = Invis_Rectangle(new_bbox_width, new_bbox_height).
                               to(self, where.opposite, Inside)
    invis_padding_rect -& self
  }
}

}


/***************************************************************************

[old version:
 (prior to defining Bounding_Box_Pos ITO Bounding_Box_Points)
]


    def calc_offset: (Double, Double) = {
      val bbox = shape.bounding_box
      (how, where) match {
        case (Inside, Center)         => bbox.center       - rect.center
        case (Inside, Top_Middle)
           | (Inside, T_Mid)          => bbox.top_middle   - rect.top_middle
        case (Inside, Bottom_Middle)
           | (Inside, B_Mid)          => bbox.bottom_middle- rect.bottom_middle
        case (Inside, Left_Middle)
           | (Inside, L_Mid)          => bbox.left_middle  - rect.left_middle
        case (Inside, Right_Middle)
           | (Inside, R_Mid)          => bbox.right_middle - rect.right_middle
        case (Inside, Top_Left)
           | (Inside, T_L)            => bbox.top_left     - rect.top_left
        case (Inside, Top_Right)
           | (Inside, T_R)            => bbox.top_right    - rect.top_right
        case (Inside, Bottom_Left)
           | (Inside, B_L)            => bbox.bottom_left  - rect.bottom_left
        case (Inside, Bottom_Right)
           | (Inside, B_R)            => bbox.bottom_right - rect.bottom_right

        case (Outside, Center)        => bbox.center       - rect.center
        case (Outside, Top_Middle)
           | (Outside, T_Mid)         => bbox.bottom_middle- rect.top_middle
        case (Outside, Bottom_Middle)
           | (Outside, B_Mid)         => bbox.top_middle   - rect.bottom_middle
        case (Outside, Left_Middle)
           | (Outside, L_Mid)         => bbox.right_middle - rect.left_middle
        case (Outside, Right_Middle)
           | (Outside, R_Mid)         => bbox.left_middle  - rect.right_middle
        case (Outside, Top_Left)
           | (Outside, T_L)           => bbox.bottom_right - rect.top_left
        case (Outside, Top_Right)
           | (Outside, T_R)           => bbox.bottom_left  - rect.top_right
        case (Outside, Bottom_Left)
           | (Outside, B_L)           => bbox.top_right    - rect.bottom_left
        case (Outside, Bottom_Right)
           | (Outside, B_R)           => bbox.top_left     - rect.bottom_right

        case (Centered, Center)       => bbox.center - rect.center
        case (Centered, Top_Middle)
           | (Centered, T_Mid)        => bbox.center - rect.top_middle
        case (Centered, Bottom_Middle)
           | (Centered, B_Mid)        => bbox.center - rect.bottom_middle
        case (Centered, Left_Middle)
           | (Centered, L_Mid)        => bbox.center - rect.left_middle
        case (Centered, Right_Middle)
           | (Centered, R_Mid)        => bbox.center - rect.right_middle
        case (Centered, Top_Left)
           | (Centered, T_L)          => bbox.center - rect.top_left
        case (Centered, Top_Right)
           | (Centered, T_R)          => bbox.center - rect.top_right
        case (Centered, Bottom_Left)
           | (Centered, B_L)          => bbox.center - rect.bottom_left
        case (Centered, Bottom_Right)
           | (Centered, B_R)          => bbox.center - rect.bottom_right
      }
    }
******************************************************************************/
