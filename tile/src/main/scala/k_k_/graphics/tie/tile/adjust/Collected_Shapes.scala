/*
   file: k_k_/graphics/tie/tile/adjust/Collected_Shapes.scala

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

import scala.collection.generic.CanBuildFrom

import k_k_.graphics.tie.shapes.{Shape, Null_Shape}

import conversions._


//final case class Collected_Shapes[+T <: Traversable[Shape]](shapes: T)
final case class Collected_Shapes[C[X] <: Traversable[X]](shapes: C[Shape]) {

  def heap: Shape =
    (Null_Shape /: shapes) { _ -& _ }


  // NOTE: `shifting` is relative to each *subsequently* chained shape--not
  // the accumulating chain!
  def chain(on: Bounding_Box_Pos, shift: Alignment_Shift = Stationary): Shape = {
    val (under_pos, over_pos) = (shift.companion(on), shift(on.opposite))
    (Null_Shape /: shapes) { (s1, s2) => (under_pos of s1) -& (s2 @- over_pos) }
  }


//?????? what about using Collection_Scaling_Strategy??????????

  def scale_up_to_uniform(implicit bf: CanBuildFrom[C[Shape], Shape, C[Shape]]):
      C[Shape] = {
    val common_fit_bbox = Shape.common_fit_bounding_box(shapes)
    bf(shapes) ++=
      shapes.map( _.scale_to(Scale_To_Min_Asym(common_fit_bbox)) ) result
  }

  def scale_up_to_uniform_sym(implicit bf: CanBuildFrom[C[Shape], Shape,
                                                        C[Shape]]):
      C[Shape] = {
    val common_fit_bbox = Shape.common_fit_bounding_box(shapes)
    bf(shapes) ++=
      shapes.map( _.scale_to(Scale_To_Min(common_fit_bbox)) ) result
  }
}

}
