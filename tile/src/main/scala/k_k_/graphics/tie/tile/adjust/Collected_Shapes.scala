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

import k_k_.graphics.tie.shapes.{Drawing_Shape, Null_Shape}

import conversions._


//final case class Collected_Shapes[+T <: Traversable[Drawing_Shape]](shapes: T)
final case class Collected_Shapes[C[X] <: Traversable[X]](shapes:
                                                            C[Drawing_Shape]) {

  // NOTE: `shifting` is relative to each new *subsequent* joining shape--not
  // the accumulating joined shape!
  def join(on: Bounding_Box_Pos, shifting: Alignment_Shift): Drawing_Shape = {
    val (under_pos, over_pos) = (shifting.companion(on), shifting(on.opposite))
    (Null_Shape /: shapes) { (s1, s2) =>
                               (under_pos of s1).align_under(s2, over_pos) }
  }

  def join(on: Bounding_Box_Pos): Drawing_Shape =
    (Null_Shape /: shapes) { (s1, s2) => (on of s1).align_under(s2, Outside) }


  def mapped_join(f: Drawing_Shape => Drawing_Shape,
                  on: Bounding_Box_Pos,
                  shifting: Alignment_Shift = Stationary):
      Drawing_Shape =
    copy(shapes = shapes map f).join(on, shifting)


//?????? what about using Collection_Scaling_Strategy??????????

  def scale_up_to_uniform(implicit bf: CanBuildFrom[C[Drawing_Shape],
                                                      Drawing_Shape,
                                                    C[Drawing_Shape]]):
      C[Drawing_Shape] = {
    val common_fit_bbox = Drawing_Shape.common_fit_bounding_box(shapes)
    bf(shapes) ++=
      shapes.map( _.scale_to(Scale_To_Min_Asym(common_fit_bbox)) ) result
  }

  def scale_up_to_uniform_sym(implicit bf: CanBuildFrom[C[Drawing_Shape],
                                                          Drawing_Shape,
                                                        C[Drawing_Shape]]):
      C[Drawing_Shape] = {
    val common_fit_bbox = Drawing_Shape.common_fit_bounding_box(shapes)
    bf(shapes) ++=
      shapes.map( _.scale_to(Scale_To_Min(common_fit_bbox)) ) result
  }
}

}
