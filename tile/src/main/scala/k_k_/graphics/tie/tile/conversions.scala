/*
   file: k_k_/graphics/tie/tile/conversions.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie.tile

import k_k_.graphics.tie.shapes.Shape


package object conversions {
//!!!!!!!!!!Renderable_Shape will likely change packages!!!!!!!!
  import adjust.{Adjustable_Shape, Collected_Shapes, Renderable_Shape}
  import pos.Positionable_Shape

  implicit def Shape_to_Adjustable_Shape(shape: Shape): Adjustable_Shape =
    new Adjustable_Shape(shape)

  implicit def Shape_to_Positionable_Shape(shape:Shape): Positionable_Shape =
    new Positionable_Shape(shape)

  implicit def Shape_to_Renderable_Shape(shape: Shape): Renderable_Shape =
    new Renderable_Shape(shape)

  implicit def Traversable_to_Collected_Shapes[C[X] <: Traversable[X]](
      shapes: C[Shape]
    ): Collected_Shapes[C] =
    new Collected_Shapes[C](shapes)

  implicit def Iterator_to_Collected_Shapes(shapes_it: Iterator[Shape]):
      Collected_Shapes[Seq] =
    Traversable_to_Collected_Shapes(shapes_it.toSeq)
}
