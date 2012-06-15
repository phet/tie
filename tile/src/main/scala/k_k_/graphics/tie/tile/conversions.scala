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

import k_k_.graphics.tie.shapes.Drawing_Shape


package object conversions {

  implicit def Drawing_Shape_to_Adjustable_Drawing_Shape(shape: Drawing_Shape):
      adjust.Adjustable_Drawing_Shape =
    new adjust.Adjustable_Drawing_Shape(shape)

  implicit def Drawing_Shape_to_Positionable_Drawing_Shape(shape:Drawing_Shape):
      pos.Positionable_Drawing_Shape =
    new pos.Positionable_Drawing_Shape(shape)



//!!!!!!!!!!Renderable_Drawing_Shape will likely change packages!!!!!!!!
  implicit def Drawing_Shape_to_Renderable_Drawing_Shape(shape: Drawing_Shape) =
    new adjust.Renderable_Drawing_Shape(shape)



  implicit def Traversable_to_Collected_Shapes[C[X] <: Traversable[X]](
                                                     shapes: C[Drawing_Shape]) =
    new adjust.Collected_Shapes[C](shapes)

  implicit def Iterator_to_Collected_Shapes(shapes_it:
                                                Iterator[Drawing_Shape]) =
    Traversable_to_Collected_Shapes(shapes_it.toSeq)
}
