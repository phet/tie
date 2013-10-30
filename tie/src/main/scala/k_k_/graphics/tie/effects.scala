/*
   file: k_k_/graphics/tie/effects.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2013 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie

package effects {


sealed abstract class Effect

object Effect {
  implicit def fromDouble(value: Double) = Opacity(value)

  case class Opacity(value: Double) extends Effect
  object Opacity {
    implicit def fromDouble(value: Double) = Opacity(value)
  }

  case class Filtering(filter: Filter)  extends Effect
}


// WARNING: open now, but intended to be `sealed`, once fully fleshed out!
abstract class Filter

object Filter {

  implicit def toEffectFiltering(filter: Filter) = Effect.Filtering(filter)
}


}
