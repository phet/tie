/*
   file: k_k_/graphics/tie/effects.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie

package effects {


sealed abstract class Effect

final case class Opacity_Effect(opacity: Double)   extends Effect
final case class Filter_Effect(filter: Filter)     extends Effect

object Opacity {

  implicit def Opacity_Effect_from_Double(opacity: Double) =
    apply(opacity)

  def apply(opacity: Double) =
    Opacity_Effect(opacity)

  def unapply(opacity_effect: Opacity_Effect): Option[Double] =
    Some(opacity_effect.opacity)
}


// WARNING: open for initial release; intended to be `sealed`, once complete in
// second release!
abstract class Filter

object Filter {

  implicit def to_Filter_Effect(filter: Filter) =
    Filter_Effect(filter)
}

}
