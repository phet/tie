/*
   file: k_k_/graphics/tie/Atribution.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie

import java.net.URI


object Attribution {
  implicit def from_String(id: String) = Id_Attribution(id)
}

sealed abstract class Attribution


object Id_Attribution {
  implicit def from_String(id: String) = Id_Attribution(id)
}

final case class Id_Attribution(id: String) extends Attribution


object Link_Attribution {

  def apply(uri: URI, target: Link_Target) =
    new Link_Attribution(uri, target)

  def apply(uri: URI) =
    new Link_Attribution(uri)
}

final case class Link_Attribution(uri: String, target: Link_Target= Target_Self)
    extends Attribution {

  def this(uri: URI, target: Link_Target) =
    this(uri.toString, target)

  def this(uri: URI) =
    this(uri.toString)
}

object Link {

  def apply(uri: String) =
    new Link_Attribution(uri)

  def apply(uri: String, target: Link_Target = Target_Self) =
    new Link_Attribution(uri, target)

  def apply(uri: URI, target: Link_Target) =
    new Link_Attribution(uri, target)

  def apply(uri: URI) =
    new Link_Attribution(uri)

  def unapply(link_attribution: Link_Attribution): Some[(String, Link_Target)] =
    Some(link_attribution.uri, link_attribution.target)
}


sealed abstract class Link_Target
case object Target_Blank          extends Link_Target
case object Target_Replace        extends Link_Target
case object Target_Self           extends Link_Target
case object Target_Parent         extends Link_Target
case object Target_Top            extends Link_Target
case class  Target_Id(id: String) extends Link_Target
