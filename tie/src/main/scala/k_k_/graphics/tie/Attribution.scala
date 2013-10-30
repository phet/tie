/*
   file: k_k_/graphics/tie/Atribution.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2013 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie

import java.net.URI


object Attribution {
  implicit def fromString(id: String) = IdAttribution(id)
}

sealed abstract class Attribution


object IdAttribution {
  implicit def fromString(id: String) = IdAttribution(id)
}

case class IdAttribution(id: String) extends Attribution


object LinkAttribution {
  def apply(uri: URI, target: LinkTarget) = new LinkAttribution(uri, target)

  def apply(uri: URI) = new LinkAttribution(uri)
}

case class LinkAttribution(uri: String, target: LinkTarget = LinkTarget.Self)
    extends Attribution {

  def this(uri: URI, target: LinkTarget) = this(uri.toString, target)

  def this(uri: URI) = this(uri.toString)
}

object Link {

  def apply(uri: String, target: LinkTarget = LinkTarget.Self) =
    new LinkAttribution(uri, target)

  def apply(uri: URI, target: LinkTarget) = new LinkAttribution(uri, target)

  def apply(uri: URI) = new LinkAttribution(uri)

  def unapply(linkAttribution: LinkAttribution): Some[(String, LinkTarget)] =
    Some(linkAttribution.uri, linkAttribution.target)
}


sealed abstract class LinkTarget
object LinkTarget {
  case object Blank          extends LinkTarget
  case object Replace        extends LinkTarget
  case object Self           extends LinkTarget
  case object Parent         extends LinkTarget
  case object Top            extends LinkTarget
  case class  Id(id: String) extends LinkTarget
}
