/*
   file: k_k_/graphics/tie/shapes/Point.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2013 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie.shapes

import k_k_.graphics.tie.transform.{Transformable, Placeable}


object Point {
  implicit def fromDoubles(xAndY: (Double, Double)) = Point(xAndY._1, xAndY._2)
  implicit def toDoubles(pt: Point): (Double, Double) = (pt.x, pt.y)
}

@SerialVersionUID(11610510158494648L)
final case class Point(x: Double, y: Double)
    extends Transformable[Point]
       with Placeable[Point] {

  def distance(p: Point): Double =
    math.sqrt((x - p.x)*(x - p.x) + (y - p.y)*(y - p.y))

  def difference(p: Point): (Double, Double) =
    (x - p.x, y - p.y)

  def -(p: Point): (Double, Double) = difference(p)


  def negate: Point = Point(-x, -y)

  def unary_- : Point = negate


  val centerPt = this


  def move(xDist: Double, yDist: Double): Point =
    Point(x + xDist, y + yDist)


  def scale(xScaling: Double, yScaling: Double): Point =
    Point(x * xScaling, y * yScaling)


  def rotate(degrees: Double, aboutX: Double, aboutY: Double): Point = {
    val translatedPt = move(-aboutX, -aboutY)
    val (cosVal, sinVal) =
        (math.cos(math.toRadians(degrees)),
         math.sin(math.toRadians(degrees)))
    val (newX, newY) =
        (translatedPt.x * cosVal - translatedPt.y * sinVal,
         translatedPt.x * sinVal + translatedPt.y * cosVal)
    Point(newX + aboutX, newY + aboutY)
  }


  def reflect(degrees: Double, aboutX: Double, aboutY: Double): Point = {
    val translatedPt = move(-aboutX, -aboutY)
    val (cosVal, sinVal) =
        (math.cos(2 * math.toRadians(degrees)),
         math.sin(2 * math.toRadians(degrees)))
    val (newX, newY) =
        (translatedPt.x * cosVal + translatedPt.y * sinVal,
         translatedPt.x * sinVal - translatedPt.y * cosVal)
    Point(newX + aboutX, newY + aboutY)
  }


  def skewHoriz(degrees: Double): Point =
    (x + y * math.tan(math.toRadians(degrees)), y)


  def skewVert(degrees: Double): Point =
    (x, y + x * math.tan(math.toRadians(degrees)))


  override def toString: String = "(%s, %s)".format(x, y)
}
