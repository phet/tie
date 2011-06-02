/*
   file: k_k_/graphics/tie/shapes/Point.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2011 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie.shapes

import k_k_.graphics.tie.transform.{Transformable, Placeable}


object Point {

  implicit def Doubles_to_Point(x_and_y: (Double, Double)) =
    Point(x_and_y._1, x_and_y._2)

  implicit def to_Doubles(pt: Point): (Double, Double) =
    (pt.x, pt.y)
}

@serializable @SerialVersionUID(11610510158494648L)
final case class Point(x: Double, y: Double)
    extends Transformable[Point]
       with Placeable[Point] {

  def distance(p: Point): Double =
    math.sqrt((x - p.x)*(x - p.x) + (y - p.y)*(y - p.y))

  def difference(p: Point): (Double, Double) =
    (x - p.x, y - p.y)

  def -(p: Point): (Double, Double) =
    difference(p)


  def negate: Point =
    Point(-x, -y)

  def unary_- : Point =
    negate


  val center_pt = this


  def move(x_dist: Double, y_dist: Double): Point =
    Point(x + x_dist, y + y_dist)


  def scale(x_scaling: Double, y_scaling: Double): Point =
    Point(x * x_scaling, y * y_scaling)


  def rotate(degrees: Double, about_x: Double, about_y: Double): Point = {
    val translated_pt = move(-about_x, -about_y)
    val (cos_val, sin_val) = (math.cos(math.toRadians(degrees)),
                              math.sin(math.toRadians(degrees)))
    val (new_x, new_y) = (translated_pt.x * cos_val - translated_pt.y * sin_val,
                          translated_pt.x * sin_val + translated_pt.y * cos_val)
    Point(new_x + about_x, new_y + about_y)
  }


  def reflect(degrees: Double, about_x: Double, about_y: Double): Point = {
    val translated_pt = move(-about_x, -about_y)
    val (cos_val, sin_val) = (math.cos(2 * math.toRadians(degrees)),
                              math.sin(2 * math.toRadians(degrees)))
    val (new_x, new_y) = (translated_pt.x * cos_val + translated_pt.y * sin_val,
                          translated_pt.x * sin_val - translated_pt.y * cos_val)
    Point(new_x + about_x, new_y + about_y)
  }


  def skew_horiz(degrees: Double): Point =
    (x + y * math.tan(math.toRadians(degrees)), y)


  def skew_vert(degrees: Double): Point =
    (x, y + x * math.tan(math.toRadians(degrees)))


  override
  def toString: String =
    "(" + x + ", " + y + ")"
}
