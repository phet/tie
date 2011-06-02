/*
   file: k_k_/graphics/tie/shapes/path.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2011 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie.shapes

package path {

import k_k_.graphics.tie.transform.{Transformable, Placeable}


sealed abstract class Path_Cmd extends Transformable[Path_Cmd] {

  protected val end_pt: Point

  protected val End_Pt_Ctor: (Point) => Path_Cmd

  def move(x_dist: Double, y_dist: Double) =
    End_Pt_Ctor(end_pt move (x_dist, y_dist))

  def scale(x_scaling: Double, y_scaling: Double) =
    End_Pt_Ctor(end_pt scale (x_scaling, y_scaling))

  def rotate(degrees: Double, about_x: Double, about_y: Double) =
    End_Pt_Ctor(end_pt rotate (degrees, about_x, about_y))

  def reflect(degrees: Double, about_x: Double, about_y: Double) =
    End_Pt_Ctor(end_pt reflect (degrees, about_x, about_y))

  def skew_horiz(degrees: Double) =
    End_Pt_Ctor(end_pt skew_horiz (degrees))

  def skew_vert(degrees: Double) =
    End_Pt_Ctor(end_pt skew_vert (degrees))
}


sealed abstract class Arc_Choice
case object Small_CW  extends Arc_Choice
case object Large_CW  extends Arc_Choice
case object Small_CCW extends Arc_Choice
case object Large_CCW extends Arc_Choice


sealed trait Path_Commandable {

  protected def ::(path_cmd: Path_Cmd): Path

  protected def :::(path: Path): Path


  def subpath(path: Path): Path =
    path ::: this

  def subpath(pathable: { def as_path: Path }): Path =
    subpath(pathable.as_path)

  def &(path: Path): Path =
    subpath(path)

  def &(pathable: { def as_path: Path }): Path =
    subpath(pathable)


  // NOTE: the name `move` is already inherited by `Path` from `Transformable`;
  // in addition it feels ambiguous as to whether 'path pen' would be up or down

  def jump(x: Double, y: Double): Path =
    Move_Rel(x, y) :: this

  def jump(pt: Point): Path = jump(pt.x, pt.y)

  def jump_@(x: Double, y: Double): Path =
    Move_Abs(x, y) :: this

  def jump_@(pt: Point): Path = jump_@(pt.x, pt.y)

  // NOTE: `jump_@` is cannonical spelling, yet `jump_abs` provided for use from
  // languages where `@` is not identifier char; analogous correlates follow...

  def jump_abs(x: Double, y: Double): Path = jump_@(x, y)

  def jump_abs(pt: Point): Path = jump_@(pt.x, pt.y)


  def line(x: Double, y: Double): Path =
    Line_Rel(x, y) :: this

  def line(pt: Point): Path = line(pt.x, pt.y)

  def line_@(x: Double, y: Double): Path =
    Line_Abs(x, y) :: this

  def line_@(pt: Point): Path = line_@(pt.x, pt.y)

  def line_abs(x: Double, y: Double): Path = line_@(x, y)

  def line_abs(pt: Point): Path = line_@(pt.x, pt.y)


  def horizontal(x: Double): Path =
    Horizontal_Rel(x) :: this

  def horiz(x: Double): Path = horizontal(x)

  def horizontal_@(x: Double): Path =
    Horizontal_Abs(x) :: this

  def horizontal_abs(x: Double): Path = horizontal_@(x)

  def horiz_@(x: Double): Path = horizontal_@(x)

  def horiz_abs(x: Double): Path = horizontal_@(x)


  def vertical(y: Double): Path =
    Vertical_Rel(y) :: this

  def vert(y: Double): Path = vertical(y)

  def vertical_@(y: Double): Path =
    Vertical_Abs(y) :: this

  def vertical_abs(y: Double): Path = vertical_@(y)

  def vert_@(y: Double): Path = vertical_@(y)

  def vert_abs(y: Double): Path = vertical_@(y)


  def arc(rad_width: Double, rad_height: Double, x_rotate_degrees: Double,
          kind: Arc_Choice, x: Double, y: Double): Path =
    Elliptical_Arc_Rel(rad_width, rad_height, x_rotate_degrees, kind, x, y) ::
      this

  def arc(rad_width: Double, rad_height: Double, x_rotate_degrees: Double,
          kind: Arc_Choice, pt: Point): Path =
    arc(rad_width, rad_height, x_rotate_degrees, kind, pt.x, pt.y)

  def arc_@(rad_width: Double, rad_height: Double, x_rotate_degrees: Double,
            kind: Arc_Choice, x: Double, y: Double): Path =
    Elliptical_Arc_Abs(rad_width, rad_height, x_rotate_degrees, kind, x, y) ::
      this

  def arc_@(rad_width: Double, rad_height: Double, x_rotate_degrees: Double,
            kind: Arc_Choice, pt: Point): Path =
    arc_@(rad_width, rad_height, x_rotate_degrees, kind, pt.x, pt.y)

  def arc_abs(rad_width: Double, rad_height: Double, x_rotate_degrees: Double,
              kind: Arc_Choice, x: Double, y: Double): Path =
    arc_@(rad_width, rad_height, x_rotate_degrees, kind, x, y)

  def arc_abs(rad_width: Double, rad_height: Double, x_rotate_degrees: Double,
              kind: Arc_Choice, pt: Point): Path =
    arc_@(rad_width, rad_height, x_rotate_degrees, kind, pt.x, pt.y)


  def arc(rad_width: Double, rad_height: Double, kind: Arc_Choice,
          x: Double, y: Double): Path =
    Elliptical_Arc_Rel(rad_width, rad_height, 0, kind, x, y) :: this

  def arc(rad_width: Double, rad_height: Double, kind: Arc_Choice, pt: Point):
      Path =
    arc(rad_width, rad_height, kind, pt.x, pt.y)

  def arc_@(rad_width: Double, rad_height: Double, kind: Arc_Choice,
            x: Double, y: Double): Path =
    Elliptical_Arc_Abs(rad_width, rad_height, 0, kind, x, y) :: this

  def arc_@(rad_width: Double, rad_height: Double, kind: Arc_Choice,
            pt: Point): Path =
    arc_@(rad_width, rad_height, kind, pt.x, pt.y)

  def arc_abs(rad_width: Double, rad_height: Double, kind: Arc_Choice,
              x: Double, y: Double): Path =
    arc_@(rad_width, rad_height, kind, x, y)

  def arc_abs(rad_width: Double, rad_height: Double, kind: Arc_Choice,
              pt: Point): Path =
    arc_@(rad_width, rad_height, kind, pt.x, pt.y)


  def arc(rad: Double, kind: Arc_Choice, x: Double, y: Double): Path =
    Elliptical_Arc_Rel(rad, rad, 0, kind, x, y) :: this

  def arc(rad: Double, kind: Arc_Choice, pt: Point): Path =
    arc(rad, kind, pt.x, pt.y)

  def arc_@(rad: Double, kind: Arc_Choice, x: Double, y: Double): Path =
    Elliptical_Arc_Abs(rad, rad, 0, kind, x, y) :: this

  def arc_@(rad: Double, kind: Arc_Choice, pt: Point): Path =
    arc_@(rad, kind, pt.x, pt.y)

  def arc_abs(rad: Double, kind: Arc_Choice, x: Double, y: Double): Path =
    arc_@(rad, kind, x, y)

  def arc_abs(rad: Double, kind: Arc_Choice, pt: Point): Path =
    arc_@(rad, kind, pt.x, pt.y)


  def quadratic(x_ctl1: Double, y_ctl1: Double, x: Double, y: Double):
      Quad_Bezier_Path =
    new Quad_Bezier_Path(Quad_Bezier_Rel(x_ctl1, y_ctl1, x, y) :: this)

  def quadratic(pt_ctl1: Point, pt: Point): Quad_Bezier_Path =
    quadratic(pt_ctl1.x, pt_ctl1.y, pt.x, pt.y)

  def quad(x_ctl1: Double, y_ctl1: Double, x: Double, y: Double):
      Quad_Bezier_Path =
    quadratic(x_ctl1, y_ctl1, x, y)

  def quad(pt_ctl1: Point, pt: Point): Quad_Bezier_Path =
    quadratic(pt_ctl1, pt)


  def quadratic_@(x_ctl1: Double, y_ctl1: Double, x: Double, y: Double):
      Quad_Bezier_Path =
    new Quad_Bezier_Path(Quad_Bezier_Abs(x_ctl1, y_ctl1, x, y) :: this)

  def quadratic_@(pt_ctl1: Point, pt: Point): Quad_Bezier_Path =
    quadratic_@(pt_ctl1.x, pt_ctl1.y, pt.x, pt.y)

  def quadratic_abs(x_ctl1: Double, y_ctl1: Double, x: Double, y: Double):
      Quad_Bezier_Path =
    quadratic_@(x_ctl1, y_ctl1, x, y)

  def quadratic_abs(pt_ctl1: Point, pt: Point): Quad_Bezier_Path =
    quadratic_@(pt_ctl1.x, pt_ctl1.y, pt.x, pt.y)


  def quad_@(x_ctl1: Double, y_ctl1: Double, x: Double, y: Double):
      Quad_Bezier_Path =
    quadratic_@(x_ctl1, y_ctl1, x, y)

  def quad_@(pt_ctl1: Point, pt: Point): Quad_Bezier_Path =
    quadratic_@(pt_ctl1, pt)

  def quad_abs(x_ctl1: Double, y_ctl1: Double, x: Double, y: Double):
      Quad_Bezier_Path =
    quadratic_@(x_ctl1, y_ctl1, x, y)

  def quad_abs(pt_ctl1: Point, pt: Point): Quad_Bezier_Path =
    quadratic_@(pt_ctl1, pt)


  def cubic(x_ctl1: Double, y_ctl1: Double, x_ctl2: Double, y_ctl2: Double,
            x: Double, y: Double): Cubic_Bezier_Path =
    new Cubic_Bezier_Path(Cubic_Bezier_Rel(x_ctl1, y_ctl1, x_ctl2, y_ctl2,
                                           x, y) :: this)

  def cubic(pt_ctl1: Point, pt_ctl2: Point, pt: Point): Cubic_Bezier_Path =
    cubic(pt_ctl1.x, pt_ctl1.y, pt_ctl2.x, pt_ctl2.y, pt.x, pt.y)

  def cubic_@(x_ctl1: Double, y_ctl1: Double, x_ctl2: Double, y_ctl2: Double,
              x: Double, y: Double): Cubic_Bezier_Path =
    new Cubic_Bezier_Path(Cubic_Bezier_Abs(x_ctl1, y_ctl1, x_ctl2, y_ctl2,
                                           x, y) :: this)

  def cubic_@(pt_ctl1: Point, pt_ctl2: Point, pt: Point): Cubic_Bezier_Path =
    cubic_@(pt_ctl1.x, pt_ctl1.y, pt_ctl2.x, pt_ctl2.y, pt.x, pt.y)

  def cubic_abs(x_ctl1: Double, y_ctl1: Double, x_ctl2: Double,y_ctl2: Double,
                  x: Double, y: Double): Cubic_Bezier_Path =
    cubic_@(x_ctl1, y_ctl1, x_ctl2, y_ctl2, x, y)

  def cubic_abs(pt_ctl1: Point, pt_ctl2: Point, pt: Point):Cubic_Bezier_Path =
    cubic_@(pt_ctl1.x, pt_ctl1.y, pt_ctl2.x, pt_ctl2.y, pt.x, pt.y)


  def close =
    Close :: this    
}


object Path extends Path_Commandable {

  def from(x: Double, y: Double): Path =
    new Path(Move_Rel(x, y))

  def from(pt: Point): Path = from(pt.x, pt.y)

  // NOTE: at Path start, when no prev. segment is present to provide the
  // current point, Move_Rel and Move_Abs would seem equivalent--and are.  yet,
  // when Paths are combined, the final command of first path suddenly provides
  // a current Point to the second; in this case, a Path-initial Move_Rel or
  // Move_Abs aquire distinct meaning.
  def from_@(x: Double, y: Double): Path =
    new Path(Move_Abs(x, y))

  def from_@(pt: Point): Path = from_@(pt.x, pt.y)

  // NOTE: `from_@` is cannonical spelling, yet `from_abs` provided for use from
  // languages where `@` is not identifier char

  def from_abs(x: Double, y: Double): Path = from_@(x, y)

  def from_abs(pt: Point): Path = from_@(pt)


  implicit def to_Free_Form(path: Path) =
    Free_Form(path)


  protected def ::(path_cmd: Path_Cmd): Path =
    path_cmd :: Path.from(0, 0)

  protected def :::(path: Path): Path =
    path ::: Path.from(0, 0)


  case class Pos_Memory(curr_pt: Point, sub_path_start_pt: Point) {

    import Point._

    def this(start_pt: Point) =
      this(start_pt, start_pt)

    def follow(cmd: Path_Cmd): Pos_Memory =
      cmd match {
        case Move_Abs(x, y)                        => start_sub_path_abs(x, y)
        case Line_Abs(x, y)                        => replace_pt_abs(x, y)
        case Horizontal_Abs(x)                     => replace_pt_horiz_abs(x)
        case Vertical_Abs(y)                       => replace_pt_vert_abs(y)
        case Elliptical_Arc_Abs(_, _, _, _, x, y)  => replace_pt_abs(x, y)
        case Quad_Bezier_Abs(_, _, x, y)           => replace_pt_abs(x, y)
        case Tangent_Quad_Bezier_Abs(x, y)         => replace_pt_abs(x, y)
        case Cubic_Bezier_Abs(_, _, _, _, x, y)    => replace_pt_abs(x, y)
        case Tangent_Cubic_Bezier_Abs(_, _, x, y)  => replace_pt_abs(x, y)
  
        case Move_Rel(x, y)                        => start_sub_path_rel(x, y)
        case Line_Rel(x, y)                        => replace_pt_rel(x, y)
        case Horizontal_Rel(x)                     => replace_pt_rel(x, 0)
        case Vertical_Rel(y)                       => replace_pt_rel(0, y)
        case Elliptical_Arc_Rel(_, _, _, _, x, y)  => replace_pt_rel(x, y)
        case Quad_Bezier_Rel(_, _, x, y)           => replace_pt_rel(x, y)
        case Tangent_Quad_Bezier_Rel(x, y)         => replace_pt_rel(x, y)
        case Cubic_Bezier_Rel(_, _, _, _, x, y)    => replace_pt_rel(x, y)
        case Tangent_Cubic_Bezier_Rel(_, _, x, y)  => replace_pt_rel(x, y)
  
        case Close                                 => close_sub_path
    }


    def start_sub_path_rel(x_rel: Double, y_rel: Double) =
      new Pos_Memory(calc_pt_rel(x_rel, y_rel))

    def start_sub_path_abs(x_abs: Double, y_abs: Double) =
      new Pos_Memory((x_abs, y_abs))

    def close_sub_path =
      new Pos_Memory(sub_path_start_pt)


    def replace_pt_rel(x_rel: Double, y_rel: Double) =
      Pos_Memory(calc_pt_rel(x_rel, y_rel), sub_path_start_pt)

    def replace_pt_abs(x_abs: Double, y_abs: Double) =
      Pos_Memory((x_abs, y_abs), sub_path_start_pt)

    def replace_pt_horiz_abs(x_abs: Double) =
      Pos_Memory((x_abs, curr_pt.y), sub_path_start_pt)

    def replace_pt_vert_abs(y_abs: Double) =
      Pos_Memory((curr_pt.x, y_abs), sub_path_start_pt)


    private def calc_pt_rel(x_rel: Double, y_rel: Double): Point =
      curr_pt move (x_rel, y_rel)
  }
}

// ctor called only by derived classes, and always w/ prev Path as `cmds` tail
sealed class Path protected (cmds: List[Path_Cmd])
    extends Path_Commandable
       with Transformable[Path]
       with Placeable[Path] {

  // invariant: called only within Path object, and always with Move_{Abs,Rel}
  private def this(cmd: Path_Cmd) =
    this(List(cmd))


  def get_cmds: List[Path_Cmd] =
    cmds.reverse

  // faster, but the List returned is in reverse 'turtle-graphics' order
  def get_cmds_backwards: List[Path_Cmd] =
    cmds

  // returns initialized Pos_Memory, and remaining get_cmds.**tail**
  def init_pos_memory: (Path.Pos_Memory, List[Path_Cmd]) = {
    val all_cmds = get_cmds
    val initial_pos_memory = all_cmds.head match {
      // since this is path-initial move, both rel and abs give same result
      case Move_Rel(x, y) => new Path.Pos_Memory((x, y))
      case Move_Abs(x, y) => new Path.Pos_Memory((x, y))
      case _ =>
        throw new RuntimeException("Path starting w/ non-move should be " +
                                   "impossible!\n[Path Cmds]:\n" + all_cmds)
    }
    (initial_pos_memory, all_cmds.tail)
  }


  // append reflection of existing path about prev. end point at angle `degrees`
  def reflection(degrees: Double): Path = {
    // NOTE: helpful that head (Move_{Abs,Rel}) dropped; reflect everything else
    val (initial_pos_memory, subsequent_cmds) = this.init_pos_memory
    val curr_pos = (initial_pos_memory /: subsequent_cmds)( _.follow(_) )
    this & (subsequent_cmds ::: Path.from(0, 0)).reflect(degrees,
                                                         curr_pos.curr_pt)
  }

  // WARNING: center_pt, used to implement move_@/move_abs/-+@ may be EXPENSIVE!
  def center_pt: Point =
    Free_Form(this).center_pt    


  def move(x_dist: Double, y_dist: Double): Path = {
    // while, technically, a rel `Path_Cmd` would be unchanged under `move`,
    // the special case of a path-initial Move_Rel does require movement
    def adjust_terminal_Move_Rel_map(list: List[Path_Cmd])
                                    (f: Path_Cmd => Path_Cmd): List[Path_Cmd] =
      list match {
        case Nil =>
          Nil
        case Move_Rel(x, y) :: Nil =>
          Move_Rel.tupled(Point(x, y) move (x_dist, y_dist)) :: Nil
        case x :: xs =>
          f(x) :: adjust_terminal_Move_Rel_map(xs)(f)
      }
    new Path(adjust_terminal_Move_Rel_map(cmds) { _.move(x_dist, y_dist) } )
  }

  def scale(x_scaling: Double, y_scaling: Double): Path =
    new Path(cmds map { _.scale(x_scaling, y_scaling) })

  def rotate(degrees: Double, about_x: Double, about_y: Double): Path =
    new Path(cmds map { _.rotate(degrees, about_x, about_y) })

  def reflect(degrees: Double, about_x: Double, about_y: Double): Path =
    new Path(cmds map { _.reflect(degrees, about_x, about_y) })

  def skew_horiz(degrees: Double): Path =
    new Path(cmds map { _.skew_horiz(degrees) })

  def skew_vert(degrees: Double): Path =
    new Path(cmds map { _.skew_vert(degrees) })


  protected def ::(path_cmd: Path_Cmd) =
    new Path(path_cmd :: cmds)

  protected def :::(path: Path) =
    new Path(path.get_cmds_backwards ::: cmds)


  private def :::(rev_cmds: List[Path_Cmd]) =
    (this /: rev_cmds)( _.::(_) ) // foldl revs `cmds`; `.` so `::` left-assoc
}

// NOTE: SVG 1.1 spec appears to allow tangent/'smooth-curveto' to follow
// any previous path command: http://www.w3.org/TR/SVG/paths.html#PathDataBNF;
// tie does *NOT*: tangent may only follow prev bezier cmd (incl. tangent) of
// same degree (viz. quadratic v. cubic)

final class Quad_Bezier_Path private (cmds: List[Path_Cmd])
    extends Path(cmds) {

  def this(p: Path) =
    this(p.get_cmds_backwards)

  def tangent(x: Double, y: Double): Quad_Bezier_Path =
    new Quad_Bezier_Path(Tangent_Quad_Bezier_Rel(x, y) :: cmds)

  def tangent(pt: Point): Quad_Bezier_Path =
    tangent(pt.x, pt.y)

  def tangent_@(x: Double, y: Double): Quad_Bezier_Path =
    new Quad_Bezier_Path(Tangent_Quad_Bezier_Abs(x, y) :: cmds)

  def tangent_@(pt: Point): Quad_Bezier_Path =
    tangent_@(pt.x, pt.y)

  // NOTE: `tangent_@` is cannonical spelling, yet `tangent_abs` provided for
  // use from languages where `@` is not identifier char

  def tangent_abs(x: Double, y: Double): Quad_Bezier_Path =
    tangent_@(x, y)

  def tangent_abs(pt: Point): Quad_Bezier_Path =
    tangent_@(pt)
}

final class Cubic_Bezier_Path private (cmds: List[Path_Cmd])
    extends Path(cmds) {

  def this(p: Path) =
    this(p.get_cmds_backwards)

  def tangent(x_ctl1: Double, y_ctl1: Double, x: Double, y: Double):
      Cubic_Bezier_Path =
    new Cubic_Bezier_Path(Tangent_Cubic_Bezier_Rel(x_ctl1, y_ctl1, x, y) ::
                          cmds)

  def tangent(pt_ctl1: Point, pt: Point): Cubic_Bezier_Path =
    tangent(pt_ctl1.x, pt_ctl1.y, pt.x, pt.y)

  def tangent_@(x_ctl1: Double, y_ctl1: Double, x: Double, y: Double):
      Cubic_Bezier_Path =
    new Cubic_Bezier_Path(Tangent_Cubic_Bezier_Abs(x_ctl1, y_ctl1, x, y) ::
                          cmds)

  def tangent_@(pt_ctl1: Point, pt: Point): Cubic_Bezier_Path =
    tangent_@(pt_ctl1.x, pt_ctl1.y, pt.x, pt.y)

  // NOTE: `tangent_@` is cannonical spelling, yet `tangent_abs` provided for
  // use from languages where `@` is not identifier char

  def tangent_abs(x_ctl1: Double, y_ctl1: Double, x: Double, y: Double):
      Cubic_Bezier_Path =
    tangent_@(x_ctl1, y_ctl1, x, y)

  def tangent_abs(pt_ctl1: Point, pt: Point): Cubic_Bezier_Path =
    tangent_@(pt_ctl1, pt)
}


sealed abstract class Rel_Path_Cmd extends Path_Cmd {

  override
  def move(x_dist: Double, y_dist: Double): Path_Cmd =
    this
}


case class Move_Abs(x: Double, y: Double)
    extends Path_Cmd {

  protected val end_pt: Point = (x, y)
  protected val End_Pt_Ctor = Move_Abs.tupled.compose(Point.to_Doubles)
}

case class Move_Rel(x: Double, y: Double)
    extends Rel_Path_Cmd {

  protected val end_pt: Point = (x, y)
  protected val End_Pt_Ctor = Move_Rel.tupled.compose(Point.to_Doubles)
}


case class Line_Abs(x: Double, y: Double)
    extends Path_Cmd {

  protected val end_pt: Point = (x, y)
  protected val End_Pt_Ctor = Line_Abs.tupled.compose(Point.to_Doubles)
}

case class Line_Rel(x: Double, y: Double)
    extends Rel_Path_Cmd {

  protected val end_pt: Point = (x, y)
  protected val End_Pt_Ctor = Line_Rel.tupled.compose(Point.to_Doubles)
}

case class Horizontal_Abs(x: Double)
    extends Path_Cmd {

  protected val end_pt: Point = (x, 0.0)
  protected val End_Pt_Ctor = (pt: Point) => Horizontal_Abs(pt.x)
}

case class Horizontal_Rel(x: Double)
    extends Rel_Path_Cmd {

  protected val end_pt: Point = (x, 0.0)
  protected val End_Pt_Ctor = (pt: Point) => Horizontal_Rel(pt.x)
}

case class Vertical_Abs(y: Double)
    extends Path_Cmd {

  protected val end_pt: Point = (0.0, y)
  protected val End_Pt_Ctor = (pt: Point) => Vertical_Abs(pt.y)
}

case class Vertical_Rel(y: Double)
    extends Rel_Path_Cmd {

  protected val end_pt: Point = (0.0, y)
  protected val End_Pt_Ctor = (pt: Point) => Vertical_Rel(pt.y)
}


case class Elliptical_Arc_Abs(rad_width: Double, rad_height: Double,
                              x_rotate_degrees: Double, kind: Arc_Choice,
                              x: Double, y: Double)
    extends Path_Cmd {

  protected val end_pt: Point = (x, y)
  protected val End_Pt_Ctor = (pt: Point) =>
                                 Elliptical_Arc_Abs(rad_width, rad_height,
                                                    x_rotate_degrees, kind,
                                                    pt.x, pt.y)

  override
  def scale(x_scaling: Double, y_scaling: Double) =
    Elliptical_Arc_Abs(rad_width * x_scaling, rad_height * y_scaling,
                       x_rotate_degrees, kind,
                       x * x_scaling, y * y_scaling)
}

case class Elliptical_Arc_Rel(rad_width: Double, rad_height: Double,
                              x_rotate_degrees: Double, kind: Arc_Choice,
                              x: Double, y: Double)
    extends Rel_Path_Cmd {

  protected val end_pt: Point = (x, y)
  protected val End_Pt_Ctor = (pt: Point) =>
                                 Elliptical_Arc_Rel(rad_width, rad_height,
                                                    x_rotate_degrees, kind,
                                                    pt.x, pt.y)

  override
  def scale(x_scaling: Double, y_scaling: Double) =
    Elliptical_Arc_Rel(rad_width * x_scaling, rad_height * y_scaling,
                       x_rotate_degrees, kind,
                       x * x_scaling, y * y_scaling)

//????????do any of the others need to be overridden too?????
}

/*
case class Circular_Arc_Abs(rad: Double, start_angle_degrees: Double,
                            sweep_angle_degrees: Double,
                            about_x: Double, about_y: Double)
*/


case class Quad_Bezier_Abs(x_ctl1: Double, y_ctl1: Double,
                           x: Double, y: Double)
    extends Path_Cmd {

  protected val end_pt: Point = (x, y)
  protected val End_Pt_Ctor = (pt: Point) =>
                                Quad_Bezier_Abs(x_ctl1, y_ctl1, pt.x, pt.y)

//!!!!!!!some of the other Transformable methods need to be overridden too!!!!!!
}

case class Quad_Bezier_Rel(x_ctl1: Double, y_ctl1: Double,
                           x: Double, y: Double)
    extends Rel_Path_Cmd {

  protected val end_pt: Point = (x, y)
  protected val End_Pt_Ctor = (pt: Point) =>
                                Quad_Bezier_Rel(x_ctl1, y_ctl1, pt.x, pt.y)

//!!!!!!!some of the other Transformable methods need to be overridden too!!!!!!
}

case class Tangent_Quad_Bezier_Abs(x: Double, y: Double)
    extends Path_Cmd {

  protected val end_pt: Point = (x, y)
  protected val End_Pt_Ctor = Tangent_Quad_Bezier_Abs.tupled.
                                                   compose(Point.to_Doubles)
}

case class Tangent_Quad_Bezier_Rel(x: Double, y: Double)
    extends Rel_Path_Cmd {

  protected val end_pt: Point = (x, y)
  protected val End_Pt_Ctor = Tangent_Quad_Bezier_Rel.tupled.
                                                   compose(Point.to_Doubles)
}


case class Cubic_Bezier_Abs(x_ctl1: Double, y_ctl1: Double,
                            x_ctl2: Double, y_ctl2: Double,
                            x: Double, y: Double)
    extends Path_Cmd {

  protected val end_pt: Point = (x, y)
  protected val End_Pt_Ctor = (pt: Point) =>
                                Cubic_Bezier_Abs(x_ctl1, y_ctl1, x_ctl2, y_ctl2,
                                                 pt.x, pt.y)

//!!!!!!!some of the other Transformable methods need to be overridden too!!!!!!
}

case class Cubic_Bezier_Rel(x_ctl1: Double, y_ctl1: Double,
                            x_ctl2: Double, y_ctl2: Double,
                            x: Double, y: Double)
    extends Rel_Path_Cmd {

  protected val end_pt: Point = (x, y)
  protected val End_Pt_Ctor = (pt: Point) =>
                                Cubic_Bezier_Rel(x_ctl1, y_ctl1, x_ctl2, y_ctl2,
                                                 pt.x, pt.y)

//!!!!!!!some of the other Transformable methods need to be overridden too!!!!!!
}

case class Tangent_Cubic_Bezier_Abs(x_ctl1: Double, y_ctl1: Double,
                                    x: Double, y: Double)
    extends Path_Cmd {

  protected val end_pt: Point = (x, y)
  protected val End_Pt_Ctor = (pt: Point) =>
                                Tangent_Cubic_Bezier_Abs(x_ctl1, y_ctl1,
                                                         pt.x, pt.y)

//!!!!!!!some of the other Transformable methods need to be overridden too!!!!!!
}

case class Tangent_Cubic_Bezier_Rel(x_ctl1: Double, y_ctl1: Double,
                                    x: Double, y: Double)
    extends Rel_Path_Cmd {

  protected val end_pt: Point = (x, y)
  protected val End_Pt_Ctor = (pt: Point) =>
                                Tangent_Cubic_Bezier_Rel(x_ctl1, y_ctl1,
                                                         pt.x, pt.y)

//!!!!!!!some of the other Transformable methods need to be overridden too!!!!!!
}


case object Close
    extends Rel_Path_Cmd {

  protected val end_pt: Point = (0.0, 0.0)       // dummy val...
  protected val End_Pt_Ctor = (_: Point) => this // ...since every tform == this
}

}
