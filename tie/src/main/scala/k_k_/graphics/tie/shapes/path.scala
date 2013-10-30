/*
   file: k_k_/graphics/tie/shapes/path.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2013 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie.shapes

package path {

import k_k_.graphics.tie.transform.{Transformable, Placeable}


sealed abstract class PathCmd extends Transformable[PathCmd] {

  protected val endPt: Point

  protected val EndPtCtor: (Point) => PathCmd

  def move(xDist: Double, yDist: Double) =
    EndPtCtor(endPt move (xDist, yDist))

  def scale(xScaling: Double, yScaling: Double) =
    EndPtCtor(endPt scale (xScaling, yScaling))

  def rotate(degrees: Double, aboutX: Double, aboutY: Double) =
    EndPtCtor(endPt rotate (degrees, aboutX, aboutY))

  def reflect(degrees: Double, aboutX: Double, aboutY: Double) =
    EndPtCtor(endPt reflect (degrees, aboutX, aboutY))

  def skewHoriz(degrees: Double) =
    EndPtCtor(endPt skewHoriz (degrees))

  def skewVert(degrees: Double) =
    EndPtCtor(endPt skewVert (degrees))
}


sealed abstract class ArcChoice
object ArcChoice {
  case object SmallCW  extends ArcChoice
  case object LargeCW  extends ArcChoice
  case object SmallCCW extends ArcChoice
  case object LargeCCW extends ArcChoice
}


sealed trait PathCommandable {

  protected def ::(pathCmd: PathCmd): Path

  protected def :::(path: Path): Path


  def subpath(path: Path): Path = path ::: this

  def subpath(pathable: { def asPath: Path }): Path = subpath(pathable.asPath)

  def &(path: Path): Path = subpath(path)

//!!!!!!!!!use a type class, rather than a structural type!!!!

  def &(pathable: { def asPath: Path }): Path = subpath(pathable)


  // NOTE: the name `move` is already inherited by `Path` from `Transformable`;
  // in addition it feels ambiguous as to whether 'path pen' would be up or down

  def jump(x: Double, y: Double): Path = MoveRel(x, y) :: this

  def jump(pt: Point): Path = jump(pt.x, pt.y)

  def jump_@(x: Double, y: Double): Path = MoveAbs(x, y) :: this

  def jump_@(pt: Point): Path = jump_@(pt.x, pt.y)

  // NOTE: `jump_@` is cannonical spelling, yet `jumpAbs` provided for use from
  // languages where `@` is not identifier char; analogous correlates follow...

  def jumpAbs(x: Double, y: Double): Path = jump_@(x, y)

  def jumpAbs(pt: Point): Path = jump_@(pt.x, pt.y)


  def line(x: Double, y: Double): Path = LineRel(x, y) :: this

  def line(pt: Point): Path = line(pt.x, pt.y)

  def line_@(x: Double, y: Double): Path = LineAbs(x, y) :: this

  def line_@(pt: Point): Path = line_@(pt.x, pt.y)

  def lineAbs(x: Double, y: Double): Path = line_@(x, y)

  def lineAbs(pt: Point): Path = line_@(pt.x, pt.y)


  def horizontal(x: Double): Path = HorizontalRel(x) :: this

  def horiz(x: Double): Path = horizontal(x)

  def horizontal_@(x: Double): Path = HorizontalAbs(x) :: this

  def horizontalAbs(x: Double): Path = horizontal_@(x)

  def horiz_@(x: Double): Path = horizontal_@(x)

  def horizAbs(x: Double): Path = horizontal_@(x)


  def vertical(y: Double): Path = VerticalRel(y) :: this

  def vert(y: Double): Path = vertical(y)

  def vertical_@(y: Double): Path = VerticalAbs(y) :: this

  def verticalAbs(y: Double): Path = vertical_@(y)

  def vert_@(y: Double): Path = vertical_@(y)

  def vertAbs(y: Double): Path = vertical_@(y)


  def arc(
      radWidth: Double, radHeight: Double,
      xRotateDegrees: Double,
      kind: ArcChoice,
      x: Double, y: Double
    ): Path =
    EllipticalArcRel(radWidth, radHeight, xRotateDegrees, kind, x, y) :: this

  def arc(
      radWidth: Double, radHeight: Double,
      xRotateDegrees: Double,
      kind: ArcChoice,
      pt: Point
    ): Path =
    arc(radWidth, radHeight, xRotateDegrees, kind, pt.x, pt.y)

  def arc_@(
      radWidth: Double, radHeight: Double,
      xRotateDegrees: Double,
      kind: ArcChoice,
      x: Double, y: Double
    ): Path =
    EllipticalArcAbs(radWidth, radHeight, xRotateDegrees, kind, x, y) :: this

  def arc_@(
      radWidth: Double, radHeight: Double,
      xRotateDegrees: Double,
      kind: ArcChoice,
      pt: Point
    ): Path =
    arc_@(radWidth, radHeight, xRotateDegrees, kind, pt.x, pt.y)

  def arcAbs(
      radWidth: Double, radHeight: Double,
      xRotateDegrees: Double,
      kind: ArcChoice,
      x: Double, y: Double
    ): Path =
    arc_@(radWidth, radHeight, xRotateDegrees, kind, x, y)

  def arcAbs(
      radWidth: Double, radHeight: Double,
      xRotateDegrees: Double,
      kind: ArcChoice,
      pt: Point
    ): Path =
    arc_@(radWidth, radHeight, xRotateDegrees, kind, pt.x, pt.y)


  def arc(
      radWidth: Double, radHeight: Double,
      kind: ArcChoice,
      x: Double, y: Double
    ): Path =
    EllipticalArcRel(radWidth, radHeight, 0, kind, x, y) :: this

  def arc(radWidth: Double, radHeight: Double, kind: ArcChoice, pt: Point):
      Path =
    arc(radWidth, radHeight, kind, pt.x, pt.y)

  def arc_@(
      radWidth: Double, radHeight: Double,
      kind: ArcChoice,
      x: Double, y: Double
    ): Path =
    EllipticalArcAbs(radWidth, radHeight, 0, kind, x, y) :: this

  def arc_@(
      radWidth: Double, radHeight: Double,
      kind: ArcChoice,
      pt: Point
    ): Path =
    arc_@(radWidth, radHeight, kind, pt.x, pt.y)

  def arcAbs(
      radWidth: Double, radHeight: Double,
      kind: ArcChoice,
      x: Double, y: Double
    ): Path =
    arc_@(radWidth, radHeight, kind, x, y)

  def arcAbs(
      radWidth: Double, radHeight: Double,
      kind: ArcChoice,
      pt: Point
    ): Path =
    arc_@(radWidth, radHeight, kind, pt.x, pt.y)


  def arc(rad: Double, kind: ArcChoice, x: Double, y: Double): Path =
    EllipticalArcRel(rad, rad, 0, kind, x, y) :: this

  def arc(rad: Double, kind: ArcChoice, pt: Point): Path =
    arc(rad, kind, pt.x, pt.y)

  def arc_@(rad: Double, kind: ArcChoice, x: Double, y: Double): Path =
    EllipticalArcAbs(rad, rad, 0, kind, x, y) :: this

  def arc_@(rad: Double, kind: ArcChoice, pt: Point): Path =
    arc_@(rad, kind, pt.x, pt.y)

  def arcAbs(rad: Double, kind: ArcChoice, x: Double, y: Double): Path =
    arc_@(rad, kind, x, y)

  def arcAbs(rad: Double, kind: ArcChoice, pt: Point): Path =
    arc_@(rad, kind, pt.x, pt.y)


  def quadratic(xCtl1: Double, yCtl1: Double, x: Double, y: Double):
      QuadBezierPath =
    new QuadBezierPath(QuadBezierRel(xCtl1, yCtl1, x, y) :: this)

  def quadratic(ptCtl1: Point, pt: Point): QuadBezierPath =
    quadratic(ptCtl1.x, ptCtl1.y, pt.x, pt.y)

  def quad(xCtl1: Double, yCtl1: Double, x: Double, y: Double): QuadBezierPath =
    quadratic(xCtl1, yCtl1, x, y)

  def quad(ptCtl1: Point, pt: Point): QuadBezierPath =
    quadratic(ptCtl1, pt)


  def quadratic_@(xCtl1: Double, yCtl1: Double, x: Double, y: Double):
      QuadBezierPath =
    new QuadBezierPath(QuadBezierAbs(xCtl1, yCtl1, x, y) :: this)

  def quadratic_@(ptCtl1: Point, pt: Point): QuadBezierPath =
    quadratic_@(ptCtl1.x, ptCtl1.y, pt.x, pt.y)

  def quadraticAbs(xCtl1: Double, yCtl1: Double, x: Double, y: Double):
      QuadBezierPath =
    quadratic_@(xCtl1, yCtl1, x, y)

  def quadraticAbs(ptCtl1: Point, pt: Point): QuadBezierPath =
    quadratic_@(ptCtl1.x, ptCtl1.y, pt.x, pt.y)


  def quad_@(xCtl1: Double, yCtl1: Double, x: Double, y: Double):
      QuadBezierPath =
    quadratic_@(xCtl1, yCtl1, x, y)

  def quad_@(ptCtl1: Point, pt: Point): QuadBezierPath =
    quadratic_@(ptCtl1, pt)

  def quadAbs(xCtl1: Double, yCtl1: Double, x: Double, y: Double):
      QuadBezierPath =
    quadratic_@(xCtl1, yCtl1, x, y)

  def quadAbs(ptCtl1: Point, pt: Point): QuadBezierPath =
    quadratic_@(ptCtl1, pt)


  def cubic(
      xCtl1: Double, yCtl1: Double,
      xCtl2: Double, yCtl2: Double,
      x: Double, y: Double
    ): CubicBezierPath =
    new CubicBezierPath(
        CubicBezierRel(xCtl1, yCtl1, xCtl2, yCtl2, x, y) :: this
      )

  def cubic(ptCtl1: Point, ptCtl2: Point, pt: Point): CubicBezierPath =
    cubic(ptCtl1.x, ptCtl1.y, ptCtl2.x, ptCtl2.y, pt.x, pt.y)

  def cubic_@(
      xCtl1: Double, yCtl1: Double,
      xCtl2: Double, yCtl2: Double,
      x: Double, y: Double
    ): CubicBezierPath =
    new CubicBezierPath(CubicBezierAbs(xCtl1, yCtl1, xCtl2, yCtl2, x,y) :: this)

  def cubic_@(ptCtl1: Point, ptCtl2: Point, pt: Point): CubicBezierPath =
    cubic_@(ptCtl1.x, ptCtl1.y, ptCtl2.x, ptCtl2.y, pt.x, pt.y)

  def cubicAbs(
      xCtl1: Double, yCtl1: Double,
      xCtl2: Double,yCtl2: Double,
      x: Double, y: Double
    ): CubicBezierPath =
    cubic_@(xCtl1, yCtl1, xCtl2, yCtl2, x, y)

  def cubicAbs(ptCtl1: Point, ptCtl2: Point, pt: Point):CubicBezierPath =
    cubic_@(ptCtl1.x, ptCtl1.y, ptCtl2.x, ptCtl2.y, pt.x, pt.y)


  def close = Close :: this    
}


object Path extends PathCommandable {
  def from(x: Double, y: Double): Path = new Path(MoveRel(x, y))
  def from(pt: Point): Path = from(pt.x, pt.y)

  // NOTE: at Path start, when no prev. segment is present to provide the
  // current point, MoveRel and MoveAbs would seem equivalent--and are.  yet,
  // when Paths are combined, the final command of first path suddenly provides
  // a current Point to the second; in this case, a Path-initial MoveRel or
  // MoveAbs aquire distinct meaning.
  def from_@(x: Double, y: Double): Path = new Path(MoveAbs(x, y))
  def from_@(pt: Point): Path = from_@(pt.x, pt.y)

  // NOTE: `from_@` is cannonical spelling, yet `fromAbs` provided for use from
  // languages where `@` is not identifier char

  def fromAbs(x: Double, y: Double): Path = from_@(x, y)
  def fromAbs(pt: Point): Path = from_@(pt)


  implicit def toFreeForm(path: Path) = FreeForm(path)


  protected def ::(pathCmd: PathCmd): Path = pathCmd :: Path.from(0, 0)
  protected def :::(path: Path): Path = path ::: Path.from(0, 0)


  case class PosMemory(currPt: Point, subPathStartPt: Point) {
    import Point._

    def this(startPt: Point) = this(startPt, startPt)

    def follow(cmd: PathCmd): PosMemory = cmd match {
      case MoveAbs(x, y)                      => startSubPathAbs(x, y)
      case LineAbs(x, y)                      => replacePtAbs(x, y)
      case HorizontalAbs(x)                   => replacePtHorizAbs(x)
      case VerticalAbs(y)                     => replacePtVertAbs(y)
      case EllipticalArcAbs(_, _, _, _, x, y) => replacePtAbs(x, y)
      case QuadBezierAbs(_, _, x, y)          => replacePtAbs(x, y)
      case TangentQuadBezierAbs(x, y)         => replacePtAbs(x, y)
      case CubicBezierAbs(_, _, _, _, x, y)   => replacePtAbs(x, y)
      case TangentCubicBezierAbs(_, _, x, y)  => replacePtAbs(x, y)

      case MoveRel(x, y)                      => startSubPathRel(x, y)
      case LineRel(x, y)                      => replacePtRel(x, y)
      case HorizontalRel(x)                   => replacePtRel(x, 0)
      case VerticalRel(y)                     => replacePtRel(0, y)
      case EllipticalArcRel(_, _, _, _, x, y) => replacePtRel(x, y)
      case QuadBezierRel(_, _, x, y)          => replacePtRel(x, y)
      case TangentQuadBezierRel(x, y)         => replacePtRel(x, y)
      case CubicBezierRel(_, _, _, _, x, y)   => replacePtRel(x, y)
      case TangentCubicBezierRel(_, _, x, y)  => replacePtRel(x, y)

      case Close                              => closeSubPath
    }


    def startSubPathRel(xRel: Double, yRel: Double) =
      new PosMemory(calcPtRel(xRel, yRel))

    def startSubPathAbs(xAbs: Double, yAbs: Double) =
      new PosMemory((xAbs, yAbs))

    def closeSubPath = new PosMemory(subPathStartPt)


    def replacePtRel(xRel: Double, yRel: Double) =
      PosMemory(calcPtRel(xRel, yRel), subPathStartPt)

    def replacePtAbs(xAbs: Double, yAbs: Double) =
      PosMemory((xAbs, yAbs), subPathStartPt)

    def replacePtHorizAbs(xAbs: Double) =
      PosMemory((xAbs, currPt.y), subPathStartPt)

    def replacePtVertAbs(yAbs: Double) =
      PosMemory((currPt.x, yAbs), subPathStartPt)


    private def calcPtRel(xRel: Double, yRel: Double): Point =
      currPt move (xRel, yRel)
  }
}

// ctor called only by derived classes, and always w/ prev Path as `cmds` tail
sealed class Path protected (cmds: List[PathCmd])
    extends PathCommandable
       with Transformable[Path]
       with Placeable[Path] {

  // invariant: called only within Path object, and always with Move_{Abs,Rel}
  private def this(cmd: PathCmd) = this(List(cmd))


  def getCmds: List[PathCmd] = cmds.reverse

  // faster, but the List returned is in reverse 'turtle-graphics' order
  def getCmdsBackwards: List[PathCmd] = cmds

  // returns initialized PosMemory, and remaining getCmds.**tail**
  def initPosMemory: (Path.PosMemory, List[PathCmd]) = {
    val allCmds = getCmds
    val initialPosMemory = allCmds.head match {
      // since this is path-initial move, both rel and abs give same result
      case MoveRel(x, y) => new Path.PosMemory((x, y))
      case MoveAbs(x, y) => new Path.PosMemory((x, y))
      case _ =>
        throw new RuntimeException("Path starting w/ non-move should be " +
                                   "impossible!\n[Path Cmds]:\n" + allCmds)
    }
    (initialPosMemory, allCmds.tail)
  }


  // append reflection of existing path about prev. end point at angle `degrees`
  def reflection(degrees: Double): Path = {
    // NOTE: helpful that head (Move_{Abs,Rel}) dropped; reflect everything else
    val (initialPosMemory, subsequentCmds) = this.initPosMemory
    val currPos = (initialPosMemory /: subsequentCmds) { _.follow(_) }
    this & (subsequentCmds ::: Path.from(0, 0)).reflect(degrees,
                                                         currPos.currPt)
  }

  // WARNING: centerPt, used to implement move_@/moveAbs/-+@ may be EXPENSIVE!
  def centerPt: Point = FreeForm(this).centerPt    


  def move(xDist: Double, yDist: Double): Path = {
    // while, technically, a rel `PathCmd` would be unchanged under `move`,
    // the special case of a path-initial MoveRel does require movement
    def adjustTerminalMoveRelMap(list: List[PathCmd])(f: PathCmd => PathCmd):
        List[PathCmd] = {
      list match {
        case Nil =>
          Nil
        case MoveRel(x, y) :: Nil =>
          MoveRel.tupled(Point(x, y) move (xDist, yDist)) :: Nil
        case x :: xs =>
          f(x) :: adjustTerminalMoveRelMap(xs)(f)
      }
    }
    new Path(adjustTerminalMoveRelMap(cmds) { _.move(xDist, yDist) } )
  }

  def scale(xScaling: Double, yScaling: Double): Path =
    new Path(cmds map { _.scale(xScaling, yScaling) })

  def rotate(degrees: Double, aboutX: Double, aboutY: Double): Path =
    new Path(cmds map { _.rotate(degrees, aboutX, aboutY) })

  def reflect(degrees: Double, aboutX: Double, aboutY: Double): Path =
    new Path(cmds map { _.reflect(degrees, aboutX, aboutY) })

  def skewHoriz(degrees: Double): Path =
    new Path(cmds map { _.skewHoriz(degrees) })

  def skewVert(degrees: Double): Path =
    new Path(cmds map { _.skewVert(degrees) })


  protected def ::(pathCmd: PathCmd) = new Path(pathCmd :: cmds)

  protected def :::(path: Path) = new Path(path.getCmdsBackwards ::: cmds)


  private def :::(revCmds: List[PathCmd]) =
    (this /: revCmds) { _.::(_) } // foldl revs `cmds`; `.` so `::` left-assoc
}

// NOTE: SVG 1.1 spec appears to allow tangent/'smooth-curveto' to follow
// any previous path command: http://www.w3.org/TR/SVG/paths.html#PathDataBNF;
// tie does *NOT*: tangent may only follow prev bezier cmd (incl. tangent) of
// same degree (viz. quadratic v. cubic)

final class QuadBezierPath private (cmds: List[PathCmd])
    extends Path(cmds) {

  def this(p: Path) = this(p.getCmdsBackwards)

  def tangent(x: Double, y: Double): QuadBezierPath =
    new QuadBezierPath(TangentQuadBezierRel(x, y) :: cmds)

  def tangent(pt: Point): QuadBezierPath = tangent(pt.x, pt.y)

  def tangent_@(x: Double, y: Double): QuadBezierPath =
    new QuadBezierPath(TangentQuadBezierAbs(x, y) :: cmds)

  def tangent_@(pt: Point): QuadBezierPath = tangent_@(pt.x, pt.y)

  // NOTE: `tangent_@` is cannonical spelling, yet `tangentAbs` provided for
  // use from languages where `@` is not identifier char

  def tangentAbs(x: Double, y: Double): QuadBezierPath = tangent_@(x, y)

  def tangentAbs(pt: Point): QuadBezierPath = tangent_@(pt)
}

final class CubicBezierPath private (cmds: List[PathCmd])
    extends Path(cmds) {

  def this(p: Path) = this(p.getCmdsBackwards)

  def tangent(xCtl1: Double, yCtl1: Double, x: Double, y: Double):
      CubicBezierPath =
    new CubicBezierPath(TangentCubicBezierRel(xCtl1, yCtl1, x, y) :: cmds)

  def tangent(ptCtl1: Point, pt: Point): CubicBezierPath =
    tangent(ptCtl1.x, ptCtl1.y, pt.x, pt.y)

  def tangent_@(xCtl1: Double, yCtl1: Double, x: Double, y: Double):
      CubicBezierPath =
    new CubicBezierPath(TangentCubicBezierAbs(xCtl1, yCtl1, x, y) :: cmds)

  def tangent_@(ptCtl1: Point, pt: Point): CubicBezierPath =
    tangent_@(ptCtl1.x, ptCtl1.y, pt.x, pt.y)

  // NOTE: `tangent_@` is cannonical spelling, yet `tangentAbs` provided for
  // use from languages where `@` is not identifier char

  def tangentAbs(xCtl1: Double, yCtl1: Double, x: Double, y: Double):
      CubicBezierPath =
    tangent_@(xCtl1, yCtl1, x, y)

  def tangentAbs(ptCtl1: Point, pt: Point): CubicBezierPath =
    tangent_@(ptCtl1, pt)
}


sealed abstract class RelPathCmd extends PathCmd {
  override def move(xDist: Double, yDist: Double): PathCmd = this
}


case class MoveAbs(x: Double, y: Double)
    extends PathCmd {
  override protected val endPt: Point = (x, y)
  override protected val EndPtCtor = MoveAbs.tupled.compose(Point.toDoubles)
}

case class MoveRel(x: Double, y: Double)
    extends RelPathCmd {
  override protected val endPt: Point = (x, y)
  override protected val EndPtCtor = MoveRel.tupled.compose(Point.toDoubles)
}


case class LineAbs(x: Double, y: Double)
    extends PathCmd {
  override protected val endPt: Point = (x, y)
  override protected val EndPtCtor = LineAbs.tupled.compose(Point.toDoubles)
}

case class LineRel(x: Double, y: Double)
    extends RelPathCmd {
  override protected val endPt: Point = (x, y)
  override protected val EndPtCtor = LineRel.tupled.compose(Point.toDoubles)
}

case class HorizontalAbs(x: Double)
    extends PathCmd {
  override protected val endPt: Point = (x, 0.0)
  override protected val EndPtCtor = (pt: Point) => HorizontalAbs(pt.x)
}

case class HorizontalRel(x: Double)
    extends RelPathCmd {
  override protected val endPt: Point = (x, 0.0)
  override protected val EndPtCtor = (pt: Point) => HorizontalRel(pt.x)
}

case class VerticalAbs(y: Double)
    extends PathCmd {
  override protected val endPt: Point = (0.0, y)
  override protected val EndPtCtor = (pt: Point) => VerticalAbs(pt.y)
}

case class VerticalRel(y: Double)
    extends RelPathCmd {
  override protected val endPt: Point = (0.0, y)
  override protected val EndPtCtor = (pt: Point) => VerticalRel(pt.y)
}


case class EllipticalArcAbs(
    radWidth: Double, radHeight: Double,
    xRotateDegrees: Double,
    kind: ArcChoice,
    x: Double, y: Double
  ) extends PathCmd {
  override protected val endPt: Point = (x, y)
  override protected val EndPtCtor = (pt: Point) =>
    EllipticalArcAbs(radWidth, radHeight, xRotateDegrees, kind, pt.x, pt.y)

  override def scale(xScaling: Double, yScaling: Double) =
    EllipticalArcAbs(
        radWidth * xScaling,
        radHeight * yScaling,
        xRotateDegrees,
        kind,
        x * xScaling,
        y * yScaling
      )
}

case class EllipticalArcRel(
    radWidth: Double, radHeight: Double,
    xRotateDegrees: Double,
    kind: ArcChoice,
    x: Double, y: Double
  ) extends RelPathCmd {
  override protected val endPt: Point = (x, y)
  override protected val EndPtCtor = (pt: Point) =>
    EllipticalArcRel(radWidth, radHeight, xRotateDegrees, kind, pt.x, pt.y)

  override def scale(xScaling: Double, yScaling: Double) =
    EllipticalArcRel(
        radWidth * xScaling,
        radHeight * yScaling,
        xRotateDegrees,
        kind,
        x * xScaling,
        y * yScaling
      )

//????????do any of the others need to be overridden too?????
}

/*
case class CircularArcAbs(
    rad: Double,
    startAngleDegrees: Double,
    sweepAngleDegrees: Double,
    aboutX: Double,
    aboutY: Double
  )
*/


case class QuadBezierAbs(xCtl1: Double, yCtl1: Double, x: Double, y: Double)
    extends PathCmd {
  override protected val endPt: Point = (x, y)
  override protected val EndPtCtor = (pt: Point) =>
    QuadBezierAbs(xCtl1, yCtl1, pt.x, pt.y)

//!!!!!!!some of the other Transformable methods need to be overridden too!!!!!!
}

case class QuadBezierRel(xCtl1: Double, yCtl1: Double, x: Double, y: Double)
    extends RelPathCmd {
  override protected val endPt: Point = (x, y)
  override protected val EndPtCtor = (pt: Point) =>
    QuadBezierRel(xCtl1, yCtl1, pt.x, pt.y)

//!!!!!!!some of the other Transformable methods need to be overridden too!!!!!!
}

case class TangentQuadBezierAbs(x: Double, y: Double)
    extends PathCmd {
  override protected val endPt: Point = (x, y)
  override protected val EndPtCtor =
      TangentQuadBezierAbs.tupled.compose(Point.toDoubles)
}

case class TangentQuadBezierRel(x: Double, y: Double)
    extends RelPathCmd {
  override protected val endPt: Point = (x, y)
  override protected val EndPtCtor =
      TangentQuadBezierRel.tupled.compose(Point.toDoubles)
}


case class CubicBezierAbs(
    xCtl1: Double, yCtl1: Double,
    xCtl2: Double, yCtl2: Double,
    x: Double, y: Double
  ) extends PathCmd {
  override protected val endPt: Point = (x, y)
  override protected val EndPtCtor = (pt: Point) =>
    CubicBezierAbs(xCtl1, yCtl1, xCtl2, yCtl2, pt.x, pt.y)

//!!!!!!!some of the other Transformable methods need to be overridden too!!!!!!
}

case class CubicBezierRel(
    xCtl1: Double, yCtl1: Double,
    xCtl2: Double, yCtl2: Double,
    x: Double, y: Double
  ) extends RelPathCmd {
  override protected val endPt: Point = (x, y)
  override protected val EndPtCtor = (pt: Point) =>
    CubicBezierRel(xCtl1, yCtl1, xCtl2, yCtl2, pt.x, pt.y)

//!!!!!!!some of the other Transformable methods need to be overridden too!!!!!!
}

case class TangentCubicBezierAbs(
    xCtl1: Double, yCtl1: Double,
    x: Double, y: Double
  ) extends PathCmd {
  override protected val endPt: Point = (x, y)
  override protected val EndPtCtor = (pt: Point) =>
    TangentCubicBezierAbs(xCtl1, yCtl1, pt.x, pt.y)

//!!!!!!!some of the other Transformable methods need to be overridden too!!!!!!
}

case class TangentCubicBezierRel(
    xCtl1: Double, yCtl1: Double,
    x: Double, y: Double
  ) extends RelPathCmd {
  override protected val endPt: Point = (x, y)
  override protected val EndPtCtor = (pt: Point) =>
    TangentCubicBezierRel(xCtl1, yCtl1, pt.x, pt.y)

//!!!!!!!some of the other Transformable methods need to be overridden too!!!!!!
}


case object Close
    extends RelPathCmd {
  override protected val endPt: Point = (0.0, 0.0) // dummy val...
  override protected val EndPtCtor =
      (_: Point) => this // ...since every transform == this
}

}
