/*
   file: k_k_/graphics/tie/ink.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2013 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie

package ink {

import scala.collection.immutable.HashMap
import scala.collection.mutable

import k_k_.graphics.tie.effects.Effect.Opacity
import k_k_.graphics.tie.shapes.{Point, Shape}
import k_k_.graphics.tie.transform._


sealed abstract class Ink {

  def opacity: Double = 1.0

  def alpha(opacity: Opacity): Ink = createNonOpaqueInk(opacity)
  def alpha(opacity: Double): Ink = alpha(Opacity(opacity))
  def -#(opacity: Opacity): Ink = alpha(opacity)
  def -#(opacity: Double): Ink = alpha(opacity)


  protected def createNonOpaqueInk(opacity: Opacity): Ink =
    if (opacity.value == 1.0) this
    else NonOpaqueInk(this, opacity.value)
}


case object NullInk extends Ink {
  override protected def createNonOpaqueInk(opacity: Opacity): Ink =
    NullInk // opacity is meaningless for `NullInk`
}


object Color {
  def apply(red: Int, green: Int, blue: Int) = new Color(red, green, blue)

  def unapply(color: Color): Option[(Int, Int, Int)] =
    Some(color.red, color.green, color.blue)

  def named(name: String): Option[NamedColor] = NamedColor.named(name)

  def withHsl(hue: Double, saturation: Double, lightness: Double): Color =
    new HSLProjection(
        ((hue % 360) + 360) % 360,
        math.abs(saturation % 2),
        math.abs(lightness % 2)
      )


  private def lerp(x: Int, y: Int, proportion: Double): Int = {
    val pct = clampUnit(proportion)
    x + ((y - x) * pct).round.toInt
  }

  private def lerp(x: Double, y: Double, proportion: Double): Double = {
    val pct = clampUnit(proportion)
    x + ((y - x) * pct)
  }

  private def stepwiseTo1(step: Double): Seq[Double] =
    clampUnit(step) until 1.0 by step

  private def clampUnit(value: Double) =
    value max 0.0 min 1.0


  // 'Hue, Saturation, Lightness' cylindrical projection of RGB cube
  // adapted from: http://en.wikipedia.org/wiki/HSL_color_space
  private object HSLProjection {
    def apply(rgbColor: Color) = {
      val (r, g, b) =
          (rgbColor.red   / 255.0,
           rgbColor.green / 255.0,
           rgbColor.blue  / 255.0)
      val (min, max) = (r min g min b, r max g max b)
      val chroma = max - min        // diff of largest and smallest components
      val lightness = (min + max)/2 // avg. of largest and smallest components
      val saturation =
        if (chroma == 0.0) 0.0 else chroma / (1 - math.abs(2*lightness - 1))
      val hue = {
        val H =
          if (chroma == 0.0)   0.0
          else if (max == r)   (g - b)/chroma
          else if (max == g)   (b - r)/chroma + 2
          else  /*(max == b)*/ (r - g)/chroma + 4
        H * 60 // convert ordinal sextant of hexagon to degrees of (of circle)
      }
      new HSLProjection(
          if (hue < 0.0) hue + 360 else hue,
          saturation,
          lightness,
          chroma
        )
    }

    implicit def toColor(hsl: HSLProjection): Color = hsl.asRgb
  }

  // HSL cylindrical geometry uses (degrees) rotation about axis to specify hue:
  // primary: 0 == red        120 == green       240 == blue          360 == red
  // secondary:    [60 == yellow]      [180 == cyan]     [300 == magenta]
  private class HSLProjection private (
      val hue: Double,        // [ 0.0,360.0 )
      val saturation: Double, // [ 0.0,  1.0 ]
      val lightness: Double,  // [ 0.0,  1.0 ]
      chroma: Double
    ) {

    def this(hue: Double, saturation: Double, lightness: Double) =
      this(hue, saturation, lightness,
          saturation * (1 - math.abs(2*lightness - 1)))


    def rotate(degrees: Double) =
      new HSLProjection((hue + degrees) % 360, saturation, lightness, chroma)


    def saturate(amount: Double) =   // amount: [ 0.0, 1.0 ]
      new HSLProjection(hue, clampUnit(saturation + amount), lightness)

    def desaturate(amount: Double) = // amount: [ 0.0, 1.0 ]
      new HSLProjection(hue, clampUnit(saturation - amount), lightness)

    def isFullySaturated:   Boolean = saturation == 1.0
    def isFullyDesaturated: Boolean = saturation == 0.0


    def lighten(amount: Double) =    // amount: [ 0.0, 1.0 ]
      new HSLProjection(hue, saturation, clampUnit(lightness + amount))

    def darken(amount: Double) =     // amount: [ 0.0, 1.0 ]
      new HSLProjection(hue, saturation, clampUnit(lightness - amount))

    def isFullyLight: Boolean = lightness == 1.0
    def isFullyDark:  Boolean = lightness == 0.0


    def transition(other: HSLProjection, stop: Double): HSLProjection =
      new HSLProjection(
          lerp(hue,        other.hue,        stop),
          lerp(saturation, other.saturation, stop),
          lerp(lightness,  other.lightness,  stop)
        )

    def transitions(other: HSLProjection, stops: Seq[Double]):
        Seq[HSLProjection] =
      stops.view.map(transition(other, _))

    def transitionsBy(other: HSLProjection, step: Double):
        Seq[HSLProjection] =
      if (step <= 0.0) Seq.empty
      else transitions(other, stepwiseTo1(step))


    def transitionHue(other: HSLProjection, stop: Double): HSLProjection =
      new HSLProjection(
          lerp(hue, other.hue, stop),
          saturation,
          lightness,
          chroma
        )

    def transitionHues(other: HSLProjection, stops: Seq[Double]):
        Seq[HSLProjection] =
      stops.view.map(transitionHue(other, _))

    def transitionHuesBy(other: HSLProjection, step: Double):
        Seq[HSLProjection] =
      if (step <= 0.0) Seq.empty
      else transitionHues(other, stepwiseTo1(step))


    def transitionSaturation(other: HSLProjection, stop: Double):
        HSLProjection =
      new HSLProjection(hue,lerp(saturation, other.saturation, stop),lightness)

    def transitionSaturations(other: HSLProjection, stops: Seq[Double]):
        Seq[HSLProjection] =
      stops.view.map(transitionSaturation(other, _))

    def transitionSaturationsBy(other: HSLProjection, step: Double):
        Seq[HSLProjection] =
      if (step <= 0.0) Seq.empty
      else transitionSaturations(other, stepwiseTo1(step))


    def transitionLightness(other: HSLProjection, stop: Double):
        HSLProjection =
      new HSLProjection(hue,saturation, lerp(lightness, other.lightness, stop))

    def transitionLightnesses(other: HSLProjection, stops: Seq[Double]):
        Seq[HSLProjection] =
      stops.view.map(transitionLightness(other, _))

    def transitionLightnessesBy(other: HSLProjection, step: Double):
        Seq[HSLProjection] =
      if (step <= 0.0) Seq.empty
      else transitionLightnesses(other, stepwiseTo1(step))


    lazy val asRgb: Color =
      if (saturation == 0.0) {
        val component = (255 * lightness).round.toInt
        new Color(component, component, component, Some(this))
      } else {
        val H = hue / 60
        val X = chroma * (1 - math.abs((H % 2) - 1))
        val (r, g, b) =
            if      (H <  1.0)   (chroma,      X,    0.0)
            else if (H <  2.0)   (X,      chroma,    0.0)
            else if (H <  3.0)   (0.0,    chroma,      X)
            else if (H <  4.0)   (0.0,         X, chroma)
            else if (H <  5.0)   (X,         0.0, chroma)
            else  /*(H <  6.0)*/ (chroma,    0.0,      X)
        val lightnessAdjust = lightness - chroma/2
        val (red, green, blue) =
            (r + lightnessAdjust,
             g + lightnessAdjust,
             b + lightnessAdjust)
        new Color(
            (red   * 255).round.toInt,
            (green * 255).round.toInt,
            (blue  * 255).round.toInt,
            Some(this)
          )
      }
  }
}

sealed class Color private (
  val red:   Int,
  val green: Int,
  val blue:  Int,
  hslProjection: Option[AnyRef]
  ) extends Ink {

//                          hslProjection: Option[Color.HSLProjection])
//????not sure why it's not possible to access the type in the constructor, yet
// it is perfectly acceptible below in the def. of hslEquiv????
//[ERROR] .../tie/tie/src/main/scala/k_k_/graphics/tie/ink.scala:251: error: class HSLProjection cannot be accessed in object k_k_.graphics.tie.ink.Color
//[INFO]                             hslProjection: Option[Color.HSLProjection])
//[INFO]                                                         ^

  def this(red: Int, green: Int, blue: Int) =
    this(math.abs(red % 256), math.abs(green % 256), math.abs(blue % 256), None)


  final def asColorOnly: Color = new Color(red, green, blue, hslProjection)


  // NOTE: override with covariant return type
  override def alpha(opacity:Opacity): Color = createNonOpaqueInk(opacity)
  override def alpha(opacity: Double): Color = alpha(Opacity(opacity))
  override def -#(opacity: Opacity): Color = alpha(opacity)
  override def -#(opacity: Double): Color = alpha(opacity)


  // rotate hue, preserving saturation and lightness:

  def rotate(degrees: Double): Color = hslEquiv.rotate(degrees)
  def -%(degrees: Double): Color = rotate(degrees)

  final def complement: Color = rotate(180)


  def saturate(amount: Double): Color =   // amount: [ 0.0, 1.0 ]
    hslEquiv.saturate(amount)

  def desaturate(amount: Double): Color = // amount: [ 0.0, 1.0 ]
    saturate(-amount)

  def isFullySaturated:   Boolean = hslEquiv.isFullySaturated
  def isFullyDesaturated: Boolean = hslEquiv.isFullyDesaturated


  def lighten(amount: Double): Color =    // amount: [ 0.0, 1.0 ]
    hslEquiv.lighten(amount)

  def darken(amount: Double): Color =     // amount: [ 0.0, 1.0 ]
    lighten(-amount)

  def isFullyLight: Boolean = hslEquiv.isFullyLight
  def isFullyDark:  Boolean = hslEquiv.isFullyDark


  def transition(other: Color, stop: Double = 0.5): Color =
    hslEquiv.transition(other.hslEquiv, stop)

  def transitions(other: Color, stops: Seq[Double]): Seq[Color] =
    hslEquiv.transitions(other.hslEquiv, stops).map( _.asRgb )

  def transitionsBy(other: Color, step: Double = 0.1): Seq[Color] =
    hslEquiv.transitionsBy(other.hslEquiv, step).map( _.asRgb )


  def transitionHue(other: Color, stop: Double = 0.5): Color =
    hslEquiv.transitionHue(other.hslEquiv, stop)

  def transitionHues(other: Color, stops: Seq[Double]): Seq[Color] =
    hslEquiv.transitionHues(other.hslEquiv, stops).map( _.asRgb )

  def transitionHuesBy(other: Color, step: Double = 0.1): Seq[Color] =
    hslEquiv.transitionHuesBy(other.hslEquiv, step).map( _.asRgb )


  def transitionSaturation(other: Color, stop: Double = 0.5): Color =
    hslEquiv.transitionSaturation(other.hslEquiv, stop)

  def transitionSaturations(other: Color, stops: Seq[Double]): Seq[Color] =
    hslEquiv.transitionSaturations(other.hslEquiv, stops).map( _.asRgb )

  def transitionSaturationsBy(other: Color, step: Double = 0.1): Seq[Color] =
    hslEquiv.transitionSaturationsBy(other.hslEquiv, step).map( _.asRgb )


  def transitionLightness(other: Color, stop: Double = 0.5): Color =
    hslEquiv.transitionLightness(other.hslEquiv, stop)

  def transitionLightnesses(other: Color, stops: Seq[Double]): Seq[Color] =
    hslEquiv.transitionLightnesses(other.hslEquiv, stops).map( _.asRgb )

  def transitionLightnessesBy(other: Color, step: Double = 0.1): Seq[Color] =
    hslEquiv.transitionLightnessesBy(other.hslEquiv, step).map( _.asRgb )


  // 'linear interpolation' in RGB color space

  def lerp(other: Color, stop: Double = 0.5): Color =
    Color(Color.lerp(red,   other.red,   stop),
          Color.lerp(green, other.green, stop),
          Color.lerp(blue,  other.blue,  stop))

  def lerps(other: Color, stops: Seq[Double]): Seq[Color] =
    stops.view.map(lerp(other, _))

  def lerpsBy(other: Color, step: Double = 0.1): Seq[Color] =
    if (step <= 0.0) Seq.empty
    else lerps(other, Color.stepwiseTo1(step))


  def asRgbString = "rgb(%d, %d, %d)".format(red, green, blue)

  def asHexString = "#%02x%02x%02x".format(red, green, blue)

  def asHslString = "hsl(%f, %f, %f)".format(
      hslEquiv.hue,
      hslEquiv.saturation,
      hslEquiv.lightness
    )

  override def toString: String = asRgbString


  override def equals(other: Any): Boolean = other match {
    case that: Color =>
      that.canEqual(this) && this.red   == that.red   &&
                             this.green == that.green &&
                             this.blue == that.blue
    case _ => false
  }

  override def hashCode: Int =
    41 * (
      41 * (
        41 + red
      ) + green
    ) + blue

  def canEqual(other: Any) = other.isInstanceOf[Color]


  override protected def createNonOpaqueInk(opacity: Opacity): Color =
    if (opacity.value == 1.0) this
    else NonOpaqueColor(this, opacity.value)


  private lazy val hslEquiv = hslProjection match {
    case Some(hslProj: Color.HSLProjection) => hslProj
    case None | _                           => Color.HSLProjection(this)
  }
}


object NamedColor {
  def apply(name: String): Option[NamedColor] = named(name)

  def unapply(namedClr: NamedColor): Option[(String, Int, Int, Int)] =
    Some(namedClr.name, namedClr.red, namedClr.green, namedClr.blue)

  def named(name: String): Option[NamedColor] =
    allNamedColors.get(normalize(name))  

  // Notes on the invariants for containment of evil, mutable state:
  // the NamedColor class is sealed, and only extended within the NamedColors
  // object, once for every CSS2/X11 color name.  During construction, each
  // instance triggers invocation of the private registerColor() method on this
  // companion object (the only method to mutate the `registeredColors` Map).
  // since the method is private, and its companion class is impossible to
  // construct elsewhere, it is called only during initialization of the
  // NamedColors object.  all external use of named-based lookup goes through
  // the lazily evaluated immutable Map `allNamedColors`.  since its
  // lazily-computed val references a member of NamedColors, the initializer
  // of the latter must have run prior to the one and only read from the
  // mutable 'registeredColors` Map.  Thus, laziness leads to safer code.

  lazy val allNamedColors = {
    // NOTE: use lowercase `val`, not `object`, which won't load enclosing class
    val _ = NamedColors.black // ...like the scourge of mutability ;)
    HashMap(registeredColors.toSeq : _*)
  }

  private def registerColor(named: NamedColor) {
    registeredColors += normalize(named.name) -> named
  }

  private def normalize(name: String) =
    name.toLowerCase

  val registeredColors = new mutable.HashMap[String, NamedColor]
}

sealed abstract class NamedColor(val name: String, r: Int, g: Int, b: Int)
    extends Color(r, g, b) {

  NamedColor.registerColor(this)

  override def toString: String = super.toString + "[" + name + "]"

  // NOTE: no override of `equals`, since `name` considered merely meta-data
}

// last-resort fall-back class for Renderer-specific color string.
// caveat user: may lead to fmt-specific code--use sparingly, if ever!
case class DescribedColor(desc: String) extends Color(0, 0, 0) {//(Black)
  override def canEqual(other: Any) = other.isInstanceOf[DescribedColor]
}

// alt. to NonOpaqueInk, which preserves typing within Color sub-hierarchy
case class NonOpaqueColor(color: Color, override val opacity: Double)
    extends Color(color.red, color.green, color.blue) {

  override def rotate(degrees: Double): Color =
    copy(color = super.rotate(degrees))

  override def saturate(amount: Double): Color =   // amount: [ 0.0, 1.0 ]
    copy(color = super.saturate(amount))

  override def lighten(amount: Double): Color =    // amount: [ 0.0, 1.0 ]
    copy(color = super.lighten(amount))


  override def transition(other: Color, stop: Double = 0.5): Color =
    copy(color = super.transition(other, stop))

  override def transitions(other: Color, stops: Seq[Double]): Seq[Color] =
    super.transitions(other, stops).map( _ alpha opacity )

  override def transitionsBy(other: Color, step: Double = 0.1): Seq[Color] =
    super.transitionsBy(other, step).map( _ alpha opacity )


  override def transitionHue(other: Color, stop: Double = 0.5): Color =
    copy(color = super.transitionHue(other, stop))

  override def transitionHues(other: Color, stops: Seq[Double]): Seq[Color] =
    super.transitionHues(other, stops).map( _ alpha opacity )

  override def transitionHuesBy(other: Color, step: Double = 0.1): Seq[Color] =
    super.transitionHuesBy(other, step).map( _ alpha opacity )


  override def transitionSaturation(other: Color, stop: Double = 0.5): Color =
    copy(color = super.transitionSaturation(other, stop))

  override def transitionSaturations(other: Color, stops: Seq[Double]):
      Seq[Color] =
    super.transitionSaturations(other, stops).map( _ alpha opacity )

  override def transitionSaturationsBy(other: Color, step: Double = 0.1):
      Seq[Color] =
    super.transitionSaturationsBy(other, step).map( _ alpha opacity )


  override def transitionLightness(other: Color, stop: Double = 0.5): Color =
    copy(color = super.transitionLightness(other, stop))

  override def transitionLightnesses(other: Color, stops: Seq[Double]):
      Seq[Color] =
    super.transitionLightnesses(other, stops).map( _ alpha opacity )

  override def transitionLightnessesBy(other: Color, step: Double = 0.1):
      Seq[Color] =
    super.transitionLightnessesBy(other, step).map( _ alpha opacity )


  override def toString: String =
     super.toString + "(alpha = " + opacity + ")"

  override def canEqual(other: Any) = other.isInstanceOf[NonOpaqueColor]


  override protected def createNonOpaqueInk(moreOpacity: Opacity): Color = {
    val combo = opacity * moreOpacity.value
    if (combo == 1.0) color
    else              copy(opacity = combo)
  }
}


object NonOpaqueInk {

  //????what changed???? scala 2.9.0--but not 2.8.1!--gives error:
  // [error] .../tie/src/main/scala/k_k_/graphics/tie/ink.scala:576: cannot
  //         resolve overloaded unapply
  // case class NonOpaqueInk(ink: Ink, override val opacity: Double)
  // 
  // def unapply(x: AnyRef): Option[(Ink, Double)] =
  //
  // ...thus, make NonOpaqueInk non-case-class, but fill in (needed) methods
  // of companion here

  def apply(ink: Ink, opacity: Double) = new NonOpaqueInk(ink, opacity)

  def unapply(x: AnyRef): Option[(Ink, Double)] =
    x match {
      case nonOpaque: NonOpaqueInk =>
        Some(nonOpaque.ink, nonOpaque.opacity)
      case nonOpaque: NonOpaqueColor =>
        Some(nonOpaque.color, nonOpaque.opacity)
      case _ =>
        None
    }
}

final class NonOpaqueInk(val ink: Ink, override val opacity: Double)
    extends Ink {
  def copy(ink: Ink = this.ink, opacity: Double = this.opacity) =
    new NonOpaqueInk(ink, opacity)

  override protected def createNonOpaqueInk(moreOpacity: Opacity):
      Ink = {
    val combo = opacity * moreOpacity.value
    if (combo == 1.0) ink
    else              copy(opacity = combo)
  }
}


object Pattern {
  def apply(shape: Shape, width: Double, height: Double): Pattern =
    new ShapePattern(shape, width, height)

  def apply(shape: Shape): Pattern =
    new ShapePattern(shape)
}

sealed abstract class Pattern
    extends Ink
       with Transforming[Pattern]
       with Placeable[Pattern] {

  type TranslatedT          = TranslatedPattern
  protected val Translated  = TranslatedPattern

  type ScaledT              = ScaledPattern
  protected val Scaled      = ScaledPattern

  type RotatedT             = RotatedPattern
  protected val Rotated     = RotatedPattern

  type ReflectedT           = ReflectedPattern
  protected val Reflected   = ReflectedPattern

  type SkewedHorizT         = SkewedHorizPattern
  protected val SkewedHoriz = SkewedHorizPattern

  type SkewedVertT          = SkewedVertPattern
  protected val SkewedVert  = SkewedVertPattern
}

object ShapePattern {
  def apply(shape: Shape): Pattern = new ShapePattern(shape)
}

case class ShapePattern(shape: Shape, width: Double, height: Double)
    extends Pattern {

  def this(shape: Shape) =
    this(shape, shape.boundingBox.width, shape.boundingBox.height)

  def centerPt: Point = shape.centerPt
}


sealed abstract class TransformedPattern extends Pattern {
  val pattern: Pattern
  def centerPt: Point = pattern.centerPt
}

object TranslatedPattern extends TranslatedTransformable[Pattern] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[TranslatedPattern]
}

case class TranslatedPattern(pattern: Pattern, xMove: Double, yMove: Double)
    extends TransformedPattern


object ScaledPattern extends ScaledTransformable[Pattern] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[ScaledPattern]
}

case class ScaledPattern(pattern: Pattern, xScaling: Double, yScaling: Double)
    extends TransformedPattern


object RotatedPattern extends RotatedTransformable[Pattern] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[RotatedPattern]
}

case class RotatedPattern(
    pattern: Pattern,
    degrees: Double,
    aboutX: Double,
    aboutY: Double
  ) extends TransformedPattern


object ReflectedPattern extends ReflectedTransformable[Pattern] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[ReflectedPattern]
}

case class ReflectedPattern(
    pattern: Pattern,
    degrees: Double,
    aboutX: Double,
    aboutY: Double
  ) extends TransformedPattern


object SkewedHorizPattern extends SkewedHorizTransformable[Pattern] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[SkewedHorizPattern]
}

case class SkewedHorizPattern(pattern: Pattern, degrees: Double)
    extends TransformedPattern


object SkewedVertPattern extends SkewedVertTransformable[Pattern] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[SkewedVertPattern]
}

case class SkewedVertPattern(pattern: Pattern, degrees: Double)
    extends TransformedPattern



sealed abstract class ColorSpread
object ColorSpread {
  case object Pad     extends ColorSpread
  case object Reflect extends ColorSpread
  case object Repeat  extends ColorSpread
}


sealed abstract class ColorInterp
object ColorInterp {
  case object sRGB      extends ColorInterp
  case object LinearRGB extends ColorInterp
}


// `stopOffsetPct`: [ 0.0, 100.0 ]
case class ColorStop(color: Color, stopOffsetPct: Double) {
  def this(color: Color, stopOffsetPct: Double, opacity: Double) =
    this(color alpha opacity, stopOffsetPct)

  def this(color: Color, stopOffsetPct: Double, opacity: Opacity) =
    this(color alpha opacity, stopOffsetPct)


  val opacity = color.opacity

  def alpha(opacity: Opacity): ColorStop =
    copy(color = color alpha opacity)
  def alpha(opacity: Double): ColorStop = alpha(Opacity(opacity))
  def -#(opacity: Opacity): ColorStop = alpha(opacity)
  def -#(opacity: Double): ColorStop = alpha(opacity)
}


sealed abstract class Gradient extends Ink with Transforming[Gradient] {

  type TranslatedT          = TranslatedGradient
  protected val Translated  = TranslatedGradient

  type ScaledT              = ScaledGradient
  protected val Scaled      = ScaledGradient

  type RotatedT             = RotatedGradient
  protected val Rotated     = RotatedGradient

  type ReflectedT           = ReflectedGradient
  protected val Reflected   = ReflectedGradient

  type SkewedHorizT         = SkewedHorizGradient
  protected val SkewedHoriz = SkewedHorizGradient

  type SkewedVertT          = SkewedVertGradient
  protected val SkewedVert  = SkewedVertGradient


  def restyle: Gradient // converts between LinearGradient <-> RadialGradient
}


trait GradientConstruction[T <: Gradient] {

  def apply(colors: Seq[ColorStop], opacity: Opacity): T =
    apply(colors map ( _ alpha opacity ))

  def apply(
      colors: Seq[ColorStop],
      opacity: Opacity,
      spread: ColorSpread
    ): T =
    apply(colors map ( _ alpha opacity ), spread)

  def apply(
      colors: Seq[ColorStop],
      opacity: Opacity,
      spread: ColorSpread,
      interp: ColorInterp
    ): T =
    apply(colors map ( _ alpha opacity ), spread, interp)


  def apply(colors: Seq[ColorStop], opacity: Double): T =
    apply(colors, Opacity(opacity))

  def apply(colors: Seq[ColorStop], opacity: Double, spread: ColorSpread): T =
    apply(colors, Opacity(opacity), spread)

  def apply(
      colors: Seq[ColorStop],
      opacity: Double,
      spread: ColorSpread,
      interp: ColorInterp
    ): T =
    apply(colors, Opacity(opacity), spread, interp)


  def uniform(
      colors: Seq[Color],
      opacity: Opacity = Opacity(1.0),
      spread: ColorSpread = ColorSpread.Pad,
      interp: ColorInterp = ColorInterp.sRGB
    ): T = {
    val eachOffsetPct = 100.0 / (colors.length - 1)
    apply(colors.zipWithIndex.map(p =>ColorStop(p._1, p._2 * eachOffsetPct)),
          opacity, spread, interp)
  }


  def uniform(colors: Seq[ColorStop], spread: ColorSpread): T =
    uniform(colors, spread = spread)

  def uniform(colors: Seq[ColorStop], spread: ColorSpread, interp: ColorInterp):
      T =
    uniform(colors, spread = spread, interp = interp)


  def uniform(colors: Seq[Color], opacity: Double): T =
    uniform(colors, Opacity(opacity))

  def uniform(colors: Seq[Color], opacity: Double, spread: ColorSpread): T =
    uniform(colors, Opacity(opacity), spread)

  def uniform(
      colors: Seq[Color],
      opacity: Double,
      spread: ColorSpread,
      interp: ColorInterp
    ): T =
    uniform(colors, Opacity(opacity), spread, interp)


 def apply(
     colorStops: Seq[ColorStop],
     spread: ColorSpread = ColorSpread.Pad,
     interp: ColorInterp = ColorInterp.sRGB
   ): T
}


object LinearGradient extends GradientConstruction[LinearGradient]

case class LinearGradient(
    colorStops: Seq[ColorStop],
    spread: ColorSpread = ColorSpread.Pad,
    interp: ColorInterp = ColorInterp.sRGB
  ) extends Gradient {
  def restyle: Gradient = asRadial
  def asRadial: RadialGradient = RadialGradient(colorStops, spread, interp)
}


object RadialGradient extends GradientConstruction[RadialGradient]

case class RadialGradient(
    colorStops: Seq[ColorStop],
    spread: ColorSpread = ColorSpread.Pad,
    interp: ColorInterp = ColorInterp.sRGB
  ) extends Gradient {
  def restyle: Gradient = asLinear
  def asLinear: LinearGradient = LinearGradient(colorStops, spread, interp)
}


sealed abstract class TransformedGradient extends Gradient {
  def restyle: Gradient = copyGrad(gradient.restyle)

  def gradient: Gradient


  // musing: it would be really slick if scala would require, in the case of
  // named argument method application in an abstract class, that deriving
  // classes need only implement a method by that name, taking a parameter with
  // the name(s) mentioned, and not actually that all deriving class methods
  // have the identical signature.  in a way, this would act analogously to a
  // structure type. for example, one might declare something like the following
  // that would be automatically provided by each synthesized case class `copy`
  // method below:
  //     def copy({gradient: Gradient}): Gradient
  // or: def copy(..., gradient: Gradient, ...): Gradient
  protected def copyGrad(g: Gradient): Gradient
}


object TranslatedGradient extends TranslatedTransformable[Gradient] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[TranslatedGradient]
}

case class TranslatedGradient(gradient: Gradient, xMove: Double, yMove: Double)
    extends TransformedGradient {
  protected def copyGrad(g: Gradient): Gradient = copy(gradient = g)
}


object ScaledGradient extends ScaledTransformable[Gradient] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[ScaledGradient]
}

case class ScaledGradient(
    gradient: Gradient,
    xScaling: Double,
    yScaling: Double
  ) extends TransformedGradient {
  protected def copyGrad(g: Gradient): Gradient = copy(gradient = g)
}


object RotatedGradient extends RotatedTransformable[Gradient] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[RotatedGradient]
}

case class RotatedGradient(
    gradient: Gradient,
    degrees: Double,
    aboutX: Double,
    aboutY: Double
  ) extends TransformedGradient {
  protected def copyGrad(g: Gradient): Gradient = copy(gradient = g)
}


object ReflectedGradient extends ReflectedTransformable[Gradient] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[ReflectedGradient]
}

case class ReflectedGradient(
    gradient: Gradient,
    degrees: Double,
    aboutX: Double,
    aboutY: Double
  ) extends TransformedGradient {
  protected def copyGrad(g: Gradient): Gradient = copy(gradient = g)
}


object SkewedHorizGradient extends SkewedHorizTransformable[Gradient] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[SkewedHorizGradient]
}

case class SkewedHorizGradient(
    gradient: Gradient,
    degrees: Double
  ) extends TransformedGradient {
  protected def copyGrad(g: Gradient): Gradient = copy(gradient = g)
}


object SkewedVertGradient extends SkewedVertTransformable[Gradient] {
  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[SkewedVertGradient]
}

case class SkewedVertGradient(
    gradient: Gradient,
    degrees: Double
  ) extends TransformedGradient {
  protected def copyGrad(g: Gradient): Gradient = copy(gradient = g)
}



/** W3C CSS2 named colors, the so-called 'X11 Colors'; compiled from:
 *  "Learn SVG: The Web Graphics Standard"; "Appendix A: sRGB Colors";
 *     by Jon Frost, Stefan Goessner and Michel Hirtzler,
 *     revisited by Robert DiBlasi and Tobias Reif
 *   http://www.learnsvg.com/
 *   english translation: http://www.learnsvg.com/dnld/eBookFullAll.zip
 *
 *   Plug: This book fuckin' rocks!  Thanks, guys, for writing it!
 */
object NamedColors {

  object AliceBlue extends NamedColor("AliceBlue", 240, 248, 255)
  val aliceBlue: NamedColor = AliceBlue

  object AntiqueWhite extends NamedColor("AntiqueWhite", 250, 235, 215)
  val antiqueWhite: NamedColor = AntiqueWhite

  object Aqua extends NamedColor("Aqua", 0, 255, 255)
  val aqua: NamedColor = Aqua

  object Aquamarine extends NamedColor("Aquamarine", 127, 255, 212)
  val aquamarine: NamedColor = Aquamarine

  object Azure extends NamedColor("Azure", 240, 255, 255)
  val azure: NamedColor = Azure

  object Beige extends NamedColor("Beige", 245, 245, 220)
  val beige: NamedColor = Beige

  object Bisque extends NamedColor("Bisque", 255, 228, 196)
  val bisque: NamedColor = Bisque

  object Black extends NamedColor("Black", 0, 0, 0)
  val black: NamedColor = Black

  object BlanchedAlmond extends NamedColor("BlanchedAlmond", 255, 235, 205)
  val blanchedAlmond: NamedColor = BlanchedAlmond

  object Blue extends NamedColor("Blue", 0, 0, 255)
  val blue: NamedColor = Blue

  object BlueViolet extends NamedColor("BlueViolet", 138, 43, 226)
  val blueViolet: NamedColor = BlueViolet

  object Brown extends NamedColor("Brown", 165, 42, 42)
  val brown: NamedColor = Brown

  object BurlyWood extends NamedColor("BurlyWood", 222, 184, 135)
  val burlyWood: NamedColor = BurlyWood

  object CadetBlue extends NamedColor("CadetBlue", 95, 158, 160)
  val cadetBlue: NamedColor = CadetBlue

  object Chartreuse extends NamedColor("Chartreuse", 127, 255, 0)
  val chartreuse: NamedColor = Chartreuse

  object Chocolate extends NamedColor("Chocolate", 210, 105, 30)
  val chocolate: NamedColor = Chocolate

  object Coral extends NamedColor("Coral", 255, 127, 80)
  val coral: NamedColor = Coral

  object CornflowerBlue extends NamedColor("CornflowerBlue", 100, 149, 237)
  val cornflowerBlue: NamedColor = CornflowerBlue

  object Cornsilk extends NamedColor("Cornsilk", 255, 248, 220)
  val cornsilk: NamedColor = Cornsilk

  object Crimson extends NamedColor("Crimson", 220, 20, 60)
  val crimson: NamedColor = Crimson

  object Cyan extends NamedColor("Cyan", 0, 255, 255)
  val cyan: NamedColor = Cyan

  object DarkBlue extends NamedColor("DarkBlue", 0, 0, 139)
  val darkBlue: NamedColor = DarkBlue

  object DarkCyan extends NamedColor("DarkCyan", 0, 139, 139)
  val darkCyan: NamedColor = DarkCyan

  object DarkGoldenrod extends NamedColor("DarkGoldenrod", 184, 134, 11)
  val darkGoldenrod: NamedColor = DarkGoldenrod

  object DarkGray extends NamedColor("DarkGray", 169, 169, 169)
  val darkGray: NamedColor = DarkGray

  object DarkGreen extends NamedColor("DarkGreen", 0, 100, 0)
  val darkGreen: NamedColor = DarkGreen

  object DarkKhaki extends NamedColor("DarkKhaki", 189, 183, 107)
  val darkKhaki: NamedColor = DarkKhaki

  object DarkMagenta extends NamedColor("DarkMagenta", 139, 0, 139)
  val darkMagenta: NamedColor = DarkMagenta

  object DarkOliveGreen extends NamedColor("DarkOliveGreen", 85, 107, 47)
  val darkOliveGreen: NamedColor = DarkOliveGreen

  object DarkOrange extends NamedColor("DarkOrange", 255, 140, 0)
  val darkOrange: NamedColor = DarkOrange

  object DarkOrchid extends NamedColor("DarkOrchid", 153, 50, 204)
  val darkOrchid: NamedColor = DarkOrchid

  object DarkRed extends NamedColor("DarkRed", 139, 0, 0)
  val darkRed: NamedColor = DarkRed

  object DarkSalmon extends NamedColor("DarkSalmon", 233, 150, 122)
  val darkSalmon: NamedColor = DarkSalmon

  object DarkSeaGreen extends NamedColor("DarkSeaGreen", 143, 188, 139)
  val darkSeaGreen: NamedColor = DarkSeaGreen

  object DarkSlateBlue extends NamedColor("DarkSlateBlue", 72, 61, 139)
  val darkSlateBlue: NamedColor = DarkSlateBlue

  object DarkSlateGray extends NamedColor("DarkSlateGray", 47, 79, 79)
  val darkSlateGray: NamedColor = DarkSlateGray

  object DarkTurquoise extends NamedColor("DarkTurquoise", 0, 206, 209)
  val darkTurquoise: NamedColor = DarkTurquoise

  object DarkViolet extends NamedColor("DarkViolet", 148, 0, 211)
  val darkViolet: NamedColor = DarkViolet

  object DeepPink extends NamedColor("DeepPink", 255, 20, 147)
  val deepPink: NamedColor = DeepPink

  object DeepSkyBlue extends NamedColor("DeepSkyBlue", 0, 191, 255)
  val deepSkyBlue: NamedColor = DeepSkyBlue

  object DimGray extends NamedColor("DimGray", 105, 105, 105)
  val dimGray: NamedColor = DimGray

  object DodgerBlue extends NamedColor("DodgerBlue", 30, 144, 255)
  val dodgerBlue: NamedColor = DodgerBlue

  object Firebrick extends NamedColor("Firebrick", 178, 34, 34)
  val firebrick: NamedColor = Firebrick

  object FloralWhite extends NamedColor("FloralWhite", 255, 250, 240)
  val floralWhite: NamedColor = FloralWhite

  object ForestGreen extends NamedColor("ForestGreen", 34, 139, 34)
  val forestGreen: NamedColor = ForestGreen

  object Fuchsia extends NamedColor("Fuchsia", 255, 0, 255)
  val fuchsia: NamedColor = Fuchsia

  object Gainsboro extends NamedColor("Gainsboro", 220, 220, 220)
  val gainsboro: NamedColor = Gainsboro

  object GhostWhite extends NamedColor("GhostWhite", 248, 248, 255)
  val ghostWhite: NamedColor = GhostWhite

  object Gold extends NamedColor("Gold", 255, 215, 0)
  val gold: NamedColor = Gold

  object Goldenrod extends NamedColor("Goldenrod", 218, 165, 32)
  val goldenrod: NamedColor = Goldenrod

  object Gray extends NamedColor("Gray", 128, 128, 128)
  val gray: NamedColor = Gray

  object Green extends NamedColor("Green", 0, 128, 0)
  val green: NamedColor = Green

  object GreenYellow extends NamedColor("GreenYellow", 173, 255, 47)
  val greenYellow: NamedColor = GreenYellow

  object Honeydew extends NamedColor("Honeydew", 240, 255, 240)
  val honeydew: NamedColor = Honeydew

  object HotPink extends NamedColor("HotPink", 255, 105, 180)
  val hotPink: NamedColor = HotPink

  object IndianRed extends NamedColor("IndianRed", 205, 92, 92)
  val indianRed: NamedColor = IndianRed

  object Indigo extends NamedColor("Indigo", 75, 0, 130)
  val indigo: NamedColor = Indigo

  object Ivory extends NamedColor("Ivory", 255, 255, 240)
  val ivory: NamedColor = Ivory

  object Khaki extends NamedColor("Khaki", 240, 230, 140)
  val khaki: NamedColor = Khaki

  object Lavender extends NamedColor("Lavender", 230, 230, 250)
  val lavender: NamedColor = Lavender

  object LavenderBlush extends NamedColor("LavenderBlush", 255, 240, 245)
  val lavenderBlush: NamedColor = LavenderBlush

  object LawnGreen extends NamedColor("LawnGreen", 124, 252, 0)
  val lawnGreen: NamedColor = LawnGreen

  object LemonChiffon extends NamedColor("LemonChiffon", 255, 250, 205)
  val lemonChiffon: NamedColor = LemonChiffon

  object LightBlue extends NamedColor("LightBlue", 173, 216, 230)
  val lightBlue: NamedColor = LightBlue

  object LightCoral extends NamedColor("LightCoral", 240, 128, 128)
  val lightCoral: NamedColor = LightCoral

  object LightCyan extends NamedColor("LightCyan", 224, 255, 255)
  val lightCyan: NamedColor = LightCyan

  object LightGoldenrodYellow extends NamedColor("LightGoldenrodYellow", 250, 250, 210)
  val lightGoldenrodYellow: NamedColor = LightGoldenrodYellow

  object LightGray extends NamedColor("LightGray", 211, 211, 211)
  val lightGray: NamedColor = LightGray

  object LightGreen extends NamedColor("LightGreen", 144, 238, 144)
  val lightGreen: NamedColor = LightGreen

  object LightPink extends NamedColor("LightPink", 255, 182, 193)
  val lightPink: NamedColor = LightPink

  object LightSalmon extends NamedColor("LightSalmon", 255, 160, 122)
  val lightSalmon: NamedColor = LightSalmon

  object LightSeaGreen extends NamedColor("LightSeaGreen", 32, 178, 170)
  val lightSeaGreen: NamedColor = LightSeaGreen

  object LightSkyBlue extends NamedColor("LightSkyBlue", 135, 206, 250)
  val lightSkyBlue: NamedColor = LightSkyBlue

  object LightSlateGray extends NamedColor("LightSlateGray", 119, 136, 153)
  val lightSlateGray: NamedColor = LightSlateGray

  object LightSteelBlue extends NamedColor("LightSteelBlue", 176, 196, 222)
  val lightSteelBlue: NamedColor = LightSteelBlue

  object LightYellow extends NamedColor("LightYellow", 255, 255, 224)
  val lightYellow: NamedColor = LightYellow

  object Lime extends NamedColor("Lime", 0, 255, 0)
  val lime: NamedColor = Lime

  object LimeGreen extends NamedColor("LimeGreen", 50, 205, 50)
  val limeGreen: NamedColor = LimeGreen

  object Linen extends NamedColor("Linen", 250, 240, 230)
  val linen: NamedColor = Linen

  object Magenta extends NamedColor("Magenta", 255, 0, 255)
  val magenta: NamedColor = Magenta

  object Maroon extends NamedColor("Maroon", 128, 0, 0)
  val maroon: NamedColor = Maroon

  object MediumAquamarine extends NamedColor("MediumAquamarine", 102, 205, 170)
  val mediumAquamarine: NamedColor = MediumAquamarine

  object MediumBlue extends NamedColor("MediumBlue", 0, 0, 205)
  val mediumBlue: NamedColor = MediumBlue

  object MediumOrchid extends NamedColor("MediumOrchid", 186, 85, 211)
  val mediumOrchid: NamedColor = MediumOrchid

  object MediumPurple extends NamedColor("MediumPurple", 147, 112, 219)
  val mediumPurple: NamedColor = MediumPurple

  object MediumSeaGreen extends NamedColor("MediumSeaGreen", 60, 179, 113)
  val mediumSeaGreen: NamedColor = MediumSeaGreen

  object MediumSlateBlue extends NamedColor("MediumSlateBlue", 123, 104, 238)
  val mediumSlateBlue: NamedColor = MediumSlateBlue

  object MediumSpringGreen extends NamedColor("MediumSpringGreen", 0, 250, 154)
  val mediumSpringGreen: NamedColor = MediumSpringGreen

  object MediumTurquoise extends NamedColor("MediumTurquoise", 72, 209, 204)
  val mediumTurquoise: NamedColor = MediumTurquoise

  object MediumVioletRed extends NamedColor("MediumVioletRed", 199, 21, 133)
  val mediumVioletRed: NamedColor = MediumVioletRed

  object MidnightBlue extends NamedColor("MidnightBlue", 25, 25, 112)
  val midnightBlue: NamedColor = MidnightBlue

  object MintCream extends NamedColor("MintCream", 245, 255, 250)
  val mintCream: NamedColor = MintCream

  object MistyRose extends NamedColor("MistyRose", 255, 228, 225)
  val mistyRose: NamedColor = MistyRose

  object Moccasin extends NamedColor("Moccasin", 255, 228, 181)
  val moccasin: NamedColor = Moccasin

  object NavajoWhite extends NamedColor("NavajoWhite", 255, 222, 173)
  val navajoWhite: NamedColor = NavajoWhite

  object Navy extends NamedColor("Navy", 0, 0, 128)
  val navy: NamedColor = Navy

  object OldLace extends NamedColor("OldLace", 253, 245, 230)
  val oldLace: NamedColor = OldLace

  object Olive extends NamedColor("Olive", 128, 128, 0)
  val olive: NamedColor = Olive

  object OliveDrab extends NamedColor("OliveDrab", 107, 142, 35)
  val oliveDrab: NamedColor = OliveDrab

  object Orange extends NamedColor("Orange", 255, 165, 0)
  val orange: NamedColor = Orange

  object OrangeRed extends NamedColor("OrangeRed", 255, 69, 0)
  val orangeRed: NamedColor = OrangeRed

  object Orchid extends NamedColor("Orchid", 218, 112, 214)
  val orchid: NamedColor = Orchid

  object PaleGoldenrod extends NamedColor("PaleGoldenrod", 238, 232, 170)
  val paleGoldenrod: NamedColor = PaleGoldenrod

  object PaleGreen extends NamedColor("PaleGreen", 152, 251, 152)
  val paleGreen: NamedColor = PaleGreen

  object PaleTurquoise extends NamedColor("PaleTurquoise", 175, 238, 238)
  val paleTurquoise: NamedColor = PaleTurquoise

  object PaleVioletRed extends NamedColor("PaleVioletRed", 219, 112, 147)
  val paleVioletRed: NamedColor = PaleVioletRed

  object PapayaWhip extends NamedColor("PapayaWhip", 255, 239, 213)
  val papayaWhip: NamedColor = PapayaWhip

  object PeachPuff extends NamedColor("PeachPuff", 255, 218, 185)
  val peachPuff: NamedColor = PeachPuff

  object Peru extends NamedColor("Peru", 205, 133, 63)
  val peru: NamedColor = Peru

  object Pink extends NamedColor("Pink", 255, 192, 203)
  val pink: NamedColor = Pink

  object Plum extends NamedColor("Plum", 221, 160, 221)
  val plum: NamedColor = Plum

  object PowderBlue extends NamedColor("PowderBlue", 176, 224, 230)
  val powderBlue: NamedColor = PowderBlue

  object Purple extends NamedColor("Purple", 128, 0, 128)
  val purple: NamedColor = Purple

  object Red extends NamedColor("Red", 255, 0, 0)
  val red: NamedColor = Red

  object RosyBrown extends NamedColor("RosyBrown", 188, 143, 143)
  val rosyBrown: NamedColor = RosyBrown

  object RoyalBlue extends NamedColor("RoyalBlue", 65, 105, 225)
  val royalBlue: NamedColor = RoyalBlue

  object SaddleBrown extends NamedColor("SaddleBrown", 139, 69, 19)
  val saddleBrown: NamedColor = SaddleBrown

  object Salmon extends NamedColor("Salmon", 250, 128, 114)
  val salmon: NamedColor = Salmon

  object SandyBrown extends NamedColor("SandyBrown", 244, 164, 96)
  val sandyBrown: NamedColor = SandyBrown

  object SeaGreen extends NamedColor("SeaGreen", 46, 139, 87)
  val seaGreen: NamedColor = SeaGreen

  object SeaShell extends NamedColor("SeaShell", 255, 245, 238)
  val seaShell: NamedColor = SeaShell

  object Sienna extends NamedColor("Sienna", 160, 82, 45)
  val sienna: NamedColor = Sienna

  object Silver extends NamedColor("Silver", 192, 192, 192)
  val silver: NamedColor = Silver

  object SkyBlue extends NamedColor("SkyBlue", 135, 206, 235)
  val skyBlue: NamedColor = SkyBlue

  object SlateBlue extends NamedColor("SlateBlue", 106, 90, 205)
  val slateBlue: NamedColor = SlateBlue

  object SlateGray extends NamedColor("SlateGray", 112, 128, 144)
  val slateGray: NamedColor = SlateGray

  object Snow extends NamedColor("Snow", 255, 250, 250)
  val snow: NamedColor = Snow

  object SpringGreen extends NamedColor("SpringGreen", 0, 255, 127)
  val springGreen: NamedColor = SpringGreen

  object SteelBlue extends NamedColor("SteelBlue", 70, 130, 180)
  val steelBlue: NamedColor = SteelBlue

  object Tan extends NamedColor("Tan", 210, 180, 140)
  val tan: NamedColor = Tan

  object Teal extends NamedColor("Teal", 0, 128, 128)
  val teal: NamedColor = Teal

  object Thistle extends NamedColor("Thistle", 216, 191, 216)
  val thistle: NamedColor = Thistle

  object Tomato extends NamedColor("Tomato", 255, 99, 71)
  val tomato: NamedColor = Tomato

  object Turquoise extends NamedColor("Turquoise", 64, 224, 208)
  val turquoise: NamedColor = Turquoise

  object Violet extends NamedColor("Violet", 238, 130, 238)
  val violet: NamedColor = Violet

  object Wheat extends NamedColor("Wheat", 245, 222, 179)
  val wheat: NamedColor = Wheat

  object White extends NamedColor("White", 255, 255, 255)
  val white: NamedColor = White

  object WhiteSmoke extends NamedColor("WhiteSmoke", 245, 245, 245)
  val whiteSmoke: NamedColor = WhiteSmoke

  object Yellow extends NamedColor("Yellow", 255, 255, 0)
  val yellow: NamedColor = Yellow

  object YellowGreen extends NamedColor("YellowGreen", 154, 205, 50)
  val yellowGreen: NamedColor = YellowGreen

  /* code gen notes:

     1. compile data into following format:

AliceBlue@240,248,255@
AntiqueWhite@250,235,215@
Aqua@0,255,255@
Aquamarine@127,255,212@
Azure@240,255,255@
Beige@245,245,220@
...

     2. transform:

sed 's/,/, /g' |
perl -ne 'm/([^@]+)@([^@]+)@/;
          print qq[  object $1 extends NamedColor("$1", $2)\n];
          my ($name, $modName) = ($1, $1);
          if ($modName =~ s/([a-z])([A-Z])/${1}_$2/g) {
  print qq[  val $modName: NamedColor = $name\n  val \L$modName\E: NamedColor = $name\n  val \L$name\E:  NamedColor = $name\n\n]
          } else {
  print qq[  val \L$name\E: NamedColor = $name\n\n]
          }'

     3. add an extra space (manually) after ':', when name has > 2 words; e.g.:

  val lightsteelblue:  NamedColor = LightSteelBlue

        becomes:

  val lightsteelblue:   NamedColor = LightSteelBlue

  */
}

}

package object ink {
  val DefaultShapePen   = Pen(NamedColors.Black, NullInk)
  val DefaultWritingPen = Pen(NullInk, NamedColors.Black)
  val DefaultPen         = DefaultShapePen
}
