/*
   file: k_k_/graphics/tie/ink.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie

package ink {

import scala.collection.immutable.HashMap
import scala.collection.mutable

import k_k_.graphics.tie.effects.{Opacity_Effect, Opacity}
import k_k_.graphics.tie.shapes.{Drawing_Shape, Point}
import k_k_.graphics.tie.transform._


sealed abstract class Ink {

  def opacity: Double =
    1.0

  def alpha(opacity: Opacity_Effect): Ink =
    create_non_opaque_ink(opacity)

  def alpha(opacity: Double): Ink =
    alpha(Opacity(opacity))

  def -#(opacity: Opacity_Effect): Ink =
    alpha(opacity)

  def -#(opacity: Double): Ink =
    alpha(opacity)


  protected def create_non_opaque_ink(opacity_effect: Opacity_Effect): Ink =
    if (opacity_effect.opacity == 1.0) this
    else Non_Opaque_Ink(this, opacity_effect.opacity)
}


case object Null_Ink extends Ink {

  override
  protected def create_non_opaque_ink(opacity_effect: Opacity_Effect): Ink =
    Null_Ink // opacity is meaningless for `Null_Ink`
}


object Color {

  def apply(red: Int, green: Int, blue: Int) =
    new Color(red, green, blue)

  def unapply(color: Color): Option[(Int, Int, Int)] =
    Some(color.red, color.green, color.blue)

  def named(name: String): Option[Named_Color] =
    Named_Color.named(name)

  def with_hsl(hue: Double, saturation: Double, lightness: Double): Color =
    new HSL_Projection(((hue % 360) + 360) % 360,
                       math.abs(saturation % 2),
                       math.abs(lightness % 2))


  private def lerp(x: Int, y: Int, proportion: Double): Int = {
    val pct = clamp_unit(proportion)
    x + ((y - x) * pct).round.toInt
  }

  private def lerp(x: Double, y: Double, proportion: Double): Double = {
    val pct = clamp_unit(proportion)
    x + ((y - x) * pct)
  }

  private def stepwise_to_1(step: Double): Seq[Double] =
    clamp_unit(step) until 1.0 by step

  private def clamp_unit(value: Double) =
    value max 0.0 min 1.0


  // 'Hue, Saturation, Lightness' cylindrical projection of RGB cube
  // adapted from: http://en.wikipedia.org/wiki/HSL_color_space
  private object HSL_Projection {

    def apply(rgb_color: Color) = {
      val (r, g, b) = (rgb_color.red   / 255.0,
                       rgb_color.green / 255.0,
                       rgb_color.blue  / 255.0)
      val (min, max) = (r min g min b, r max g max b)
      val chroma = max - min        // diff of largest and smallest components
      val lightness = (min + max)/2 // avg. of largest and smallest components
      val saturation = if (chroma == 0.0) 0.0
                       else chroma / (1 - math.abs(2*lightness - 1))
      val hue = {
        val H = if (chroma == 0.0)   0.0
                else if (max == r)   (g - b)/chroma
                else if (max == g)   (b - r)/chroma + 2
                else  /*(max == b)*/ (r - g)/chroma + 4
        H * 60 // convert ordinal sextant of hexagon to degrees of (of circle)
      }
      new HSL_Projection(if (hue < 0.0) hue + 360 else hue,
                         saturation, lightness, chroma)
    }

    implicit def to_Color(hsl: HSL_Projection): Color =
      hsl.as_rgb
  }

  // HSL cylindrical geometry uses (degrees) rotation about axis to specify hue:
  // primary: 0 == red        120 == green       240 == blue          360 == red
  // secondary:    [60 == yellow]      [180 == cyan]     [300 == magenta]
  private class HSL_Projection private (val hue: Double,        // [ 0.0,360.0 )
                                        val saturation: Double, // [ 0.0,  1.0 ]
                                        val lightness: Double,  // [ 0.0,  1.0 ]
                                        chroma: Double) {

    def this(hue: Double, saturation: Double, lightness: Double) =
      this(hue, saturation, lightness,
           saturation * (1 - math.abs(2*lightness - 1)))


    def rotate(degrees: Double) =
      new HSL_Projection((hue + degrees) % 360, saturation, lightness, chroma)


    def saturate(amount: Double) =   // amount: [ 0.0, 1.0 ]
      new HSL_Projection(hue, clamp_unit(saturation + amount), lightness)

    def desaturate(amount: Double) = // amount: [ 0.0, 1.0 ]
      new HSL_Projection(hue, clamp_unit(saturation - amount), lightness)

    def is_fully_saturated:   Boolean = saturation == 1.0
    def is_fully_desaturated: Boolean = saturation == 0.0


    def lighten(amount: Double) =    // amount: [ 0.0, 1.0 ]
      new HSL_Projection(hue, saturation, clamp_unit(lightness + amount))

    def darken(amount: Double) =     // amount: [ 0.0, 1.0 ]
      new HSL_Projection(hue, saturation, clamp_unit(lightness - amount))

    def is_fully_light: Boolean = lightness == 1.0
    def is_fully_dark:  Boolean = lightness == 0.0


    def transition(other: HSL_Projection, stop: Double): HSL_Projection =
      new HSL_Projection(lerp(hue,        other.hue,        stop),
                         lerp(saturation, other.saturation, stop),
                         lerp(lightness,  other.lightness,  stop))

    def transitions(other: HSL_Projection, stops: Seq[Double]):
        Seq[HSL_Projection] =
      stops.view.map(transition(other, _))

    def transitions_by(other: HSL_Projection, step: Double):
        Seq[HSL_Projection] =
      if (step <= 0.0) Seq.empty
      else transitions(other, stepwise_to_1(step))


    def transition_hue(other: HSL_Projection, stop: Double): HSL_Projection =
      new HSL_Projection(lerp(hue, other.hue, stop),
                         saturation, lightness, chroma)

    def transition_hues(other: HSL_Projection, stops: Seq[Double]):
        Seq[HSL_Projection] =
      stops.view.map(transition_hue(other, _))

    def transition_hues_by(other: HSL_Projection, step: Double):
        Seq[HSL_Projection] =
      if (step <= 0.0) Seq.empty
      else transition_hues(other, stepwise_to_1(step))


    def transition_saturation(other: HSL_Projection, stop: Double):
        HSL_Projection =
      new HSL_Projection(hue,lerp(saturation, other.saturation, stop),lightness)

    def transition_saturations(other: HSL_Projection, stops: Seq[Double]):
        Seq[HSL_Projection] =
      stops.view.map(transition_saturation(other, _))

    def transition_saturations_by(other: HSL_Projection, step: Double):
        Seq[HSL_Projection] =
      if (step <= 0.0) Seq.empty
      else transition_saturations(other, stepwise_to_1(step))


    def transition_lightness(other: HSL_Projection, stop: Double):
        HSL_Projection =
      new HSL_Projection(hue,saturation, lerp(lightness, other.lightness, stop))

    def transition_lightnesses(other: HSL_Projection, stops: Seq[Double]):
        Seq[HSL_Projection] =
      stops.view.map(transition_lightness(other, _))

    def transition_lightnesses_by(other: HSL_Projection, step: Double):
        Seq[HSL_Projection] =
      if (step <= 0.0) Seq.empty
      else transition_lightnesses(other, stepwise_to_1(step))


    lazy val as_rgb: Color =
      if (saturation == 0.0) {
        val component = (255 * lightness).round.toInt
        new Color(component, component, component, Some(this))
      } else {
        val H = hue / 60
        val X = chroma * (1 - math.abs((H % 2) - 1))
        val (r, g, b) = if      (H <  1.0)   (chroma,      X,    0.0)
                        else if (H <  2.0)   (X,      chroma,    0.0)
                        else if (H <  3.0)   (0.0,    chroma,      X)
                        else if (H <  4.0)   (0.0,         X, chroma)
                        else if (H <  5.0)   (X,         0.0, chroma)
                        else  /*(H <  6.0)*/ (chroma,    0.0,      X)
        val lightness_adjust = lightness - chroma/2
        val (red, green, blue) = (r + lightness_adjust,
                                  g + lightness_adjust,
                                  b + lightness_adjust)
        new Color((red   * 255).round.toInt,
                  (green * 255).round.toInt,
                  (blue  * 255).round.toInt,
                  Some(this))
      }
  }
}

sealed class Color private (val red: Int, val green: Int, val blue: Int,
                            hsl_projection: Option[AnyRef])

//                          hsl_projection: Option[Color.HSL_Projection])
//????not sure why it's not possible to access the type in the constructor, yet
// it is perfectly acceptible below in the def. of hsl_equiv????
//[ERROR] .../tie/tie/src/main/scala/k_k_/graphics/tie/ink.scala:146: error: class HSL_Projection cannot be accessed in object k_k_.graphics.tie.ink.Color
//[INFO]                             hsl_projection: Option[Color.HSL_Projection])
//[INFO]                                                          ^

    extends Ink {

  def this(red: Int, green: Int, blue: Int) =
    this(math.abs(red % 256), math.abs(green % 256), math.abs(blue % 256), None)


  final def as_color_only: Color =
    new Color(red, green, blue, hsl_projection)


  // NOTE: override with covariant return type
  override
  def alpha(opacity: Opacity_Effect): Color =
    create_non_opaque_ink(opacity)

  override
  def alpha(opacity: Double): Color =
    alpha(Opacity(opacity))

  override
  def -#(opacity: Opacity_Effect): Color =
    alpha(opacity)

  override
  def -#(opacity: Double): Color =
    alpha(opacity)


  // rotate hue, preserving saturation and lightness:

  def rotate(degrees: Double): Color =
    hsl_equiv.rotate(degrees)

  def -%(degrees: Double): Color =
    rotate(degrees)

  final def complement: Color =
    rotate(180)


  def saturate(amount: Double): Color =   // amount: [ 0.0, 1.0 ]
    hsl_equiv.saturate(amount)

  def desaturate(amount: Double): Color = // amount: [ 0.0, 1.0 ]
    saturate(-amount)

  def is_fully_saturated:   Boolean = hsl_equiv.is_fully_saturated
  def is_fully_desaturated: Boolean = hsl_equiv.is_fully_desaturated


  def lighten(amount: Double): Color =    // amount: [ 0.0, 1.0 ]
    hsl_equiv.lighten(amount)

  def darken(amount: Double): Color =     // amount: [ 0.0, 1.0 ]
    lighten(-amount)

  def is_fully_light: Boolean = hsl_equiv.is_fully_light
  def is_fully_dark:  Boolean = hsl_equiv.is_fully_dark


  def transition(other: Color, stop: Double = 0.5): Color =
    hsl_equiv.transition(other.hsl_equiv, stop)

  def transitions(other: Color, stops: Seq[Double]): Seq[Color] =
    hsl_equiv.transitions(other.hsl_equiv, stops).map( _.as_rgb )

  def transitions_by(other: Color, step: Double = 0.1): Seq[Color] =
    hsl_equiv.transitions_by(other.hsl_equiv, step).map( _.as_rgb )


  def transition_hue(other: Color, stop: Double = 0.5): Color =
    hsl_equiv.transition_hue(other.hsl_equiv, stop)

  def transition_hues(other: Color, stops: Seq[Double]): Seq[Color] =
    hsl_equiv.transition_hues(other.hsl_equiv, stops).map( _.as_rgb )

  def transition_hues_by(other: Color, step: Double = 0.1): Seq[Color] =
    hsl_equiv.transition_hues_by(other.hsl_equiv, step).map( _.as_rgb )


  def transition_saturation(other: Color, stop: Double = 0.5): Color =
    hsl_equiv.transition_saturation(other.hsl_equiv, stop)

  def transition_saturations(other: Color, stops: Seq[Double]): Seq[Color] =
    hsl_equiv.transition_saturations(other.hsl_equiv, stops).map( _.as_rgb )

  def transition_saturations_by(other: Color, step: Double = 0.1): Seq[Color] =
    hsl_equiv.transition_saturations_by(other.hsl_equiv, step).map( _.as_rgb )


  def transition_lightness(other: Color, stop: Double = 0.5): Color =
    hsl_equiv.transition_lightness(other.hsl_equiv, stop)

  def transition_lightnesses(other: Color, stops: Seq[Double]): Seq[Color] =
    hsl_equiv.transition_lightnesses(other.hsl_equiv, stops).map( _.as_rgb )

  def transition_lightnesses_by(other: Color, step: Double = 0.1): Seq[Color] =
    hsl_equiv.transition_lightnesses_by(other.hsl_equiv, step).map( _.as_rgb )


  // 'linear interpolation' in RGB color space

  def lerp(other: Color, stop: Double = 0.5): Color =
    Color(Color.lerp(red,   other.red,   stop),
          Color.lerp(green, other.green, stop),
          Color.lerp(blue,  other.blue,  stop))

  def lerps(other: Color, stops: Seq[Double]): Seq[Color] =
    stops.view.map(lerp(other, _))

  def lerps_by(other: Color, step: Double = 0.1): Seq[Color] =
    if (step <= 0.0) Seq.empty
    else lerps(other, Color.stepwise_to_1(step))


  def as_rgb_string =
    "rgb(%d, %d, %d)".format(red, green, blue)

  def as_hex_string =
    "#%02x%02x%02x".format(red, green, blue)

  def as_hsl_string =
    "hsl(%f, %f, %f)".format(hsl_equiv.hue,
                             hsl_equiv.saturation,
                             hsl_equiv.lightness)

  override
  def toString: String =
    as_rgb_string


  override
  def equals(other: Any): Boolean =
    other match {
      case that: Color =>
        that.canEqual(this) && this.red   == that.red   &&
                               this.green == that.green &&
                               this.blue == that.blue
      case _ => false
    }

  override
  def hashCode: Int =
    41 * (
      41 * (
        41 + red
      ) + green
    ) + blue

  def canEqual(other: Any) = other.isInstanceOf[Color]


  override
  protected def create_non_opaque_ink(opacity_effect: Opacity_Effect): Color =
    if (opacity_effect.opacity == 1.0) this
    else Non_Opaque_Color(this, opacity_effect.opacity)


  private lazy val hsl_equiv = hsl_projection match {
    case Some(hsl_proj: Color.HSL_Projection) => hsl_proj
    case None | _                             => Color.HSL_Projection(this)
  }
}


object Named_Color {

  def apply(name: String): Option[Named_Color] =
    named(name)

  def unapply(named_clr: Named_Color): Option[(String, Int, Int, Int)] =
    Some(named_clr.name, named_clr.red, named_clr.green, named_clr.blue)

  def named(name: String): Option[Named_Color] =
    all_named_colors.get(normalize(name))  

  // Notes on the invariants for containment of evil, mutable state:
  // the Named_Color class is sealed, and only extended within the Named_Colors
  // object, once for every CSS2/X11 color name.  During construction, each
  // instance triggers invocation of the private register_color() method on this
  // companion object (the only method to mutate the `registered_colors` Map).
  // since the method is private, and its companion class is impossible to
  // construct elsewhere, it is called only during initialization of the
  // Named_Colors object.  all external use of named-based lookup goes through
  // the lazily evaluated immutable Map `all_named_colors`.  since its
  // lazily-computed val references a member of Named_Colors, the initializer
  // of the latter must have run prior to the one and only read from the
  // mutable 'registered_colors` Map.  Thus, laziness leads to safer code.

  lazy val all_named_colors = {
    // NOTE: use lowercase `val`, not `object`, which won't load enclosing class
    Named_Colors.black // ...like the scourge of mutability ;)
    HashMap(registered_colors.toSeq : _*)
  }

  private def register_color(named: Named_Color) {
    registered_colors += normalize(named.name) -> named
  }

  private def normalize(name: String) =
    name.toLowerCase

  val registered_colors = new mutable.HashMap[String, Named_Color]
}

sealed abstract class Named_Color(val name: String, r: Int, g: Int, b: Int)
    extends Color(r, g, b) {

  Named_Color.register_color(this)

  override
  def toString: String =
    super.toString + "[" + name + "]"

  // NOTE: no override of `equals`, since `name` considered merely meta-data
}

// last-resort fall-back class for Renderer-specific color string.
// caveat user: may lead to fmt-specific code--use sparingly, if ever!
final case class Described_Color(desc: String) extends Color(0, 0, 0) {//(Black)

  override
  def canEqual(other: Any) = other.isInstanceOf[Described_Color]
}

// alt. to Non_Opaque_Ink, which preserves typing within Color sub-hierarchy
final case class Non_Opaque_Color(color: Color, override val opacity: Double)
    extends Color(color.red, color.green, color.blue) {

  override
  def rotate(degrees: Double): Color =
    copy(color = super.rotate(degrees))

  override
  def saturate(amount: Double): Color =   // amount: [ 0.0, 1.0 ]
    copy(color = super.saturate(amount))

  override
  def lighten(amount: Double): Color =    // amount: [ 0.0, 1.0 ]
    copy(color = super.lighten(amount))


  override
  def transition(other: Color, stop: Double = 0.5): Color =
    copy(color = super.transition(other, stop))

  override
  def transitions(other: Color, stops: Seq[Double]): Seq[Color] =
    super.transitions(other, stops).map( _ alpha opacity )

  override
  def transitions_by(other: Color, step: Double = 0.1): Seq[Color] =
    super.transitions_by(other, step).map( _ alpha opacity )


  override
  def transition_hue(other: Color, stop: Double = 0.5): Color =
    copy(color = super.transition_hue(other, stop))

  override
  def transition_hues(other: Color, stops: Seq[Double]): Seq[Color] =
    super.transition_hues(other, stops).map( _ alpha opacity )

  override
  def transition_hues_by(other: Color, step: Double = 0.1): Seq[Color] =
    super.transition_hues_by(other, step).map( _ alpha opacity )


  override
  def transition_saturation(other: Color, stop: Double = 0.5): Color =
    copy(color = super.transition_saturation(other, stop))

  override
  def transition_saturations(other: Color, stops: Seq[Double]): Seq[Color] =
    super.transition_saturations(other, stops).map( _ alpha opacity )

  override
  def transition_saturations_by(other: Color, step: Double = 0.1): Seq[Color] =
    super.transition_saturations_by(other, step).map( _ alpha opacity )


  override
  def transition_lightness(other: Color, stop: Double = 0.5): Color =
    copy(color = super.transition_lightness(other, stop))

  override
  def transition_lightnesses(other: Color, stops: Seq[Double]): Seq[Color] =
    super.transition_lightnesses(other, stops).map( _ alpha opacity )

  override
  def transition_lightnesses_by(other: Color, step: Double = 0.1): Seq[Color] =
    super.transition_lightnesses_by(other, step).map( _ alpha opacity )


  override
  def toString: String =
    super.toString + "(alpha = " + opacity + ")"

  override
  def canEqual(other: Any) = other.isInstanceOf[Non_Opaque_Color]


  override
  protected def create_non_opaque_ink(opacity_effect: Opacity_Effect): Color = {
    val combo = opacity * opacity_effect.opacity
    if (combo == 1.0) color
    else              copy(opacity = combo)
  }
}


object Non_Opaque_Ink {

  //????what changed???? scala 2.9.0--but not 2.8.1!--gives error:
  // [error] .../tie/src/main/scala/k_k_/graphics/tie/ink.scala:576: cannot
  //         resolve overloaded unapply
  // final case class Non_Opaque_Ink(ink: Ink, override val opacity: Double)
  // 
  // def unapply(x: AnyRef): Option[(Ink, Double)] =
  //
  // ...thus, make Non_Opaque_Ink non-case-class, but fill in (needed) methods
  // of companion here

  def apply(ink: Ink, opacity: Double) =
    new Non_Opaque_Ink(ink, opacity)

  def unapply(x: AnyRef): Option[(Ink, Double)] =
    x match {
      case non_opaque: Non_Opaque_Ink =>
        Some(non_opaque.ink, non_opaque.opacity)
      case non_opaque: Non_Opaque_Color =>
        Some(non_opaque.color, non_opaque.opacity)
      case _ =>
        None
    }
}

final class Non_Opaque_Ink(val ink: Ink, override val opacity: Double)
    extends Ink {

  def copy(ink: Ink = this.ink, opacity: Double = this.opacity) =
    new Non_Opaque_Ink(ink, opacity)

  override
  protected def create_non_opaque_ink(opacity_effect: Opacity_Effect): Ink = {
    val combo = opacity * opacity_effect.opacity
    if (combo == 1.0) ink
    else              copy(opacity = combo)
  }
}


object Pattern {

  def apply(shape: Drawing_Shape, width: Double, height: Double): Pattern =
    new Shape_Pattern(shape, width, height)

  def apply(shape: Drawing_Shape): Pattern =
    new Shape_Pattern(shape)
}

sealed abstract class Pattern
    extends Ink
       with Transforming[Pattern]
       with Placeable[Pattern] {

  type Translated_T          = Translated_Pattern
  protected val Translated   = Translated_Pattern

  type Scaled_T              = Scaled_Pattern
  protected val Scaled       = Scaled_Pattern

  type Rotated_T             = Rotated_Pattern
  protected val Rotated      = Rotated_Pattern

  type Reflected_T           = Reflected_Pattern
  protected val Reflected    = Reflected_Pattern

  type Skewed_Horiz_T        = Skewed_Horiz_Pattern
  protected val Skewed_Horiz = Skewed_Horiz_Pattern

  type Skewed_Vert_T         = Skewed_Vert_Pattern
  protected val Skewed_Vert  = Skewed_Vert_Pattern
}

object Shape_Pattern {

  def apply(shape: Drawing_Shape): Pattern =
    new Shape_Pattern(shape)
}

final case class Shape_Pattern(shape: Drawing_Shape,
                               width: Double, height: Double)
    extends Pattern {

  def this(shape: Drawing_Shape) =
    this(shape, shape.bounding_box.width, shape.bounding_box.height)

  def center_pt: Point =
    shape.center_pt
}


sealed abstract class Transformed_Pattern extends Pattern {

  val pattern: Pattern

  def center_pt: Point =
    pattern.center_pt
}

object Translated_Pattern extends Translated_Transformable[Pattern] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Translated_Pattern]
}

final case class Translated_Pattern(pattern: Pattern,
                                    x_move: Double, y_move: Double)
    extends Transformed_Pattern


object Scaled_Pattern extends Scaled_Transformable[Pattern] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Scaled_Pattern]
}

final case class Scaled_Pattern(pattern: Pattern,
                                x_scaling: Double, y_scaling: Double)
    extends Transformed_Pattern


object Rotated_Pattern extends Rotated_Transformable[Pattern] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Rotated_Pattern]
}

final case class Rotated_Pattern(pattern: Pattern, degrees: Double,
                                 about_x: Double, about_y: Double)
    extends Transformed_Pattern


object Reflected_Pattern extends Reflected_Transformable[Pattern] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Reflected_Pattern]
}

final case class Reflected_Pattern(pattern: Pattern, degrees: Double,
                                 about_x: Double, about_y: Double)
    extends Transformed_Pattern


object Skewed_Horiz_Pattern extends Skewed_Horiz_Transformable[Pattern] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Skewed_Horiz_Pattern]
}

final case class Skewed_Horiz_Pattern(pattern: Pattern, degrees: Double)
    extends Transformed_Pattern


object Skewed_Vert_Pattern extends Skewed_Vert_Transformable[Pattern] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Skewed_Vert_Pattern]
}

final case class Skewed_Vert_Pattern(pattern: Pattern, degrees: Double)
    extends Transformed_Pattern



sealed abstract class Color_Spread
case object Pad_Colors     extends Color_Spread
case object Reflect_Colors extends Color_Spread
case object Repeat_Colors  extends Color_Spread


sealed abstract class Color_Interpolation
case object sRGB_Interpolation       extends Color_Interpolation
case object Linear_RGB_Interpolation extends Color_Interpolation


// `stop_offset_pct`: [ 0.0, 100.0 ]
final case class Color_Stop(color: Color, stop_offset_pct: Double) {

  def this(color: Color, stop_offset_pct: Double, opacity: Double) =
    this(color alpha opacity, stop_offset_pct)

  def this(color: Color, stop_offset_pct: Double, opacity: Opacity_Effect) =
    this(color alpha opacity, stop_offset_pct)


  val opacity = color.opacity

  def alpha(opacity: Opacity_Effect): Color_Stop =
    copy(color = color alpha opacity)

  def alpha(opacity: Double): Color_Stop =
    alpha(Opacity(opacity))

  def -#(opacity: Opacity_Effect): Color_Stop =
    alpha(opacity)

  def -#(opacity: Double): Color_Stop =
    alpha(opacity)
}


sealed abstract class Gradient extends Ink with Transforming[Gradient] {

  type Translated_T          = Translated_Gradient
  protected val Translated   = Translated_Gradient

  type Scaled_T              = Scaled_Gradient
  protected val Scaled       = Scaled_Gradient

  type Rotated_T             = Rotated_Gradient
  protected val Rotated      = Rotated_Gradient

  type Reflected_T           = Reflected_Gradient
  protected val Reflected    = Reflected_Gradient

  type Skewed_Horiz_T        = Skewed_Horiz_Gradient
  protected val Skewed_Horiz = Skewed_Horiz_Gradient

  type Skewed_Vert_T         = Skewed_Vert_Gradient
  protected val Skewed_Vert  = Skewed_Vert_Gradient


  def restyle: Gradient // converts between Linear_Gradient <-> Radial_Gradient
}


trait Gradient_Construction[T <: Gradient] {

  def apply(colors: Seq[Color_Stop], opacity: Opacity_Effect): T =
    apply(colors map ( _ alpha opacity ))

  def apply(colors: Seq[Color_Stop], opacity: Opacity_Effect,
            spread: Color_Spread): T =
    apply(colors map ( _ alpha opacity ), spread)

  def apply(colors: Seq[Color_Stop], opacity: Opacity_Effect,
            spread: Color_Spread, interp: Color_Interpolation): T =
    apply(colors map ( _ alpha opacity ), spread, interp)


  def apply(colors: Seq[Color_Stop], opacity: Double): T =
    apply(colors, Opacity(opacity))

  def apply(colors: Seq[Color_Stop], opacity: Double, spread: Color_Spread): T =
    apply(colors, Opacity(opacity), spread)

  def apply(colors: Seq[Color_Stop], opacity: Double, spread: Color_Spread,
            interp: Color_Interpolation): T =
    apply(colors, Opacity(opacity), spread, interp)


  def uniform(colors: Seq[Color],
              opacity: Opacity_Effect     = Opacity(1.0),
              spread: Color_Spread        = Pad_Colors,
              interp: Color_Interpolation = sRGB_Interpolation): T = {
    val each_offset_pct = 100.0 / (colors.length - 1)
    apply(colors.zipWithIndex.map(p =>Color_Stop(p._1, p._2 * each_offset_pct)),
          opacity, spread, interp)
  }


  def uniform(colors: Seq[Color_Stop], spread: Color_Spread): T =
    uniform(colors, spread = spread)

  def uniform(colors: Seq[Color_Stop], spread: Color_Spread,
              interp: Color_Interpolation): T =
    uniform(colors, spread = spread, interp = interp)


  def uniform(colors: Seq[Color], opacity: Double): T =
    uniform(colors, Opacity(opacity))

  def uniform(colors: Seq[Color], opacity: Double, spread: Color_Spread): T =
    uniform(colors, Opacity(opacity), spread)

  def uniform(colors: Seq[Color], opacity: Double, spread: Color_Spread,
              interp: Color_Interpolation): T =
    uniform(colors, Opacity(opacity), spread, interp)


 def apply(color_stops: Seq[Color_Stop],
           spread: Color_Spread = Pad_Colors,
           interp: Color_Interpolation = sRGB_Interpolation): T
}


object Linear_Gradient extends Gradient_Construction[Linear_Gradient]

final case class Linear_Gradient(color_stops: Seq[Color_Stop],
                                 spread: Color_Spread = Pad_Colors,
                                 interp: Color_Interpolation =
                                   sRGB_Interpolation)
    extends Gradient {

  def restyle: Gradient =
    as_radial

  def as_radial: Radial_Gradient =
    Radial_Gradient(color_stops, spread, interp)
}


object Radial_Gradient extends Gradient_Construction[Radial_Gradient]

final case class Radial_Gradient(color_stops: Seq[Color_Stop],
                                 spread: Color_Spread = Pad_Colors,
                                 interp: Color_Interpolation =
                                   sRGB_Interpolation)
    extends Gradient {

  def restyle: Gradient =
    as_linear

  def as_linear: Linear_Gradient =
    Linear_Gradient(color_stops, spread, interp)
}


sealed abstract class Transformed_Gradient extends Gradient {

  def restyle: Gradient =
    copy_grad(gradient.restyle)

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
  protected def copy_grad(g: Gradient): Gradient
}


object Translated_Gradient extends Translated_Transformable[Gradient] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Translated_Gradient]
}

final case class Translated_Gradient(gradient: Gradient,
                                     x_move: Double, y_move: Double)
    extends Transformed_Gradient {

  protected def copy_grad(g: Gradient): Gradient =
    copy(gradient = g)
}


object Scaled_Gradient extends Scaled_Transformable[Gradient] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Scaled_Gradient]
}

final case class Scaled_Gradient(gradient: Gradient,
                                 x_scaling: Double, y_scaling: Double)
    extends Transformed_Gradient {

  protected def copy_grad(g: Gradient): Gradient =
    copy(gradient = g)
}


object Rotated_Gradient extends Rotated_Transformable[Gradient] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Rotated_Gradient]
}

final case class Rotated_Gradient(gradient: Gradient, degrees: Double,
                                  about_x: Double, about_y: Double)
    extends Transformed_Gradient {

  protected def copy_grad(g: Gradient): Gradient =
    copy(gradient = g)
}


object Reflected_Gradient extends Reflected_Transformable[Gradient] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Reflected_Gradient]
}

final case class Reflected_Gradient(gradient: Gradient, degrees: Double,
                                    about_x: Double, about_y: Double)
    extends Transformed_Gradient {

  protected def copy_grad(g: Gradient): Gradient =
    copy(gradient = g)
}


object Skewed_Horiz_Gradient extends Skewed_Horiz_Transformable[Gradient] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Skewed_Horiz_Gradient]
}

final case class Skewed_Horiz_Gradient(gradient: Gradient, degrees: Double)
    extends Transformed_Gradient {

  protected def copy_grad(g: Gradient): Gradient =
    copy(gradient = g)
}


object Skewed_Vert_Gradient extends Skewed_Vert_Transformable[Gradient] {

  protected def isInstanceOfCompanion(x: Any): Boolean =
    x.isInstanceOf[Skewed_Vert_Gradient]
}

final case class Skewed_Vert_Gradient(gradient: Gradient, degrees: Double)
    extends Transformed_Gradient {

  protected def copy_grad(g: Gradient): Gradient =
    copy(gradient = g)
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
object Named_Colors {

  object AliceBlue extends Named_Color("AliceBlue", 240, 248, 255)
  val Alice_Blue: Named_Color = AliceBlue
  val alice_blue: Named_Color = AliceBlue
  val aliceblue:  Named_Color = AliceBlue

  object AntiqueWhite extends Named_Color("AntiqueWhite", 250, 235, 215)
  val Antique_White: Named_Color = AntiqueWhite
  val antique_white: Named_Color = AntiqueWhite
  val antiquewhite:  Named_Color = AntiqueWhite

  object Aqua extends Named_Color("Aqua", 0, 255, 255)
  val aqua: Named_Color = Aqua

  object Aquamarine extends Named_Color("Aquamarine", 127, 255, 212)
  val aquamarine: Named_Color = Aquamarine

  object Azure extends Named_Color("Azure", 240, 255, 255)
  val azure: Named_Color = Azure

  object Beige extends Named_Color("Beige", 245, 245, 220)
  val beige: Named_Color = Beige

  object Bisque extends Named_Color("Bisque", 255, 228, 196)
  val bisque: Named_Color = Bisque

  object Black extends Named_Color("Black", 0, 0, 0)
  val black: Named_Color = Black

  object BlanchedAlmond extends Named_Color("BlanchedAlmond", 255, 235, 205)
  val Blanched_Almond: Named_Color = BlanchedAlmond
  val blanched_almond: Named_Color = BlanchedAlmond
  val blanchedalmond:  Named_Color = BlanchedAlmond

  object Blue extends Named_Color("Blue", 0, 0, 255)
  val blue: Named_Color = Blue

  object BlueViolet extends Named_Color("BlueViolet", 138, 43, 226)
  val Blue_Violet: Named_Color = BlueViolet
  val blue_violet: Named_Color = BlueViolet
  val blueviolet:  Named_Color = BlueViolet

  object Brown extends Named_Color("Brown", 165, 42, 42)
  val brown: Named_Color = Brown

  object BurlyWood extends Named_Color("BurlyWood", 222, 184, 135)
  val Burly_Wood: Named_Color = BurlyWood
  val burly_wood: Named_Color = BurlyWood
  val burlywood:  Named_Color = BurlyWood

  object CadetBlue extends Named_Color("CadetBlue", 95, 158, 160)
  val Cadet_Blue: Named_Color = CadetBlue
  val cadet_blue: Named_Color = CadetBlue
  val cadetblue:  Named_Color = CadetBlue

  object Chartreuse extends Named_Color("Chartreuse", 127, 255, 0)
  val chartreuse: Named_Color = Chartreuse

  object Chocolate extends Named_Color("Chocolate", 210, 105, 30)
  val chocolate: Named_Color = Chocolate

  object Coral extends Named_Color("Coral", 255, 127, 80)
  val coral: Named_Color = Coral

  object CornflowerBlue extends Named_Color("CornflowerBlue", 100, 149, 237)
  val Cornflower_Blue: Named_Color = CornflowerBlue
  val cornflower_blue: Named_Color = CornflowerBlue
  val cornflowerblue:  Named_Color = CornflowerBlue

  object Cornsilk extends Named_Color("Cornsilk", 255, 248, 220)
  val cornsilk: Named_Color = Cornsilk

  object Crimson extends Named_Color("Crimson", 220, 20, 60)
  val crimson: Named_Color = Crimson

  object Cyan extends Named_Color("Cyan", 0, 255, 255)
  val cyan: Named_Color = Cyan

  object DarkBlue extends Named_Color("DarkBlue", 0, 0, 139)
  val Dark_Blue: Named_Color = DarkBlue
  val dark_blue: Named_Color = DarkBlue
  val darkblue:  Named_Color = DarkBlue

  object DarkCyan extends Named_Color("DarkCyan", 0, 139, 139)
  val Dark_Cyan: Named_Color = DarkCyan
  val dark_cyan: Named_Color = DarkCyan
  val darkcyan:  Named_Color = DarkCyan

  object DarkGoldenrod extends Named_Color("DarkGoldenrod", 184, 134, 11)
  val Dark_Goldenrod: Named_Color = DarkGoldenrod
  val dark_goldenrod: Named_Color = DarkGoldenrod
  val darkgoldenrod:  Named_Color = DarkGoldenrod

  object DarkGray extends Named_Color("DarkGray", 169, 169, 169)
  val Dark_Gray: Named_Color = DarkGray
  val dark_gray: Named_Color = DarkGray
  val darkgray:  Named_Color = DarkGray

  object DarkGreen extends Named_Color("DarkGreen", 0, 100, 0)
  val Dark_Green: Named_Color = DarkGreen
  val dark_green: Named_Color = DarkGreen
  val darkgreen:  Named_Color = DarkGreen

  object DarkKhaki extends Named_Color("DarkKhaki", 189, 183, 107)
  val Dark_Khaki: Named_Color = DarkKhaki
  val dark_khaki: Named_Color = DarkKhaki
  val darkkhaki:  Named_Color = DarkKhaki

  object DarkMagenta extends Named_Color("DarkMagenta", 139, 0, 139)
  val Dark_Magenta: Named_Color = DarkMagenta
  val dark_magenta: Named_Color = DarkMagenta
  val darkmagenta:  Named_Color = DarkMagenta

  object DarkOliveGreen extends Named_Color("DarkOliveGreen", 85, 107, 47)
  val Dark_Olive_Green: Named_Color = DarkOliveGreen
  val dark_olive_green: Named_Color = DarkOliveGreen
  val darkolivegreen:   Named_Color = DarkOliveGreen

  object DarkOrange extends Named_Color("DarkOrange", 255, 140, 0)
  val Dark_Orange: Named_Color = DarkOrange
  val dark_orange: Named_Color = DarkOrange
  val darkorange:  Named_Color = DarkOrange

  object DarkOrchid extends Named_Color("DarkOrchid", 153, 50, 204)
  val Dark_Orchid: Named_Color = DarkOrchid
  val dark_orchid: Named_Color = DarkOrchid
  val darkorchid:  Named_Color = DarkOrchid

  object DarkRed extends Named_Color("DarkRed", 139, 0, 0)
  val Dark_Red: Named_Color = DarkRed
  val dark_red: Named_Color = DarkRed
  val darkred:  Named_Color = DarkRed

  object DarkSalmon extends Named_Color("DarkSalmon", 233, 150, 122)
  val Dark_Salmon: Named_Color = DarkSalmon
  val dark_salmon: Named_Color = DarkSalmon
  val darksalmon:  Named_Color = DarkSalmon

  object DarkSeaGreen extends Named_Color("DarkSeaGreen", 143, 188, 139)
  val Dark_Sea_Green: Named_Color = DarkSeaGreen
  val dark_sea_green: Named_Color = DarkSeaGreen
  val darkseagreen:   Named_Color = DarkSeaGreen

  object DarkSlateBlue extends Named_Color("DarkSlateBlue", 72, 61, 139)
  val Dark_Slate_Blue: Named_Color = DarkSlateBlue
  val dark_slate_blue: Named_Color = DarkSlateBlue
  val darkslateblue:   Named_Color = DarkSlateBlue

  object DarkSlateGray extends Named_Color("DarkSlateGray", 47, 79, 79)
  val Dark_Slate_Gray: Named_Color = DarkSlateGray
  val dark_slate_gray: Named_Color = DarkSlateGray
  val darkslategray:   Named_Color = DarkSlateGray

  object DarkTurquoise extends Named_Color("DarkTurquoise", 0, 206, 209)
  val Dark_Turquoise: Named_Color = DarkTurquoise
  val dark_turquoise: Named_Color = DarkTurquoise
  val darkturquoise:  Named_Color = DarkTurquoise

  object DarkViolet extends Named_Color("DarkViolet", 148, 0, 211)
  val Dark_Violet: Named_Color = DarkViolet
  val dark_violet: Named_Color = DarkViolet
  val darkviolet:  Named_Color = DarkViolet

  object DeepPink extends Named_Color("DeepPink", 255, 20, 147)
  val Deep_Pink: Named_Color = DeepPink
  val deep_pink: Named_Color = DeepPink
  val deeppink:  Named_Color = DeepPink

  object DeepSkyBlue extends Named_Color("DeepSkyBlue", 0, 191, 255)
  val Deep_Sky_Blue: Named_Color = DeepSkyBlue
  val deep_sky_blue: Named_Color = DeepSkyBlue
  val deepskyblue:  Named_Color = DeepSkyBlue

  object DimGray extends Named_Color("DimGray", 105, 105, 105)
  val Dim_Gray: Named_Color = DimGray
  val dim_gray: Named_Color = DimGray
  val dimgray:  Named_Color = DimGray

  object DodgerBlue extends Named_Color("DodgerBlue", 30, 144, 255)
  val Dodger_Blue: Named_Color = DodgerBlue
  val dodger_blue: Named_Color = DodgerBlue
  val dodgerblue:  Named_Color = DodgerBlue

  object Firebrick extends Named_Color("Firebrick", 178, 34, 34)
  val firebrick: Named_Color = Firebrick

  object FloralWhite extends Named_Color("FloralWhite", 255, 250, 240)
  val Floral_White: Named_Color = FloralWhite
  val floral_white: Named_Color = FloralWhite
  val floralwhite:  Named_Color = FloralWhite

  object ForestGreen extends Named_Color("ForestGreen", 34, 139, 34)
  val Forest_Green: Named_Color = ForestGreen
  val forest_green: Named_Color = ForestGreen
  val forestgreen:  Named_Color = ForestGreen

  object Fuchsia extends Named_Color("Fuchsia", 255, 0, 255)
  val fuchsia: Named_Color = Fuchsia

  object Gainsboro extends Named_Color("Gainsboro", 220, 220, 220)
  val gainsboro: Named_Color = Gainsboro

  object GhostWhite extends Named_Color("GhostWhite", 248, 248, 255)
  val Ghost_White: Named_Color = GhostWhite
  val ghost_white: Named_Color = GhostWhite
  val ghostwhite:  Named_Color = GhostWhite

  object Gold extends Named_Color("Gold", 255, 215, 0)
  val gold: Named_Color = Gold

  object Goldenrod extends Named_Color("Goldenrod", 218, 165, 32)
  val goldenrod: Named_Color = Goldenrod

  object Gray extends Named_Color("Gray", 128, 128, 128)
  val gray: Named_Color = Gray

  object Green extends Named_Color("Green", 0, 128, 0)
  val green: Named_Color = Green

  object GreenYellow extends Named_Color("GreenYellow", 173, 255, 47)
  val Green_Yellow: Named_Color = GreenYellow
  val green_yellow: Named_Color = GreenYellow
  val greenyellow:  Named_Color = GreenYellow

  object Honeydew extends Named_Color("Honeydew", 240, 255, 240)
  val honeydew: Named_Color = Honeydew

  object HotPink extends Named_Color("HotPink", 255, 105, 180)
  val Hot_Pink: Named_Color = HotPink
  val hot_pink: Named_Color = HotPink
  val hotpink:  Named_Color = HotPink

  object IndianRed extends Named_Color("IndianRed", 205, 92, 92)
  val Indian_Red: Named_Color = IndianRed
  val indian_red: Named_Color = IndianRed
  val indianred:  Named_Color = IndianRed

  object Indigo extends Named_Color("Indigo", 75, 0, 130)
  val indigo: Named_Color = Indigo

  object Ivory extends Named_Color("Ivory", 255, 255, 240)
  val ivory: Named_Color = Ivory

  object Khaki extends Named_Color("Khaki", 240, 230, 140)
  val khaki: Named_Color = Khaki

  object Lavender extends Named_Color("Lavender", 230, 230, 250)
  val lavender: Named_Color = Lavender

  object LavenderBlush extends Named_Color("LavenderBlush", 255, 240, 245)
  val Lavender_Blush: Named_Color = LavenderBlush
  val lavender_blush: Named_Color = LavenderBlush
  val lavenderblush:  Named_Color = LavenderBlush

  object LawnGreen extends Named_Color("LawnGreen", 124, 252, 0)
  val Lawn_Green: Named_Color = LawnGreen
  val lawn_green: Named_Color = LawnGreen
  val lawngreen:  Named_Color = LawnGreen

  object LemonChiffon extends Named_Color("LemonChiffon", 255, 250, 205)
  val Lemon_Chiffon: Named_Color = LemonChiffon
  val lemon_chiffon: Named_Color = LemonChiffon
  val lemonchiffon:  Named_Color = LemonChiffon

  object LightBlue extends Named_Color("LightBlue", 173, 216, 230)
  val Light_Blue: Named_Color = LightBlue
  val light_blue: Named_Color = LightBlue
  val lightblue:  Named_Color = LightBlue

  object LightCoral extends Named_Color("LightCoral", 240, 128, 128)
  val Light_Coral: Named_Color = LightCoral
  val light_coral: Named_Color = LightCoral
  val lightcoral:  Named_Color = LightCoral

  object LightCyan extends Named_Color("LightCyan", 224, 255, 255)
  val Light_Cyan: Named_Color = LightCyan
  val light_cyan: Named_Color = LightCyan
  val lightcyan:  Named_Color = LightCyan

  object LightGoldenrodYellow extends Named_Color("LightGoldenrodYellow",
                                                  250, 250, 210)
  val Light_Goldenrod_Yellow: Named_Color = LightGoldenrodYellow
  val light_goldenrod_yellow: Named_Color = LightGoldenrodYellow
  val lightgoldenrodyellow:   Named_Color = LightGoldenrodYellow

  object LightGray extends Named_Color("LightGray", 211, 211, 211)
  val Light_Gray: Named_Color = LightGray
  val light_gray: Named_Color = LightGray
  val lightgray:  Named_Color = LightGray

  object LightGreen extends Named_Color("LightGreen", 144, 238, 144)
  val Light_Green: Named_Color = LightGreen
  val light_green: Named_Color = LightGreen
  val lightgreen:  Named_Color = LightGreen

  object LightPink extends Named_Color("LightPink", 255, 182, 193)
  val Light_Pink: Named_Color = LightPink
  val light_pink: Named_Color = LightPink
  val lightpink:  Named_Color = LightPink

  object LightSalmon extends Named_Color("LightSalmon", 255, 160, 122)
  val Light_Salmon: Named_Color = LightSalmon
  val light_salmon: Named_Color = LightSalmon
  val lightsalmon:  Named_Color = LightSalmon

  object LightSeaGreen extends Named_Color("LightSeaGreen", 32, 178, 170)
  val Light_Sea_Green: Named_Color = LightSeaGreen
  val light_sea_green: Named_Color = LightSeaGreen
  val lightseagreen:   Named_Color = LightSeaGreen

  object LightSkyBlue extends Named_Color("LightSkyBlue", 135, 206, 250)
  val Light_Sky_Blue: Named_Color = LightSkyBlue
  val light_sky_blue: Named_Color = LightSkyBlue
  val lightskyblue:   Named_Color = LightSkyBlue

  object LightSlateGray extends Named_Color("LightSlateGray", 119, 136, 153)
  val Light_Slate_Gray: Named_Color = LightSlateGray
  val light_slate_gray: Named_Color = LightSlateGray
  val lightslategray:   Named_Color = LightSlateGray

  object LightSteelBlue extends Named_Color("LightSteelBlue", 176, 196, 222)
  val Light_Steel_Blue: Named_Color = LightSteelBlue
  val light_steel_blue: Named_Color = LightSteelBlue
  val lightsteelblue:   Named_Color = LightSteelBlue

  object LightYellow extends Named_Color("LightYellow", 255, 255, 224)
  val Light_Yellow: Named_Color = LightYellow
  val light_yellow: Named_Color = LightYellow
  val lightyellow:  Named_Color = LightYellow

  object Lime extends Named_Color("Lime", 0, 255, 0)
  val lime: Named_Color = Lime

  object LimeGreen extends Named_Color("LimeGreen", 50, 205, 50)
  val Lime_Green: Named_Color = LimeGreen
  val lime_green: Named_Color = LimeGreen
  val limegreen:  Named_Color = LimeGreen

  object Linen extends Named_Color("Linen", 250, 240, 230)
  val linen: Named_Color = Linen

  object Magenta extends Named_Color("Magenta", 255, 0, 255)
  val magenta: Named_Color = Magenta

  object Maroon extends Named_Color("Maroon", 128, 0, 0)
  val maroon: Named_Color = Maroon

  object MediumAquamarine extends Named_Color("MediumAquamarine", 102, 205, 170)
  val Medium_Aquamarine: Named_Color = MediumAquamarine
  val medium_aquamarine: Named_Color = MediumAquamarine
  val mediumaquamarine:  Named_Color = MediumAquamarine

  object MediumBlue extends Named_Color("MediumBlue", 0, 0, 205)
  val Medium_Blue: Named_Color = MediumBlue
  val medium_blue: Named_Color = MediumBlue
  val mediumblue:  Named_Color = MediumBlue

  object MediumOrchid extends Named_Color("MediumOrchid", 186, 85, 211)
  val Medium_Orchid: Named_Color = MediumOrchid
  val medium_orchid: Named_Color = MediumOrchid
  val mediumorchid:  Named_Color = MediumOrchid

  object MediumPurple extends Named_Color("MediumPurple", 147, 112, 219)
  val Medium_Purple: Named_Color = MediumPurple
  val medium_purple: Named_Color = MediumPurple
  val mediumpurple:  Named_Color = MediumPurple

  object MediumSeaGreen extends Named_Color("MediumSeaGreen", 60, 179, 113)
  val Medium_Sea_Green: Named_Color = MediumSeaGreen
  val medium_sea_green: Named_Color = MediumSeaGreen
  val mediumseagreen:   Named_Color = MediumSeaGreen

  object MediumSlateBlue extends Named_Color("MediumSlateBlue", 123, 104, 238)
  val Medium_Slate_Blue: Named_Color = MediumSlateBlue
  val medium_slate_blue: Named_Color = MediumSlateBlue
  val mediumslateblue:   Named_Color = MediumSlateBlue

  object MediumSpringGreen extends Named_Color("MediumSpringGreen", 0, 250, 154)
  val Medium_Spring_Green: Named_Color = MediumSpringGreen
  val medium_spring_green: Named_Color = MediumSpringGreen
  val mediumspringgreen:   Named_Color = MediumSpringGreen

  object MediumTurquoise extends Named_Color("MediumTurquoise", 72, 209, 204)
  val Medium_Turquoise: Named_Color = MediumTurquoise
  val medium_turquoise: Named_Color = MediumTurquoise
  val mediumturquoise:  Named_Color = MediumTurquoise

  object MediumVioletRed extends Named_Color("MediumVioletRed", 199, 21, 133)
  val Medium_Violet_Red: Named_Color = MediumVioletRed
  val medium_violet_red: Named_Color = MediumVioletRed
  val mediumvioletred:   Named_Color = MediumVioletRed

  object MidnightBlue extends Named_Color("MidnightBlue", 25, 25, 112)
  val Midnight_Blue: Named_Color = MidnightBlue
  val midnight_blue: Named_Color = MidnightBlue
  val midnightblue:  Named_Color = MidnightBlue

  object MintCream extends Named_Color("MintCream", 245, 255, 250)
  val Mint_Cream: Named_Color = MintCream
  val mint_cream: Named_Color = MintCream
  val mintcream:  Named_Color = MintCream

  object MistyRose extends Named_Color("MistyRose", 255, 228, 225)
  val Misty_Rose: Named_Color = MistyRose
  val misty_rose: Named_Color = MistyRose
  val mistyrose:  Named_Color = MistyRose

  object Moccasin extends Named_Color("Moccasin", 255, 228, 181)
  val moccasin: Named_Color = Moccasin

  object NavajoWhite extends Named_Color("NavajoWhite", 255, 222, 173)
  val Navajo_White: Named_Color = NavajoWhite
  val navajo_white: Named_Color = NavajoWhite
  val navajowhite:  Named_Color = NavajoWhite

  object Navy extends Named_Color("Navy", 0, 0, 128)
  val navy: Named_Color = Navy

  object OldLace extends Named_Color("OldLace", 253, 245, 230)
  val Old_Lace: Named_Color = OldLace
  val old_lace: Named_Color = OldLace
  val oldlace:  Named_Color = OldLace

  object Olive extends Named_Color("Olive", 128, 128, 0)
  val olive: Named_Color = Olive

  object OliveDrab extends Named_Color("OliveDrab", 107, 142, 35)
  val Olive_Drab: Named_Color = OliveDrab
  val olive_drab: Named_Color = OliveDrab
  val olivedrab:  Named_Color = OliveDrab

  object Orange extends Named_Color("Orange", 255, 165, 0)
  val orange: Named_Color = Orange

  object OrangeRed extends Named_Color("OrangeRed", 255, 69, 0)
  val Orange_Red: Named_Color = OrangeRed
  val orange_red: Named_Color = OrangeRed
  val orangered:  Named_Color = OrangeRed

  object Orchid extends Named_Color("Orchid", 218, 112, 214)
  val orchid: Named_Color = Orchid

  object PaleGoldenrod extends Named_Color("PaleGoldenrod", 238, 232, 170)
  val Pale_Goldenrod: Named_Color = PaleGoldenrod
  val pale_goldenrod: Named_Color = PaleGoldenrod
  val palegoldenrod:  Named_Color = PaleGoldenrod

  object PaleGreen extends Named_Color("PaleGreen", 152, 251, 152)
  val Pale_Green: Named_Color = PaleGreen
  val pale_green: Named_Color = PaleGreen
  val palegreen:  Named_Color = PaleGreen

  object PaleTurquoise extends Named_Color("PaleTurquoise", 175, 238, 238)
  val Pale_Turquoise: Named_Color = PaleTurquoise
  val pale_turquoise: Named_Color = PaleTurquoise
  val paleturquoise:  Named_Color = PaleTurquoise

  object PaleVioletRed extends Named_Color("PaleVioletRed", 219, 112, 147)
  val Pale_Violet_Red: Named_Color = PaleVioletRed
  val pale_violet_red: Named_Color = PaleVioletRed
  val palevioletred:   Named_Color = PaleVioletRed

  object PapayaWhip extends Named_Color("PapayaWhip", 255, 239, 213)
  val Papaya_Whip: Named_Color = PapayaWhip
  val papaya_whip: Named_Color = PapayaWhip
  val papayawhip:  Named_Color = PapayaWhip

  object PeachPuff extends Named_Color("PeachPuff", 255, 218, 185)
  val Peach_Puff: Named_Color = PeachPuff
  val peach_puff: Named_Color = PeachPuff
  val peachpuff:  Named_Color = PeachPuff

  object Peru extends Named_Color("Peru", 205, 133, 63)
  val peru: Named_Color = Peru

  object Pink extends Named_Color("Pink", 255, 192, 203)
  val pink: Named_Color = Pink

  object Plum extends Named_Color("Plum", 221, 160, 221)
  val plum: Named_Color = Plum

  object PowderBlue extends Named_Color("PowderBlue", 176, 224, 230)
  val Powder_Blue: Named_Color = PowderBlue
  val powder_blue: Named_Color = PowderBlue
  val powderblue:  Named_Color = PowderBlue

  object Purple extends Named_Color("Purple", 128, 0, 128)
  val purple: Named_Color = Purple

  object Red extends Named_Color("Red", 255, 0, 0)
  val red: Named_Color = Red

  object RosyBrown extends Named_Color("RosyBrown", 188, 143, 143)
  val Rosy_Brown: Named_Color = RosyBrown
  val rosy_brown: Named_Color = RosyBrown
  val rosybrown:  Named_Color = RosyBrown

  object RoyalBlue extends Named_Color("RoyalBlue", 65, 105, 225)
  val Royal_Blue: Named_Color = RoyalBlue
  val royal_blue: Named_Color = RoyalBlue
  val royalblue:  Named_Color = RoyalBlue

  object SaddleBrown extends Named_Color("SaddleBrown", 139, 69, 19)
  val Saddle_Brown: Named_Color = SaddleBrown
  val saddle_brown: Named_Color = SaddleBrown
  val saddlebrown:  Named_Color = SaddleBrown

  object Salmon extends Named_Color("Salmon", 250, 128, 114)
  val salmon: Named_Color = Salmon

  object SandyBrown extends Named_Color("SandyBrown", 244, 164, 96)
  val Sandy_Brown: Named_Color = SandyBrown
  val sandy_brown: Named_Color = SandyBrown
  val sandybrown:  Named_Color = SandyBrown

  object SeaGreen extends Named_Color("SeaGreen", 46, 139, 87)
  val Sea_Green: Named_Color = SeaGreen
  val sea_green: Named_Color = SeaGreen
  val seagreen:  Named_Color = SeaGreen

  object SeaShell extends Named_Color("SeaShell", 255, 245, 238)
  val Sea_Shell: Named_Color = SeaShell
  val sea_shell: Named_Color = SeaShell
  val seashell:  Named_Color = SeaShell

  object Sienna extends Named_Color("Sienna", 160, 82, 45)
  val sienna: Named_Color = Sienna

  object Silver extends Named_Color("Silver", 192, 192, 192)
  val silver: Named_Color = Silver

  object SkyBlue extends Named_Color("SkyBlue", 135, 206, 235)
  val Sky_Blue: Named_Color = SkyBlue
  val sky_blue: Named_Color = SkyBlue
  val skyblue:  Named_Color = SkyBlue

  object SlateBlue extends Named_Color("SlateBlue", 106, 90, 205)
  val Slate_Blue: Named_Color = SlateBlue
  val slate_blue: Named_Color = SlateBlue
  val slateblue:  Named_Color = SlateBlue

  object SlateGray extends Named_Color("SlateGray", 112, 128, 144)
  val Slate_Gray: Named_Color = SlateGray
  val slate_gray: Named_Color = SlateGray
  val slategray:  Named_Color = SlateGray

  object Snow extends Named_Color("Snow", 255, 250, 250)
  val snow: Named_Color = Snow

  object SpringGreen extends Named_Color("SpringGreen", 0, 255, 127)
  val Spring_Green: Named_Color = SpringGreen
  val spring_green: Named_Color = SpringGreen
  val springgreen:  Named_Color = SpringGreen

  object SteelBlue extends Named_Color("SteelBlue", 70, 130, 180)
  val Steel_Blue: Named_Color = SteelBlue
  val steel_blue: Named_Color = SteelBlue
  val steelblue:  Named_Color = SteelBlue

  object Tan extends Named_Color("Tan", 210, 180, 140)
  val tan: Named_Color = Tan

  object Teal extends Named_Color("Teal", 0, 128, 128)
  val teal: Named_Color = Teal

  object Thistle extends Named_Color("Thistle", 216, 191, 216)
  val thistle: Named_Color = Thistle

  object Tomato extends Named_Color("Tomato", 255, 99, 71)
  val tomato: Named_Color = Tomato

  object Turquoise extends Named_Color("Turquoise", 64, 224, 208)
  val turquoise: Named_Color = Turquoise

  object Violet extends Named_Color("Violet", 238, 130, 238)
  val violet: Named_Color = Violet

  object Wheat extends Named_Color("Wheat", 245, 222, 179)
  val wheat: Named_Color = Wheat

  object White extends Named_Color("White", 255, 255, 255)
  val white: Named_Color = White

  object WhiteSmoke extends Named_Color("WhiteSmoke", 245, 245, 245)
  val White_Smoke: Named_Color = WhiteSmoke
  val white_smoke: Named_Color = WhiteSmoke
  val whitesmoke:  Named_Color = WhiteSmoke

  object Yellow extends Named_Color("Yellow", 255, 255, 0)
  val yellow: Named_Color = Yellow

  object YellowGreen extends Named_Color("YellowGreen", 154, 205, 50)
  val Yellow_Green: Named_Color = YellowGreen
  val yellow_green: Named_Color = YellowGreen
  val yellowgreen:  Named_Color = YellowGreen

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
          print qq[  object $1 extends Named_Color("$1", $2)\n];
          my ($name, $mod_name) = ($1, $1);
          if ($mod_name =~ s/([a-z])([A-Z])/${1}_$2/g) {
  print qq[  val $mod_name: Named_Color = $name\n  val \L$mod_name\E: Named_Color = $name\n  val \L$name\E:  Named_Color = $name\n\n]
          } else {
  print qq[  val \L$name\E: Named_Color = $name\n\n]
          }'

     3. add an extra space (manually) after ':', when name has > 2 words; e.g.:

  val lightsteelblue:  Named_Color = LightSteelBlue

        becomes:

  val lightsteelblue:   Named_Color = LightSteelBlue

  */
}

}

package object ink {

  val Default_Shape_Pen   = Pen(Named_Colors.Black, Null_Ink)
  val Default_Writing_Pen = Pen(Null_Ink, Named_Colors.Black)

  val Default_Pen         = Default_Shape_Pen
}
