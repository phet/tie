/*
   file: k_k_/graphics/tie/fmt/svg.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2013 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie

package fmt.svg {

import java.io.Writer

import scala.xml.{Utility => XMLUtil}

import k_k_.graphics.tie.effects.Filter
import k_k_.graphics.tie.ink._
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.path._
import k_k_.graphics.tie.shapes.text._


class ScriptType(val mime: String)

object ScriptType {
  val ecmascriptMime = "application/ecmascript"

  object ECMAscript extends ScriptType(ScriptType.ecmascriptMime)
  object Javascript extends ScriptType(ScriptType.ecmascriptMime)
}


sealed abstract class SvgRendererBase extends CharOutputRenderer {

  protected case class UnivAttrs(
      id: Option[String] = None,
      opacity: Option[Double] = None
    ) {

    def isDefined = id.isDefined || opacity.isDefined
  }


  protected def fmtPrologue(
      viewBox: VisibleArea, title: String, desc: Option[String]
    ): String

  protected def fmtEpilogue: String


  protected def scriptURIs:            List[(ScriptType, String)] = Nil
  protected def scriptContent:         Option[(ScriptType, String)] = None
  protected def scriptPostContentURIs: List[(ScriptType, String)] = Nil
}


trait CatenatedTransforms { self: SvgContentRenderer =>

  override protected def writeTransformedShape(
      defs: Defs,
      os: Writer,
      orderedTransforms: List[String],
      shape: Shape,
      attrs: UnivAttrs
    ): Defs = {
    val transformAttr = orderedTransforms.mkString("transform=\"", " ", "\"")
    writeGroup(defs, os, fmt(attrs) + transformAttr, shape)
  }
}

//!!!to truly reduce size, replace String.format %f (.000000 on whole nums)!!!
trait SmallFileSize extends CatenatedTransforms{ self: SvgContentRenderer =>
}


object SvgRenderer extends SvgRenderer

object SizeOptimizedSvgRenderer extends SvgRenderer with SmallFileSize

class SvgRenderer extends SvgContentRenderer {

  override protected def fmtPrologue(
      viewBox: VisibleArea, title: String, desc: Option[String]
    ): String = {
    val result = """<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" 
                     "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg viewBox="%f %f %f %f" version="1.1"
     xmlns="http://www.w3.org/2000/svg"
     xmlns:xlink="http://www.w3.org/1999/xlink"
     xmlns:tie="%s"
     tie:version="%s">

  <title>%s</title>

""".format(  // " (workaround emacs syntax coloring bug)
        viewBox.upperLeft.x, viewBox.upperLeft.y, viewBox.width, viewBox.height,
        Version.xmlnsUri, Version.toString, title
      )
    desc match {
      case Some(str) => result + "  <desc>%s</desc>\n".format(str)
      case None      => result
    }
  }

  override protected def fmtEpilogue: String = {
    "</svg>\n"
  }
}


object SvgElemRenderer extends SvgElemRenderer

class SvgElemRenderer extends SvgContentRenderer {

  override protected def fmtPrologue(
      viewBox: VisibleArea, title: String, desc: Option[String]
    ): String = {
    val result =
"""<svg viewBox="%f %f %f %f" xmlns="http://www.w3.org/2000/svg"
                           xmlns:xlink="http://www.w3.org/1999/xlink"
                           xmlns:tie="%s"
                           tie:version="%s">

  <title>%s</title>

""".format(  // " (workaround emacs syntax coloring bug)
        viewBox.upperLeft.x, viewBox.upperLeft.y, viewBox.width, viewBox.height,
        Version.xmlnsUri, Version.toString, title
      )
    desc match {
      case Some(str) => result + "  <desc>%s</desc>\n".format(str)
      case None      => result
    }
  }

  override protected def fmtEpilogue: String =
    "</svg>\n"
}


object SvgFragRenderer extends SvgFragRenderer

class SvgFragRenderer extends SvgContentRenderer {

  override protected def fmtPrologue(
      viewBox: VisibleArea, title: String, desc: Option[String]
    ): String =
    ""

  override protected def fmtEpilogue: String =
    ""
}


sealed abstract class SvgContentRenderer extends SvgRendererBase {

  override protected final def doRender(canvas: Canvas, os: Writer): Boolean = {
    val viewBox = canvas.visibleArea
    os.write(fmtPrologue(viewBox, canvas.title, canvas.desc))
    os.write(fmtViewBoxDebugStr(viewBox))

    scriptURIs.           foreach { p => writeScriptUri(os, p._1, p._2) }
    scriptContent.        foreach { p => writeScriptContent(os, p._1, p._2) }
    scriptPostContentURIs.foreach { p => writeScriptUri(os, p._1, p._2) }
    
    // NOTE: render in reverse shapes order, so last-added is drawn last
    val defs = (canvas.shapes foldRight new Defs(viewBox)){ (shape, defs) =>
      renderShape(defs, os, ensureInk(shape))
    }
    renderLicenseStamp(defs, os, viewBox)
    renderDefs(os, defs)
    os.write(fmtEpilogue)
    return true
  }


  val gradientIdPrefix = "tie-Gradient"
  val patternIdPrefix  = "tie-Pattern"
  val clipPathIdPrefix = "tie-Clip"
  val maskIdPrefix     = "tie-Mask"
  val filterIdPrefix   = "tie-Filter"
  val licenseStampId   = "tie-l-i-c-e-n-s-e-s-t-a-m-p"

  protected val textRulerFactory = Writing.textRulerFactory


  protected abstract class Def {
    val id: String
  }
  protected case class GradientDef(gradient: Gradient, id: String)
      extends Def
  protected case class PatternDef(pattern: Pattern, id: String)
      extends Def
  protected case class ClipPathDef(clipping: Shape, rule: ClipRule, id: String)
      extends Def
  protected case class RngDef(rng: scala.util.Random, id: String)
      extends Def
  protected case class MaskDef(mask: Shape, id: String)
      extends Def
  protected case class FilterDef(filter: Filter, id: String)
      extends Def


  protected sealed class Defs(
      val defs: List[Def],
      protected[SvgContentRenderer] val nextGradientNum: Int,
      protected[SvgContentRenderer] val nextPatternNum: Int,
      protected[SvgContentRenderer] val nextClipPathNum: Int,
      protected[SvgContentRenderer] val nextMaskNum: Int,
      protected[SvgContentRenderer] val nextFilterNum: Int
    ) {

    def this(area: VisibleArea) =
      this(Defs.init(area), 1, 1, 1, 1, 1)

    def reverse: Defs =
      createDefs(defs.reverse)      

    def calcUrl(g: Gradient): (String, Defs) = {
      findMatchingGradient(g) match {
        case Some(definition) =>
          ("#" + definition.id, this)
        case None =>
          val id = fmtGradientId(nextGradientNum)
          ("#" + id, createDefs(
              GradientDef(g, id) :: defs,
              nextGradientNum = nextGradientNum + 1
            ))
      }
    }

    def calcUrl(p: Pattern): (String, Defs) = {
      findMatchingPattern(p) match {
        case Some(definition) =>
          ("#" + definition.id, this)
        case None =>
          val id = fmtPatternId(nextPatternNum)
          ("#" + id, createDefs(
              PatternDef(p, id) :: defs,
              nextPatternNum = nextPatternNum + 1
            ))
      }
    }

    def calcClipPathUrl(shape: Shape, rule: ClipRule): (String, Defs) = {
      findMatchingClipPath(shape, rule) match {
        case Some(definition) =>
          ("#" + definition.id, this)
        case None =>
          val id = fmtClipPathId(nextClipPathNum)
          ("#" + id, createDefs(
              ClipPathDef(shape, rule, id) :: defs,
              nextClipPathNum = nextClipPathNum + 1
            ))
      }
    }

    def calcMaskUrl(shape: Shape): (String, Defs) = {
      findMatchingMask(shape) match {
        case Some(definition) =>
          ("#" + definition.id, this)
        case None =>
          val id = fmtMaskId(nextMaskNum)
          ("#" + id, createDefs(
              MaskDef(shape, id) :: defs,
              nextMaskNum = nextMaskNum + 1
            ))
      }
    }

    def calcUrl(filter: Filter): (String, Defs) = {
      findMatchingFilter(filter) match {
        case Some(definition) =>
          ("#" + definition.id, this)
        case None =>
          val id = fmtFilterId(nextFilterNum)
          ("#" + id, createDefs(
              FilterDef(filter, id) :: defs,
              nextFilterNum = nextFilterNum + 1
            ))
      }
    }


    protected def findIn(
        theDefs: List[Def]
      )(
        discriminator: PartialFunction[Def, Boolean]
      ): Option[Def] =
      theDefs.find( PartialFunction.cond(_)(discriminator) )

    // NOTE: use `==`, not `eq` in discriminators since looking for equivalent
    // vals, even when programmatically assembled identically multiple times

    protected def findMatchingGradient(g: Gradient): Option[Def] =
      findIn(defs){ case GradientDef(grad, _)      if grad == g  => true }

    protected def findMatchingPattern(p: Pattern): Option[Def] =
      findIn(defs){ case PatternDef(pat, _)        if pat == p   => true }

    protected def findMatchingClipPath(s: Shape, rle: ClipRule):
        Option[Def] =
      findIn(defs){ case ClipPathDef(shape, rule, _) if shape == s &&
                                                           rule == rle => true }

    protected def findMatchingRng(p: Option[Pattern]): Option[Def] =
      findIn(defs){ case RngDef(rng, _)            if None == p => true }

    protected def findMatchingMask(s: Shape): Option[Def] =
      findIn(defs){ case MaskDef(shape, _)         if shape == s => true }

    protected def findMatchingFilter(f: Filter): Option[Def] =
      findIn(defs){ case FilterDef(filter, _)      if filter == f => true }

    protected def createDefs(
        defs: List[Def],
        nextGradientNum: Int  = nextGradientNum,
        nextPatternNum: Int   = nextPatternNum,
        nextClipPathNum: Int = nextClipPathNum,
        nextMaskNum: Int      = nextMaskNum,
        nextFilterNum: Int    = nextFilterNum
      ): Defs =
      new Defs(
          defs, nextGradientNum, nextPatternNum, nextClipPathNum,
          nextMaskNum, nextFilterNum
        )


    protected val fmtGradientId = fmtId(gradientIdPrefix) _
    protected val fmtPatternId  = fmtId(patternIdPrefix)  _
    protected val fmtClipPathId = fmtId(clipPathIdPrefix) _
    protected val fmtMaskId     = fmtId(maskIdPrefix)     _
    protected val fmtFilterId   = fmtId(filterIdPrefix)   _

    def ? = orFalse(findMatchingRng(None).collect{ case r: RngDef => bool(r) })
    protected def fmtId(baseName: String)(num: Int) =
      "%s%02d".format(baseName, num)
  }

  protected final class DefineDefs(
      protected[SvgContentRenderer] val prevDefs: List[Def],
      override val defs: List[Def],
      nextGradientNum: Int,
      nextPatternNum: Int,
      nextClipPathNum: Int,
      nextMaskNum: Int,
      nextFilterNum: Int
    ) extends Defs(
        defs, nextGradientNum, nextPatternNum, nextClipPathNum,
        nextMaskNum, nextFilterNum
      ) {

    def this(parentDefs: Defs) =
      this(
          parentDefs match {
            case defDefs: DefineDefs =>
              parentDefs.defs ::: defDefs.prevDefs
            case _ =>
              parentDefs.defs
          },
          Nil,
          parentDefs.nextGradientNum,
          parentDefs.nextPatternNum,
          parentDefs.nextClipPathNum,
          parentDefs.nextMaskNum,
          parentDefs.nextFilterNum
        )

    override protected def findMatchingGradient(g: Gradient) =
      super.findMatchingGradient(g).orElse {
        findIn(defs){ case GradientDef(grad, _) if grad == g => true }
      }

    override protected def findMatchingPattern(p: Pattern) =
      super.findMatchingPattern(p).orElse {
        findIn(defs){ case PatternDef(pat, _) if pat == p => true }
      }

    override protected def findMatchingClipPath(s: Shape, rle: ClipRule) =
      super.findMatchingClipPath(s, rle).orElse {
        findIn(defs){
          case ClipPathDef(shape, rule, _) if shape == s && rule == rle => true
        }
      }

    override protected def findMatchingRng(p: Option[Pattern]): Option[Def] =
      super.findMatchingRng(p).orElse {
        findIn(defs){ case RngDef(rng, _) if None == p => true }
      }

    override protected def findMatchingMask(s: Shape) =
      super.findMatchingMask(s).orElse {
        findIn(defs){ case MaskDef(shape, _) if shape == s => true }
      }

    override protected def findMatchingFilter(f: Filter) =
      super.findMatchingFilter(f).orElse {
        findIn(defs){ case FilterDef(filter, _) if filter == f => true }
      }

    override protected def createDefs(
        defs: List[Def],
        nextGradientNum: Int  = nextGradientNum,
        nextPatternNum: Int   = nextPatternNum,
        nextClipPathNum: Int = nextClipPathNum,
        nextMaskNum: Int      = nextMaskNum,
        nextFilterNum: Int    = nextFilterNum): Defs =
      new DefineDefs(
          prevDefs, defs, nextGradientNum, nextPatternNum, nextClipPathNum,
          nextMaskNum, nextFilterNum
        )
  }

  protected object Defs {
    def init(i: Int) = RngDef(new scala.util.Random(i), "tie-r_") :: Nil
  }


  protected def renderDefs(os: Writer, origDefs: Defs) {
    def writeDefs(defs: Defs) {
      val defDefs: Defs = new DefineDefs(defs)
      val additionalDefs = (defDefs /: defs.defs){ renderDef(_, os, _) }
      if (!additionalDefs.defs.isEmpty) {
        writeDefs(additionalDefs.reverse) // repeat... so long as new defs
      }
    }

    os.write("\n  <defs>\n")
    writeDefs(origDefs.reverse)
    os.write("  </defs>\n")
  }

  protected def renderDef(defs: Defs, os: Writer, definition: Def): Defs = {
    definition match {
      case GradientDef(gradient, id) =>
        defineGradient(defs, os, id, gradient)
      case PatternDef(pattern, id) =>
        definePattern(defs, os, id, pattern)
      case ClipPathDef(clipping, clipRule, id) =>
        defineClipPath(defs, os, id, clipping, clipRule)
      case RngDef(rng, id) => defs
      case MaskDef(mask, id) =>
        defineMask(defs, os, id, mask)
      case FilterDef(filter, id) =>
        defineFilter(defs, os, id, filter)
    }
  }

  protected val defaultShapePen   = DefaultShapePen
  protected val defaultWritingPen = DefaultWritingPen

  protected def ensureInk(shape: Shape): Shape = {

    // returns Option of updated shape 'modified' internally; iff not modified
    // (i.e. Option is None), Boolean indicates whether orig. shape needs ink.
    //
    // Yes, this is much more complicated than merely rebuilding the whole tree,
    // but the expectation is that most shapes will have Ink, and that it would
    // be cheaper to merely traverse, while looking for Ink, than to reconstruct
    // every shape.  furthermore, the Boolean is used to add Ink, not at the
    // terminal, but as high above as would not effect another shape (i.e. below
    // a BinaryShapeOp).
    //!!!TRULY, THIS PREMISE SHOULD BE VALIDATED BY PROFILING!!!

    //!!!!!also, this heavily-recursive method is not tail-recursive; perhaps a
    // different algo is in order!!!!!!!
    def addMissingInk(shape: Shape, penTform: Option[PenTransform]):
        (Option[Shape], Boolean) =
      shape match {
        // neither invisible shape, nor an image requires Ink
        case InvisRectangle(_, _)
          |  Image(_, _, _)    => (None, false)

        case InkedShape(s, penTf: PenTransform) =>
          val newPenTform = penTform.map { _.compose(penTf) }.getOrElse(penTf)
          addMissingInk(s, Some(newPenTform)) match {
            case (Some(modShape), _) =>
              if (newPenTform.isReplacement)
                (Some(InkedShape(modShape, newPenTform)), false)
              else
                (Some(modShape), false)
            case (None, true) => // request from child to add ink higher above
                if (newPenTform.isReplacement)
                  (Some(InkedShape(s, newPenTform)), false)
                else
                  (Some(InkedShape(s, newPenTform(defaultShapePen))),false)
            case (None, false) =>
              // NOTE: defensive programming--should never happen!: either a Pen
              // beneath would be transformed by newPenTform and result in new
              // InkedShape returned, or flag would indicate that ink needed
              (Some(InkedShape(s, newPenTform)), false)
            }

        case InkedShape(s, simplePen) =>
          penTform.map( _(simplePen) ) match {
            case Some(transformedPen) =>
              (Some(InkedShape(s, transformedPen)), false)
            case None =>
              (None, false) // if no transformation, leave as is
          }

        case ushape @ UnaryShapeOp(s) =>
          addMissingInk(s, penTform) match {
            case (Some(modShape), _) =>
              (Some(ushape.child = modShape), false)
            case (None, needsInk) =>
              (None, needsInk)
          }

        case compShape @ CompositeShape(below, above) =>
          val finalShape =
            (addMissingInk(below, penTform),
             addMissingInk(above, penTform)) match {
              case ((Some(modBelowShape), _), (Some(modAboveShape), _)) =>
                Some(CompositeShape(modBelowShape, modAboveShape))
              case ((Some(modBelowShape), _), (None, true)) =>
                Some(CompositeShape(
                    modBelowShape, InkedShape(above, defaultShapePen)
                  ))
              case ((Some(modBelowShape), _), (None, false)) =>
                Some(compShape.left = modBelowShape)
              case ((None, true), (Some(modAboveShape), _)) =>
                Some(CompositeShape(
                    InkedShape(below, defaultShapePen), modAboveShape
                  ))
              case ((None, false), (Some(modAboveShape), _)) =>
                Some(compShape.right = modAboveShape)
              case ((None, true), (None, true)) =>
                Some(InkedShape(compShape, defaultShapePen))
              case ((None, true), (None, false)) =>
                Some(compShape.left = InkedShape(below, defaultShapePen))
              case ((None, false), (None, true)) =>
                Some(compShape.right = InkedShape(above, defaultShapePen))
              case ((None, false), (None, false)) =>
                None
            }
          (finalShape, false)

        // NOTE: no need to ensure the [right] `clipping` (shape) has ink
        //     : add no ink to the [right] `mask` (shape), as it might interfere
        case binShape @ BinaryShapeOp(left, right) =>
          addMissingInk(left, penTform) match {
            case (Some(modShape), _) =>
              (Some(binShape.replaceLeft(modShape)), false)
            case (None, needsInk) =>
              (None, needsInk)
          }

        // NOTE: unlike 'true' shapes, ink Writing now, since using special kind
        case writing @ Writing(_) =>
          (Some(InkedShape(writing, defaultWritingPen)), false)

        case NullaryShapeOp() => (None, true)
      }

    addMissingInk(shape, None) match {
      case (Some(modShape), _) => modShape
      case (None, true)        => InkedShape(shape, defaultShapePen)
      case (None, false)       => shape
    }
  }

  protected final def renderShape(
      defs: Defs, os: Writer, shape: Shape, attrs: UnivAttrs = UnivAttrs()
    ): Defs = {
    def writeShape(s: Shape, transforms: List[String]): Defs =
      s match {
        case TranslatedShape(shape, xMove, yMove) =>
          val transform = "translate(%f, %f)".format(xMove, yMove)
          writeShape(shape, transform :: transforms)
        case ScaledShape(shape, xScaling, yScaling) =>
          val transform = "scale(%f, %f)".format(xScaling, yScaling)
          writeShape(shape, transform :: transforms)
        case RotatedShape(shape, degrees, xPivot, yPivot) =>
          val transform = "rotate(%f, %f, %f)".format(degrees, xPivot, yPivot)
          writeShape(shape, transform :: transforms)
        case ReflectedShape(shape, degrees, xPivot, yPivot) =>
          val transform = fmtReflectionTransform(degrees, xPivot, yPivot)
          writeShape(shape, transform :: transforms)
        case SkewedHorizShape(shape, degrees) =>
          val transform = "skewX(%f)".format(degrees)
          writeShape(shape, transform :: transforms)
        case SkewedVertShape(shape, degrees) =>
          val transform = "skewY(%f)".format(degrees)
          writeShape(shape, transform :: transforms)
        case transformedShape if !transforms.isEmpty =>
          // NOTE: cons`ing to transforms on pre-order traversal reverses them
          writeTransformedShape(
              defs, os, transforms.reverse, transformedShape, attrs
            )
        case nonTransformShape =>
          writeNonTransformedShape(nonTransformShape)
      }

    def writeNonTransformedShape(s: Shape): Defs =
      s match {
        case compositeShape @ CompositeShape(below, above) =>
          if (attrs.isDefined) // create group for attrs around entire composite
            writeGroup(defs, os, fmt(attrs).trim, compositeShape)
          else {
            val newDefs = renderShape(defs, os, below)
            renderShape(newDefs, os, above)
          }

        case ClippedShape(clipped, clipping, clipRule) =>
          doClippedShape(defs, os, clipped, clipping, clipRule, attrs)
        case InkedShape(shape, pen) =>
          doInk(defs, os, shape, pen, attrs)

        case nonOpaqueShape @ NonOpaqueShape(shape, opacity) =>
          if (clampOpacity(opacity) == 1.0) // the default; skip writing value
            renderShape(defs, os, shape, attrs)
          else if (attrs.opacity.isDefined) // create group to write exist opcty
            writeGroup(defs, os, fmt(attrs).trim, nonOpaqueShape)
          else // carry `opacity` until next draw_* invocation
            renderShape(defs, os, shape, attrs.copy(opacity = Some(opacity)))

        case MaskedShape(masked, mask) =>
          doMaskedShape(defs, os, masked, mask, attrs)
        case FilteredShape(shape, filter) =>
          doFilteredShape(defs, os, shape, filter, attrs)

        case attribShape @ AttributedShape(shape, attribution) =>
          attribution match {
            case IdAttribution(id) if XMLUtil.isName(id) =>
              if (attrs.id.isDefined) // create group to write before new `id`
                writeGroup(defs, os, fmt(attrs).trim, attribShape)
              else // carry `id` until next draw_* invocation
                renderShape(defs, os, shape, attrs.copy(id = Some(id)))
            case IdAttribution(_) => // ignore mal-formed ids
              renderShape(defs, os, shape, attrs) // pass existing attrs if any
            case LinkAttribution(uri, target) =>
              doLink(defs, os, uri, target, shape, attrs)
          }

        case FreeForm(path) =>
          drawPath(defs, os, path, attrs)
        case Writing(text) =>
          drawText(defs, os, text, attrs)
        case image @ Image(_, width, height) =>
          drawImage(defs, os, image.mappedFpath, width, height, attrs)
  
        case InvisRectangle(width, height) =>
          // NOTE: no `attrs` for InvisRectangle--it's ID'ably invisible too!
          hideInvisRectangle(defs, os, width, height)
  
        case trueShape: TrueShape =>
          drawPath(defs, os, trueShape.asPath, attrs)
        case _ => // peferred to using @unchecked on match!
          // WARNING: mutual recursion via unconstrained (final) match holds
          // potential for infinite loop
          writeShape(shape, Nil)
      }
    writeShape(shape, Nil)
  }

  protected def defineGradient(
      defs: Defs, os: Writer, id: String, gradient: Gradient
    ): Defs = {

    def writeGradientDef(gradient: Gradient, transforms: List[String]): Defs = {

      def calcSpreadName(colorSpread: ColorSpread): String =
        colorSpread.toString.takeWhile(_ != '_').toLowerCase

      def calcInterpName(colorInterp: ColorInterp): String =
        colorInterp match {
          // NOTE: since class name begins w/ lc, extractor'd be mistaken as val
          case ColorInterp.sRGB      => "sRGB"
          case ColorInterp.LinearRGB => "linearRGB"
        }

      gradient match {
        case TranslatedGradient(grad, xMove, yMove) =>
          val transform = "translate(%f, %f)".format(xMove, yMove)
          writeGradientDef(grad, transform :: transforms)
        case ScaledGradient(grad, xScaling, yScaling) =>
          val transform = "scale(%f, %f)".format(xScaling, yScaling)
          writeGradientDef(grad, transform :: transforms)
        case RotatedGradient(grad, degrees, xPivot, yPivot) =>
          val transform = "rotate(%f, %f, %f)".format(degrees, xPivot, yPivot)
          writeGradientDef(grad, transform :: transforms)
        case ReflectedGradient(grad, degrees, xPivot, yPivot) =>
          val transform = fmtReflectionTransform(degrees, xPivot, yPivot)
          writeGradientDef(grad, transform :: transforms)
        case SkewedHorizGradient(grad, degrees) =>
          val transform = "skewX(%f)".format(degrees)
          writeGradientDef(grad, transform :: transforms)
        case SkewedVertGradient(grad, degrees) =>
          val transform = "skewY(%f)".format(degrees)
          writeGradientDef(grad, transform :: transforms)
        case LinearGradient(colorStops, colorSpread, colorInterp) =>
          // NOTE: cons`ing to transforms on pre-order traversal reverses them
          val transformAttr =
            if (transforms.isEmpty) ""
            else transforms.reverse.mkString(" gradientTransform=\"", " ", "\"")
          os.write(
              "    <linearGradient id=\"" + id + "\"" + transformAttr +
              " spreadMethod=\"" + calcSpreadName(colorSpread) + "\"" +
              " color-interpolation=\"" + calcInterpName(colorInterp) +
              "\">\n"
            )
          val newDefs = (colorStops foldLeft defs){ renderColorStop(_, os, _) }
          os.write("    </linearGradient>\n")
          newDefs
        case RadialGradient(colorStops, colorSpread, colorInterp) =>
          // NOTE: cons`ing to transforms on pre-order traversal reverses them
          val transformAttr =
            if (transforms.isEmpty) ""
            else transforms.reverse.mkString(" gradientTransform=\"", " ", "\"")
          os.write(
              "    <radialGradient id=\"" + id + "\"" + transformAttr +
              " spreadMethod=\"" + calcSpreadName(colorSpread) + "\"" +
              " color-interpolation=\"" + calcInterpName(colorInterp) +
              "\">\n"
            )
          val newDefs = (colorStops foldLeft defs){ renderColorStop(_, os, _) }
          os.write("    </radialGradient>\n")
          newDefs
      }
    }
    writeGradientDef(gradient, Nil)
  }

  protected def renderColorStop(defs: Defs, os: Writer, cs: ColorStop):
      Defs = {
    os.write(
        "      <stop offset=\"%f%%\" stop-color=\"%s\" stop-opacity=\"%f\"/>\n".
          format(
              cs.stopOffsetPct, fmtColorName(cs.color), clampOpacity(cs.opacity)
            )
      )
    defs
  }

  protected def definePattern(
      defs: Defs, os: Writer, id: String, pattern: Pattern
    ): Defs = {

    def writePatternDef(pattern: Pattern, transforms: List[String]): Defs = {
      pattern match {
        case TranslatedPattern(pat, xMove, yMove) =>
          val transform = "translate(%f, %f)".format(xMove, yMove)
          writePatternDef(pat, transform :: transforms)
        case ScaledPattern(pat, xScaling, yScaling) =>
          val transform = "scale(%f, %f)".format(xScaling, yScaling)
          writePatternDef(pat, transform :: transforms)
        case RotatedPattern(pat, degrees, xPivot, yPivot) =>
          val transform = "rotate(%f, %f, %f)".format(degrees, xPivot, yPivot)
          writePatternDef(pat, transform :: transforms)
        case ReflectedPattern(pat, degrees, xPivot, yPivot) =>
          val transform = fmtReflectionTransform(degrees, xPivot, yPivot)
          writePatternDef(pat, transform :: transforms)
        case SkewedHorizPattern(pat, degrees) =>
          val transform = "skewX(%f)".format(degrees)
          writePatternDef(pat, transform :: transforms)
        case SkewedVertPattern(pat, degrees) =>
          val transform = "skewY(%f)".format(degrees)
          writePatternDef(pat, transform :: transforms)
        case ShapePattern(shape, width, height) =>
          // NOTE: cons`ing to transforms on pre-order traversal reverses them
          val transformAttr =
            if (transforms.isEmpty) ""
            else transforms.reverse.mkString(" patternTransform=\"", " ", "\"")
          os.write(
              "    <pattern id=\"" + id + "\"" + transformAttr +
              " width=\"%f\" height=\"%f\"".format(width, height) +
              " patternUnits=\"userSpaceOnUse\">\n"
            )
          val newDefs = renderShape(defs, os, shape)
          os.write("    </pattern>\n")
          newDefs
      }
    }
    writePatternDef(pattern, Nil)
  }

  protected def defineClipPath(
      defs: Defs, os: Writer, id: String, clipping: Shape, clipRule: ClipRule
    ): Defs = {
    val clipRuleAttr = "clip-rule=\"%s\"".format( clipRule match {
      case ClipRule.NonZero => "nonzero"
      case ClipRule.EvenOdd => "evenodd"
      case ClipRule.Inherit => "inherit"
    } )
    os.write(
        "    <clipPath id=\"" + id + "\" clipPathUnits=\"userSpaceOnUse\">\n"
      )
    val newDefs = writeGroup(defs, os, clipRuleAttr, clipping)
    os.write("    </clipPath>\n")
    newDefs
  }

  protected def defineMask(defs: Defs, os: Writer, id: String, mask: Shape):
      Defs = {
    os.write("    <mask id=\"" + id + "\" maskUnits=\"userSpaceOnUse\">\n")
    val newDefs = renderShape(defs, os, mask)
    os.write("    </mask>\n")
    newDefs
  }

  protected def defineFilter(
      defs: Defs, os: Writer, id: String, filter: Filter
    ): Defs = {
    os.write("    <filter id=\"" + id + "\" filterUnits=\"userSpaceOnUse\">\n")
    val newDefs = renderFilter(defs, os, filter)
    os.write("    </filter>\n")
    newDefs
  }

  // NOTE: essentially a noop until next release, when Filter class hierarchy
  // elaborated (completed); may be overridden in this release by the Impatient
  protected def renderFilter(defs: Defs, os: Writer, filter: Filter): Defs = {
    defs
  }

  protected def hideInvisRectangle(
      defs: Defs, os: Writer, width: Double, height: Double
    ): Defs = {
    os.write("  <!-- 'invisible' rectangle (%f x %f):\n".format(width, height))
    val unusedDefs =
        drawPath(defs, os, Rectangle(width, height).asPath, UnivAttrs())
    os.write("  -->\n")
    defs
  }

  protected final def drawText(
      defs: Defs, os: Writer, text: Text, attrs: UnivAttrs
    ): Defs = {

    def writeTextBlock(block: TextBlock) {
      import WritingMode._
      import TextAlign.{Start, Middle, End}

      val Rectangular(blockW, blockH) = block.textBoundingBox(textRulerFactory)
      val upperLeft: Point = (0 - blockW/2, 0 - blockH/2)
      val upperRight = upperLeft -+ (blockW, 0)


      val calcLineStart = block.mode match {
        case LeftRight_TopBottom =>
          (align: TextAlign, lineW: Double, lineH: Double, prevPt: Point) =>
            val xOffset = align match {
              case Start  => 0 // parent `TextBlock` x-aligned for LR
              case Middle => (blockW - lineW)/2
              case End    =>  blockW - lineW
            }
            (upperLeft.x + xOffset, prevPt.y + lineH)
        case RightLeft_TopBottom =>
          (align: TextAlign, lineW: Double, lineH: Double, prevPt: Point) =>
            val xOffset = align match {
              case Start  => -blockW
              case Middle => -blockW + (blockW - lineW)/2
              case End    => -lineW
            }
            (upperLeft.x + xOffset, prevPt.y + lineH)
        case TopBottom_RightLeft =>
          (align: TextAlign, lineW: Double, lineH: Double, prevPt: Point) =>
            val yOffset = align match {
              case Start  => 0 // parent `TextBlock` y-aligned for TB
              case Middle => (blockH - lineH)/2
              case End    =>  blockH - lineH
            }
            (prevPt.x - lineW, upperLeft.y + yOffset)
      }


      def calcLineW_offsets(
          prev: (TextLine, Point, Dims), curr: (TextLine, Dims)
        ): (TextLine, Point, Dims) = {
        val (_, prevPt, _) = prev
        val (currLine, currLineBb @ Rectangular(w, h)) = curr
        val lineStart = calcLineStart(currLine.align, w, h, prevPt)
        (currLine, lineStart, currLineBb)
      }

      // NOTE: extra complication due to fact that <tspan>.{x,y} pos is baseline
      // (for Horizontal orientation--ceter-line, for Vertical orientation)
      // not text extent: descender chars (e.g. {p,g,j,...} still hang below
      // baseline for unbalanced appearance.  for simplicity, use simple, font-
      // independent ratio of 'ascender reach' (e.g. {l,k,(,...}) to 'descender
      // drop' of:
      //   3/4 (above baseline) :: 1/4 (below baseline)
      // then, to compensate for misaligned appearance when initial char is not
      // descender (no latin-1 caps are), split the difference to 1/8 down
      def balanceAscentDescent(lineW_offsetsW_bb: (TextLine, Point, Dims)):
          (TextLine, Point) = {
        val (line, offsetPoint, Rectangular(bbW, bbH)) = lineW_offsetsW_bb
        (line, offsetPoint -+ (0, -(bbH/8)))
      }

      // NOTE: analogous complication due to fact that <tspan>.{x,y} pos is
      // center-line (for Vertical orientation); simpler solution, here: shift
      // all chars left by half line width
      def shiftCenterLine(lineW_offsetsW_bb: (TextLine, Point, Dims)):
          (TextLine, Point) = {
        val (line, offsetPoint, Rectangular(bbW, bbH)) = lineW_offsetsW_bb
        (line, offsetPoint -+ (-(bbW/2), 0))
      }


      val lineW_bbs = block.content map { line =>
        (line, line.textBoundingBox(textRulerFactory, block.mode))
      }
      // init. scan with a dummy line and startingPt, later dropped w/ `tail`
      val (startingPt, startPtCompensation) = block.mode.orientation match {
        case Orientation.Horizontal => (upperLeft,  balanceAscentDescent _ )
        case Orientation.Vertical   => (upperRight, shiftCenterLine      _ )
      }
      val dummyInit =
          (TextLine("", Font.Default), startingPt, OriginDims(1, 1): Dims)
      val lineW_offsets =
          lineW_bbs.scanLeft(dummyInit) { calcLineW_offsets(_, _) }.tail.map {
            startPtCompensation(_)
          }

      val layoutAttrs = calcBlockLayoutAttrs(
          TextAlign.Start, block.mode, Some(upperLeft.x), Some(upperLeft.y)
        )
      os.write("    <text " + fmt(attrs) + layoutAttrs.mkString(" ") + ">\n")
      for {
        (line, Point(xOffset, yOffset)) <- lineW_offsets
      } writeTextLine(line, xOffset, yOffset)
      os.write("    </text>\n")
    }

    def writeTextLine(line: TextLine, xOffset: Double, yOffset: Double) {
      val layoutAttrs =
              calcLineLayoutAttrs(line.dir, Some(xOffset), Some(yOffset))
        os.write("      <tspan " + layoutAttrs.mkString(" ") + ">\n")
        line.content.foreach { writeTextSpan(_, Nil) }
        os.write("      </tspan>\n")
    }

    def writeTextSpan(span: TextSpan, moreAttrs: List[String]) {
      val shallPreserveSpaces = !span.compressSpaces

      def writeBasicSpan(span: BasicTextSpan) {
        val presentationAttrs =
              calcPresentationAttrs(span.font, span.decor, shallPreserveSpaces)
        val attrsStr = (moreAttrs ::: presentationAttrs).mkString(" ")
        val (spanStart, spanContent, spanEnd) =
            ("<tspan " + attrsStr + ">", escapeXmlText(span.text), "</tspan>")
        if (shallPreserveSpaces) // take care to add no extra, unrequested spaces
          os.write(spanStart + spanContent + spanEnd)
        else // format to be more legible, perhaps for debugging
          os.write(  "        " + spanStart + "\n" +
                      spanContent           +
                   "\n        " + spanEnd   + "\n")
      }

      span match {
        case basicSpan: BasicTextSpan =>
          writeBasicSpan(basicSpan)
        case BaselineShiftedTextSpan(childSpan, shift) =>
          val attrsStr = (moreAttrs ::: calcShiftAttrs(shift)).mkString(" ")
          val (spanStart, spanEnd) = ("<tspan " + attrsStr + ">", "</tspan>")
          if (shallPreserveSpaces) { // take care to add no extra spaces
            os.write(spanStart)
            writeTextSpan(childSpan, moreAttrs) // pass attrs to child
            os.write(spanEnd)
          } else { // format to be more legible, perhaps for debugging
            os.write("        " + spanStart + "\n")
            writeTextSpan(childSpan, moreAttrs) // pass attrs for child also
            os.write("        " + spanEnd + "\n")
          }
      }
    }

    val textBlock = text match {
      case span:  TextSpan  => TextBlock(TextLine(span))
      case line:  TextLine  => TextBlock(line)
      case block: TextBlock => block
    }
    writeTextBlock(textBlock)
    defs
  }

  private def bool(r: RngDef) =
    r.rng.nextBoolean

  protected def escapeXmlText(s: String): String = {
    val xmlReserved = "(?x)  (   <   |   &   |   >   ) ".r
    xmlReserved.replaceAllIn(s, reMatch => reMatch.group(1) match {
      case "<" => "&lt;"
      case "&" => "&amp;"
      case ">" => "&gt;"
      case _   => "" // defensive catch-all
    })
  }

  protected def calcPresentationAttrs(
      font: Font, decor: TextDecoration, shallPreserveSpaces: Boolean
    ): List[String] = {

    val familyAttr = "font-family=\"%s\"".format(font.family)
    val sizeAttr = font.size match {
      case FontSize.Std(size) => "font-size=\"%s\"".format(size)
    }
    val styleAttr = "font-style=\"%s\"".format(font.style match {
      case FontStyle.Plain   => "normal"
      case FontStyle.Italic  => "italic"
      case FontStyle.Oblique => "oblique"
    })
    val weightAttr = "font-weight=\"%d\"".format(font.weight.css2Equiv)
    val decorationAttr = "text-decoration=\"%s\"".format(decor match {
      case TextDecoration.Undecorated => "none"
      case TextDecoration.Underline   => "underline"
      case TextDecoration.Overline    => "overline"
      case TextDecoration.Strike      => "line-through"
      case TextDecoration.Blinking    => "blink"
    })
    val spacesAttr = if (shallPreserveSpaces) "xml:space=\"preserve\"" else ""

    List(spacesAttr, familyAttr, sizeAttr, styleAttr, weightAttr,
         decorationAttr) filter (_ != "")
  }

  protected def calcShiftAttrs(shift: BaselineShift): List[String] = {
    val shiftAttr = "baseline-shift=\"%s\"".format(shift match {
      case BaselineShift.Above => "super"
      case BaselineShift.Below => "sub"
    })

    List(shiftAttr)
  }

  protected def calcBlockLayoutAttrs(
      align: TextAlign, mode: WritingMode,
      xPos: Option[Double] = None, yPos: Option[Double] = None
    ): List[String] = {
    val anchorAttr = "text-anchor=\"%s\"".format(align match {
      case TextAlign.Start  => "start"
      case TextAlign.Middle => "middle"
      case TextAlign.End    => "end"
    })
    val writingModeAttr = "writing-mode=\"%s\"".format(mode match {
      case WritingMode.LeftRight_TopBottom => "lr-tb"
      case WritingMode.RightLeft_TopBottom => "rl-tb"
      case WritingMode.TopBottom_RightLeft => "tb-rl"
    })

    val xAttr = xPos.map("x=\"%f\"".format(_)).getOrElse("")
    val yAttr = yPos.map("y=\"%f\"".format(_)).getOrElse("")
    List(anchorAttr, writingModeAttr, xAttr, yAttr) filter (_ != "")
  }

  protected def calcLineLayoutAttrs(
      dir: TextDirection,
      xPos: Option[Double] = None, yPos: Option[Double] = None
    ): List[String] = {
    val directionAttr = "direction=\"%s\"".format(dir match {
      case TextDirection.LeftToRight => "ltr"
      case TextDirection.RightToLeft => "rtl"
    })
    val xAttr = xPos.map("x=\"%f\"".format(_)).getOrElse("")
    val yAttr = yPos.map("y=\"%f\"".format(_)).getOrElse("")
    List(xAttr, yAttr, directionAttr) filter (_ != "")
  }

  protected final def drawImage(
      defs: Defs, os: Writer, fpath: String, width: Double, height: Double,
      attrs: UnivAttrs
    ): Defs = {
    val upperLeft: Point = (-width / 2, -height / 2)
    os.write(
        "  <image %sxlink:href=\"%s\" x=\"%f\" y=\"%f\" width=\"%f\" height=\"%f\"/>\n".format(
          fmt(attrs), XMLUtil.escape(fpath),
          upperLeft._1, upperLeft._2,
          width, height
      ))
    defs
  }

  protected final def drawPath(
      defs: Defs, os: Writer, path: Path, attrs: UnivAttrs
    ): Defs = {
    def encodeArcChoice(arcKind: ArcChoice): (Int, Int) = arcKind match {
      case ArcChoice.SmallCW  => (0, 1)
      case ArcChoice.LargeCW  => (1, 1)
      case ArcChoice.SmallCCW => (0, 0)
      case ArcChoice.LargeCCW => (1, 0)
    }

    val pathCmds = path.getCmdsBackwards
    val pathCmdStrs = (pathCmds foldLeft (Nil: List[String])) {
      (cmdStrs, pathCmd) =>
        val pathCmdStr = pathCmd match {
          case MoveAbs(x, y) => "M" + x + "," + y
          case MoveRel(x, y) => "m" + x + "," + y
          case LineAbs(x, y) => "L" + x + "," + y
          case LineRel(x, y) => "l" + x + "," + y
          case HorizontalAbs(x) => "H" + x
          case HorizontalRel(x) => "h" + x
          case VerticalAbs(y) => "V" + y
          case VerticalRel(y) => "v" + y
          case EllipticalArcAbs(
              radWidth, radHeight, xRotateDegrees, arcKind, x, y
            ) =>
            val (largeArcFlag, posSweepFlag) = encodeArcChoice(arcKind)
            "A" + radWidth + "," + radHeight + " " + xRotateDegrees + " " +
                largeArcFlag + " " + posSweepFlag + " " + x + "," + y
          case EllipticalArcRel(
              radWidth, radHeight, xRotateDegrees, arcKind, x, y
            ) =>
            val (largeArcFlag, posSweepFlag) = encodeArcChoice(arcKind)
            "a" + radWidth + "," + radHeight + " " + xRotateDegrees + " " +
                largeArcFlag + " " + posSweepFlag + " " + x + "," + y
          case QuadBezierAbs(xCtl1, yCtl1, x, y) =>
            "Q" + xCtl1 + "," + yCtl1 + " " + x + "," + y
          case QuadBezierRel(xCtl1, yCtl1, x, y) =>
            "q" + xCtl1 + "," + yCtl1 + " " + x + "," + y
          case TangentQuadBezierAbs(x, y) => "T" + x + "," + y
          case TangentQuadBezierRel(x, y) => "t" + x + "," + y
          case CubicBezierAbs(xCtl1, yCtl1, xCtl2, yCtl2, x, y) =>
            "C" + xCtl1 +","+ yCtl1 +" "+ xCtl2 +","+ yCtl2 +" "+ x +","+ y
          case CubicBezierRel(xCtl1, yCtl1, xCtl2, yCtl2, x, y) =>
            "c" + xCtl1 +","+ yCtl1 +" "+ xCtl2 +","+ yCtl2 +" "+ x +","+ y
          case TangentCubicBezierAbs(xCtl1, yCtl1, x, y) =>
            "S" + xCtl1 + "," + yCtl1 + " " + x + "," + y
          case TangentCubicBezierRel(xCtl1, yCtl1, x, y) =>
            "s" + xCtl1 + "," + yCtl1 + " " + x + "," + y
          case Close => if (defs?) "z" else "Z"
        }
        pathCmdStr :: cmdStrs
    }
    os.write(pathCmdStrs.mkString(
        "  <path " + fmt(attrs) + "d=\"", " ", "\"/>\n"
      ))
    defs
  }

  protected def doClippedShape(
      defs: Defs, os: Writer,
      clipped: Shape, clipping: Shape, clipRule: ClipRule,
      attrs: UnivAttrs
    ): Defs = {
    val (url, newDefs) = defs.calcClipPathUrl(clipping, clipRule)
    writeGroup(
        newDefs, os, "%sclip-path=\"url(%s)\"".format(fmt(attrs), url), clipped
      )
  }

  protected def doInk(
      defs: Defs, os: Writer, shape: Shape, pen: Pen, attrs: UnivAttrs
    ): Defs = {
    def fmtInkAttrs(ink: Ink, defs: Defs): (String, Option[Double], Defs) =
      ink match {
        // NOTE: must precede `c: Color` since unapply matches NonOpaqueColor
        case NonOpaqueInk(ink, opacity) =>
          val (strokeAttr, None, newDefs) = fmtInkAttrs(ink, defs)
          (strokeAttr, Some(opacity), newDefs)
        case NullInk    => ("none", None, defs)
        case c: Color    => (fmtColorName(c), None, defs)
        case g: Gradient =>
          val (url, newDefs) = defs.calcUrl(g)
          ("url(" + url + ")", None, newDefs)
        case p: Pattern  =>
          val (url, newDefs) = defs.calcUrl(p)
          ("url(" + url + ")", None, newDefs)
      }

    // SVG default: stroke,fill="black"; tie default: stroke="black",fill="none"
    val (stroke, strokeOpacity, defs1) = pen.stroke.map {
      fmtInkAttrs(_, defs)
    }.getOrElse( ("", None, defs) )
    val (fill, fillOpacity, defs2) = pen.fill.map {
      fmtInkAttrs(_, defs1)
    }.getOrElse( ("none", None, defs) )

    val strokeAttr = if (stroke == "") "" else "stroke=\"%s\"".format(stroke)
    val fillAttr   =                           "fill=\"%s\"".  format(fill)
    val strokeOpacityAttr = strokeOpacity.map( clampOpacity ).map {
      "stroke-opacity=\"%f\"".format(_)
    }.getOrElse("")
    val fillOpacityAttr = fillOpacity.map( clampOpacity ).map {
      "fill-opacity=\"%f\"".format(_)
    }.getOrElse("")

    val strokeWidthAttr = pen.width match {
      case Some(len) => "stroke-width=\"%f\"".format(len)
      case None      => ""
    }

    val strokeDashPatternAttr = pen.dashPattern match {
      case None 
        |  Some(Nil) => ""
      case Some(len :: Nil) =>
        "stroke-dasharray=\"%f %f\"".format(len, len)
      case Some(patternLens) =>
        "stroke-dasharray=\"%s\"".format(patternLens.mkString(" "))
    }
    val strokeDashOffsetAttr = pen.dashOffset match {
      case Some(offset) => "stroke-dashoffset=\"%f\"".format(offset)
      case None         => ""
    }

    val strokeLinecapAttr = pen.ends.map {
      case StrokeEnds.Exact   => "butt"
      case StrokeEnds.Round   => "round"
      case StrokeEnds.Extend  => "square"
      case StrokeEnds.Inherit => "inherit"
    }.map { "stroke-linecap=\"%s\"".format(_) }.getOrElse("")
    val strokeLinejoinAttr = pen.corners.map {
      case StrokeCorners.Exact
        |  StrokeCorners.Exact(None)                => "miter"
      case StrokeCorners.Exact(Some(clipUnder))     =>
        // cheat by returning value with one more attr and its value
        "miter\" stroke-miterlimit=\"%f".format(clipUnder)
      case StrokeCorners.Round                      => "round"
      case StrokeCorners.Clip                       => "bevel"
      case StrokeCorners.Inherit                    => "inherit"
    }.map { "stroke-linejoin=\"%s\"".format(_) }.getOrElse("")

    val fillRuleAttr = pen.fillRule.map {
      case FillRule.NonZero      => "nonzero"
      case FillRule.EvenOdd      => "evenodd"
      case FillRule.WindingCount => "nonzero" // [SVG 2.0] "winding-count"
      case FillRule.Inherit      => "inherit"
    }.map { "fill-rule=\"%s\"".format(_) }.getOrElse("")

    writeGroup(
        defs2, os,
        List(
            fmt(attrs).trim, strokeAttr, fillAttr, strokeWidthAttr,
            strokeDashPatternAttr, strokeDashOffsetAttr,
            strokeLinecapAttr, strokeLinejoinAttr,
            strokeOpacityAttr, fillOpacityAttr, fillRuleAttr
          ).filter (_ != ""),
        shape
      )
  }

  private implicit def conv(visible: VisibleArea): Int = visible.hashCode

  protected def doMaskedShape(
      defs: Defs, os: Writer, masked: Shape, mask: Shape, attrs: UnivAttrs
    ): Defs = {
    val (url, newDefs) = defs.calcMaskUrl(mask)
    writeGroup(
        newDefs, os, "%smask=\"url(%s)\"".format(fmt(attrs), url), masked
      )
  }

  protected def doFilteredShape(
      defs: Defs, os: Writer, shape: Shape, filter: Filter, attrs: UnivAttrs
    ): Defs = {
    val (url, newDefs) = defs.calcUrl(filter)
    writeGroup(
        newDefs, os, "%sfilter=\"url(%s)\"".format(fmt(attrs), url), shape
      )
  }

  protected def doLink(
      defs: Defs, os: Writer, uri: String, target: LinkTarget, child: Shape,
      attrs: UnivAttrs
    ): Defs = {
    val targetStr = target match {
      case LinkTarget.Blank                        => "_blank"
      case LinkTarget.Replace                      => "_replace"
      case LinkTarget.Parent                       => "_parent"
      case LinkTarget.Top                          => "_top"
      case LinkTarget.Id(id) if XMLUtil.isName(id) => id
      case LinkTarget.Self | _                     => "_self"
    }

    os.write("    <a xlink:href=\"%s\" target=\"%s\">\n".format(
        XMLUtil.escape(uri), targetStr
      ))
    // NOTE: apply `attrs` not to link, but to child element
    val newDefs = renderShape(defs, os, child, attrs)
    os.write("    </a>\n")
    newDefs
  }

  protected def writeTransformedShape(
      defs: Defs, os: Writer, orderedTransforms: List[String], shape: Shape,
      attrs: UnivAttrs
    ): Defs = {
    def indent(s: String, level: Int): String = ("  " * level) + s

    def indentNesting(levels: Seq[String]) =
      levels.zipWithIndex.map( indent _ tupled )

    val groupOpens = {
      val transGrps = orderedTransforms.map("    <g transform=\""+ _ +"\">\n")
      if (attrs.isDefined) "    <g %s>\n".format(fmt(attrs).trim) :: transGrps
      else                                                           transGrps
    }
    os.write(groupOpens.mkString(""))
    // os.write(indentNesting(groupOpens).mkString(""))
    val newDefs = renderShape(defs, os, shape)
    os.write("    </g>\n" * groupOpens.length)
    // val groupCloses = List.fill(groupOpens.length)("    </g>\n")
    // os.write(indentNesting(groupCloses).reverse.mkString(""))
    newDefs
  }

  protected def writeGroup(
      defs: Defs, os: Writer, groupAttrs: List[String], child: Shape
    ): Defs = {
    os.write("    <g" + groupAttrs.mkString(" ", " ", "") + ">\n")
    val newDefs = renderShape(defs, os, child)
    os.write("    </g>\n")
    newDefs
  }

  protected def writeGroup(
      defs: Defs, os: Writer, groupAttr: String, child: Shape
    ): Defs = {
    writeGroup(defs, os, List(groupAttr), child)
  }


  protected def writeScriptUri(
      os: Writer, scriptType: ScriptType, uri: String
    ) {
    os.write("    <script type=\"%s\" xlink:href=\"%s\"/>\n".format(
        scriptType.mime, 
        XMLUtil.escape(uri)
      ))
  }

  protected def writeScriptContent(
      os: Writer, scriptType: ScriptType, content: String
    ) {
    os.write(
        "    <script type=\"%s\"> <![CDATA[\n%s\n    ]]> </script>\n".format(
            scriptType.mime, content
          ))
  }


  protected def orFalse(opt: Option[Boolean]): Boolean =
    opt.getOrElse(false)

  protected def fmt(attrs: UnivAttrs): String =
    attrs.id.filter(XMLUtil.isName(_)).map("id=\"" + _ + "\" ").getOrElse("") +
      attrs.opacity.map("opacity=\"" + clampOpacity(_) + "\" ").getOrElse("")

  protected def fmtColorName(color: Color): String =
    color match {
      case NamedColor(name, _, _, _) => name.toLowerCase
      case DescribedColor(desc)      => desc
      case _                          => color.asRgbString
    }

  protected def fmtReflectionTransform(
      degrees: Double, aboutX: Double, aboutY: Double
    ): String = {
    val (cosVal, sinVal) =
      (math.cos(2 * math.toRadians(degrees)),
       math.sin(2 * math.toRadians(degrees)))
    "matrix(%f, %f, %f, %f, %f, %f)".format(
        cosVal, sinVal, sinVal, -cosVal, aboutX, aboutY
      )
  }

  protected def clampOpacity(opacity: Double): Double =
    (opacity max 0.0) min 1.0

  private def fmtViewBoxDebugStr(viewBox: VisibleArea): String =
    "    <!-- viewBox: " + fmtViewBoxDims(viewBox) + " -->\n"

  private def fmtViewBoxDims(viewBox: VisibleArea): String =
    "(%f, %f) -+ %f x %f".format(viewBox.upperLeft.x, viewBox.upperLeft.y,
                                 viewBox.width,        viewBox.height)

  private def renderLicenseStamp(defs: Defs, os: Writer, viewBox: VisibleArea) {

    def fmtStampText: Text = {
      val stampFont = Font("Arial", 12)
      val (licenseStr, dateStr) = this.formattedLicenseTimestamp
      val captionLines =
          "Prepared by tie, " + Version + ":" ::
          "  " + Version.infoUrl              ::
          "rendered: " + dateStr              ::
          "under " + licenseStr + " // see:"  ::
          "  " + Version.licenseUrl           :: Nil
      TextBlock(captionLines, stampFont, false, 1.4).content :::
          TextBlock("   " + fmtViewBoxDims(viewBox), stampFont * .6, false, 3)
    }

    def placeStamp(
        stamp: Shape, sizeToViewBoxRatio: Double,
        lowerLeftOffsetToViewBoxRatio: Double
      ): Shape = {
      val Rectangular(w, h) = stamp.boundingBox
      val (viewW, viewH) = (viewBox.width, viewBox.height)
      val viewLowerLeft = viewBox.upperLeft -+ (0, viewH)

      val avgDims = ((viewW + viewH)/2) max (10.0 / sizeToViewBoxRatio)
      val smallDimTargSize = avgDims * sizeToViewBoxRatio

      val lowerLeftOffset   = avgDims * lowerLeftOffsetToViewBoxRatio

      val orientedStamp = (viewW < viewH, w < h) match {
        case (true,  true)  => stamp
        case (true,  false) => stamp -% -90
        case (false, false) => stamp
        case (false, true)  => stamp -% -90
      }
      val scaledStamp = orientedStamp -* (smallDimTargSize / (w min h))

      val Rectangular(scaledW, scaledH) = scaledStamp.boundingBox
      val stampTargCenterPt =
          viewLowerLeft -+
          (lowerLeftOffset, -lowerLeftOffset) -+
          (scaledW/2, -scaledH/2)
      scaledStamp -@ stampTargCenterPt
    }

    val stampText = fmtStampText
    val Rectangular(w, h) = stampText.textBoundingBox(textRulerFactory)
    val stampCaption = stampText -~ defaultWritingPen
    val (strokeColor, fillColor) = licenseString match {
      case Some(named) => (NamedColors.Lavender,    NamedColors.PapayaWhip)
      case None        => (NamedColors.PapayaWhip, NamedColors.Lavender)
    }
    val blot = Hexagon(w, w*1.15, h*1.8) -~ Pen(strokeColor, fillColor, 8)
    val stamp = placeStamp(blot -& stampCaption, 1.0/16, 1.0/8) -# 0.0
    renderShape(defs, os, licenseStampId -: stamp)
  }
}

}
