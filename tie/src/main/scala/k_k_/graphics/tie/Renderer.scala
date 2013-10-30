/*
   file: k_k_/graphics/tie/Renderer.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2013 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie

import java.io.{File, BufferedOutputStream, FileOutputStream, OutputStream,
                BufferedWriter, OutputStreamWriter, Writer}


object Version {
  final val (major, minor, rev) = (0, 11, 0)

  final val (infoUrl, licenseUrl, xmlnsUri) =
      ("http://tie-illustrates-everything.com/",
       "http://tie-illustrates-everything.com/licensing",
       "http://tie-illustrates-everything.com/xmlns/mixin/svg/1.0/")

  final val (versionMetadataId, licenseMetadataId, dateMetadataId) =
      ("tie__sw_version",   "tie__license_key",  "tie__render_tstamp")

  override def toString: String = "v.%d.%d.%d".format(major, minor, rev)
}


trait Renderer {

  def render(canvas: Canvas, os: OutputStream): Boolean = doRender(canvas, os)

  def render(canvas: Canvas, file: File, mayClobber: Boolean = false): Boolean =
    if (!mayClobber && !file.createNewFile())
      false
    else {
      val fos = new BufferedOutputStream(new FileOutputStream(file))
      try {
        render(canvas, fos)
      } finally {
        fos.close()
      }
    }

  def render(canvas: Canvas, fpath: String, mayClobber: Boolean): Boolean =
    render(canvas, new File(fpath), mayClobber)

  def render(canvas: Canvas, fpath: String): Boolean =
    render(canvas, new File(fpath))


  def licenseString: Option[String] =
    Some("Apache License, v.2.0")

  final def formattedLicenseTimestamp: (String, String) = {
    val currSecs  = System.currentTimeMillis / 1000
    val licenseStr = licenseString.getOrElse("Unknown License")
    (licenseStr, fmtDatetime(currSecs))
  }


  protected def doRender(canvas: Canvas, os: OutputStream): Boolean


  private def fmtDatetime(currSecs: Long): String = {
    import _root_.java.text.SimpleDateFormat
    import _root_.java.util.{Date, Locale, TimeZone}

    val dateFmt = "yyyy-MM-dd HH:mm:ssZ"
    val dateFormatter = {
      val df = new SimpleDateFormat(dateFmt, Locale.US)
      df.setTimeZone(TimeZone.getTimeZone("GMT+0"))
      df
    }
    dateFormatter.format(currSecs * 1000)
  }
}


trait UniversalRenderer[-RenderTargT, +RenderResultT] { self: Renderer =>

  def render(canvas: Canvas, obj: RenderTargT): Option[RenderResultT] =
    None
}


object CharOutputRenderer {
  final val defaultCharsetName = "UTF-8"
}

trait CharOutputRenderer extends Renderer {
  import CharOutputRenderer.defaultCharsetName

  def render(canvas: Canvas, os: OutputStream, charset: String): Boolean =
    doRender(canvas, os, charset)

  def render(canvas: Canvas, file: File, charset: String): Boolean =
    render(canvas, file, false, charset)

  def render(canvas: Canvas, file: File, mayClobber: Boolean, charset: String):
      Boolean = {
    val rendererShim = new Renderer {
      protected def doRender(canvas: Canvas, os: OutputStream): Boolean =
        CharOutputRenderer.this.doRender(canvas, os, charset)
    }
    rendererShim.render(canvas, file, mayClobber)
  }

  def render(
      canvas: Canvas,
      fpath: String,
      mayClobber: Boolean,
      charset: String
    ): Boolean =
    render(canvas, new File(fpath), mayClobber, charset)

  def render(canvas: Canvas, fpath: String, charset: String): Boolean =
    render(canvas, new File(fpath), false, charset)


  override protected final def doRender(canvas: Canvas, os: OutputStream):
      Boolean = {
    doRender(canvas, os, defaultCharsetName)
  }

  protected final def doRender(
      canvas: Canvas,
      os: OutputStream,
      charset: String
    ): Boolean = {
    val writer = new BufferedWriter(new OutputStreamWriter(os, charset))
    try {
      doRender(canvas, writer)
    } finally {
      writer.close()
    }
  }


  protected def doRender(canvas: Canvas, writer: Writer): Boolean
}
