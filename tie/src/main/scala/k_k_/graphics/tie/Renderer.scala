/*
   file: k_k_/graphics/tie/Renderer.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie

import java.io.{File, BufferedOutputStream, FileOutputStream, OutputStream,
                BufferedWriter, OutputStreamWriter, Writer}


object Version {
  val (major, minor, rev) = (0, 9, 1)

  val (info_url, license_url, xmlns_uri) =
        ("http://tie-illustrates-everything.com/",
         "http://tie-illustrates-everything.com/licensing",
         "http://tie-illustrates-everything.com/xmlns/mixin/svg/1.0/")

  val (version_metadata_id, license_metadata_id, date_metadata_id) =
      ("tie__sw_version",   "tie__license_key",  "tie__render_tstamp")

  override def toString: String =
    "v.%d.%d.%d".format(major, minor, rev)
}


trait Renderer {

  def render(canvas: Canvas, os: OutputStream): Boolean =
    do_render(canvas, os)

  def render(canvas: Canvas, file: File, clobber_? : Boolean = false): Boolean =
    if (!clobber_? && !file.createNewFile())
      false
    else {
      val fos = new BufferedOutputStream(new FileOutputStream(file))
      try {
        render(canvas, fos)
      } finally {
        fos.close()
      }
    }

  def render(canvas: Canvas, fpath: String, clobber_? : Boolean): Boolean =
    render(canvas, new File(fpath), clobber_?)

  def render(canvas: Canvas, fpath: String): Boolean =
    render(canvas, new File(fpath))


  def license_string: Option[String] =
    Some("Apache License, v.2.0")

  final def formatted_license_timestamp: (String, String) = {
    val curr_secs  = System.currentTimeMillis / 1000
    val license_str = license_string.getOrElse("Unknown License")
    (license_str, fmt_datetime(curr_secs))
  }


  protected def do_render(canvas: Canvas, os: OutputStream): Boolean


  private def fmt_datetime(curr_secs: Long): String = {
    import _root_.java.text.SimpleDateFormat
    import _root_.java.util.{Date, Locale, TimeZone}

    val date_fmt = "yyyy-MM-dd HH:mm:ssZ"
    val date_formatter = {
      val df = new SimpleDateFormat(date_fmt, Locale.US)
      df.setTimeZone(TimeZone.getTimeZone("GMT+0"))
      df
    }
    date_formatter.format(curr_secs * 1000)
  }
}


trait Universal_Renderer[-Render_Targ_T, +Render_Result_T] { self: Renderer =>

  def render(canvas: Canvas, obj: Render_Targ_T): Option[Render_Result_T] =
    None
}


trait Char_Output_Renderer extends Renderer {

  val default_charset_name = "UTF-8"


  def render(canvas: Canvas, os: OutputStream, charset_name: String): Boolean =
    do_render(canvas, os, charset_name)

  def render(canvas: Canvas, file: File, charset_name: String): Boolean =
    render(canvas, file, false, charset_name)

  def render(canvas: Canvas, file: File, clobber_? : Boolean,
             charset_name: String): Boolean = {
    val renderer_sham = new Renderer {

      protected def do_render(canvas: Canvas, os: OutputStream): Boolean =
        Char_Output_Renderer.this.do_render(canvas, os, charset_name)
    }
    renderer_sham.render(canvas, file, clobber_?)
  }

  def render(canvas: Canvas, fpath: String, clobber_? : Boolean,
             charset_name: String): Boolean =
    render(canvas, new File(fpath), clobber_?, charset_name)

  def render(canvas: Canvas, fpath: String, charset_name: String): Boolean =
    render(canvas, new File(fpath), false, charset_name)


  protected def do_render(canvas: Canvas, os: OutputStream): Boolean =
    do_render(canvas, os, default_charset_name)

  protected def do_render(canvas: Canvas, os: OutputStream,
                          charset_name: String): Boolean = {
    val writer = new BufferedWriter(new OutputStreamWriter(os, charset_name))
    try {
      do_render(canvas, writer)
    } finally {
      writer.close
    }
  }


  protected def do_render(canvas: Canvas, writer: Writer): Boolean
}
