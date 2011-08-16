package com.miriamlaurel.fxcore

import io.Source
import java.text.SimpleDateFormat
import scala.collection._
import java.util.{TimeZone, Date}
import java.io._

/**
 * @author Alexander Temerev
 */
object LineSplit {

  val format = new SimpleDateFormat("yyyy-MM-dd")
  format.setTimeZone(TimeZone.getTimeZone("UTC"))

  val writers = mutable.Map[String, Writer]()

  def writer(dir: String, ts: Date) = {
    val filename = dir + "/" + format.format(ts) + ".csv"
    writers.getOrElseUpdate(filename, new BufferedWriter(new FileWriter(filename)))
  }

  def main(args: Array[String]) {

    object Filter extends FilenameFilter {
      def accept(p1: File, p2: String) = p2.endsWith(".partial")
    }
    def files = new File(".").list(Filter)
    val Extractor = """^(\w\w\w\-\w\w\w)\.txt.partial$""".r

    files.foreach(filename => {
      println(<t>Processing {filename}...</t>.text)
      val Extractor(v1) = filename
      val dir = v1.toLowerCase.replaceFirst("\\-", "")
      new File(dir).mkdir()
      Source.fromFile(filename).getLines.foreach(line => {
        val ts = new Date(line.substring(0, 13).toLong)
        writer(dir, ts).write(line + "\n")
      })
      writers.values.foreach(_.flush)
    })
  }
}