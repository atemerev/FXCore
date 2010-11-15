package com.miriamlaurel.fxcore

import io.Source
import java.text.SimpleDateFormat
import scala.collection._
import java.util.Date
import java.io.{FileWriter, BufferedWriter, Writer}

/**
 * @author Alexander Temerev
 */
object LineSplit {

  val format = new SimpleDateFormat("yyyy-MM-dd")
  val writers = mutable.Map[String, Writer]()

  def writer(ts: Date) = {
    val filename = format.format(ts) + ".csv"
    writers.getOrElseUpdate(filename, new BufferedWriter(new FileWriter(filename)))
  }

  def main(args: Array[String]) {
    Source.fromFile(args(0)).getLines.foreach(line => {
      val ts = new Date(line.split(',')(0).toLong)
      writer(ts).write(line + "\n")
    })
    writers.foreach(_._2.flush)
  }
}