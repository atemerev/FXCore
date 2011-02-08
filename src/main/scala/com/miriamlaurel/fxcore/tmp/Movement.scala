package com.miriamlaurel.fxcore.tmp

import io.Source
import com.miriamlaurel.fxcore.Lane
import com.miriamlaurel.fxcore.pipscaler._
import com.miriamlaurel.fxcore.numbers.Decimal

/**
 * @author Alexander Temerev
 */

object Movement {
  def main(args: Array[String]) {

    var level: Decimal = null;
    var plusCount = 0;
    var minusCount = 0;

    Source.fromFile(args(0)).getLines.foreach(line => {
      val quote = Lane.fromCsv(line).bestQuote
      val spread = asPips(quote, (quote.ask.get - quote.bid.get))
      if (spread < 1) {
        val price = quote.average.get
        if (level == null) level = price
        if (asPips(quote, price - level) > 7) {
          level = price
          print("+")
          plusCount += 1
        }
        if (asPips(quote, price - level) < -7) {
          level = price
          print("-")
          minusCount += 1
        }
      }
    })
    println("\n" + plusCount + " / " + minusCount)
  }
}