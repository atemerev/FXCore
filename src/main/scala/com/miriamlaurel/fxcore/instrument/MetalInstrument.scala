package com.miriamlaurel.fxcore.instrument

import com.miriamlaurel.fxcore.asset._

case class MetalInstrument(override val base: Metal, override val counter: Currency) extends Instrument {
  override def toString = s"$base/$counter"
}

object MetalInstrument {
  def apply(ticker: String): MetalInstrument = {
    require(ticker.contains("/"))
    val tokens = ticker.split("/")
    MetalInstrument(Metal(tokens(0)), Currency(tokens(1)))
  }
}