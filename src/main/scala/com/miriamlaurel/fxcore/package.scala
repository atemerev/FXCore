package com.miriamlaurel

import scala.math._
import scala.BigDecimal
import java.text.DecimalFormat

package object fxcore {

  val USD = Currency("USD")
  val EUR = Currency("EUR")
  val GBP = Currency("GBP")
  val JPY = Currency("JPY")
  val AUD = Currency("AUD")
  val CHF = Currency("CHF")
  val CAD = Currency("CAD")
  val NZD = Currency("NZD")
  val SEK = Currency("SEK")
  val NOK = Currency("NOK")
  val EURUSD = CurrencyPair("EUR/USD")
  val GBPUSD = CurrencyPair("GBP/USD")
  val USDJPY = CurrencyPair("USD/JPY")
  val USDCHF = CurrencyPair("USD/CHF")

  def pipScale(cp: CurrencyPair) = cp.counter.pointScale + 2

  def pipValue(cp: CurrencyPair) = BigDecimal(pow(10, -pipScale(cp)))

  def asPips(cp: CurrencyPair, v: BigDecimal): BigDecimal = v / pipValue(cp)

  def asPips(instrument: Instrument, v: BigDecimal): BigDecimal = asPips(instrument.asInstanceOf[CurrencyPair], v)

  def asPips(q: Quote, v: BigDecimal): BigDecimal = asPips(q.instrument, v)

  def fromPips(cp: CurrencyPair, v: BigDecimal): BigDecimal = v * pipValue(cp)

  def fromPips(instrument: Instrument, v: BigDecimal): BigDecimal = fromPips(instrument.asInstanceOf[CurrencyPair], v)

  def fromPips(q: Quote, v: BigDecimal): BigDecimal = fromPips(q.instrument, v)

  def rescale(v: BigDecimal, scale: Int): BigDecimal = v.setScale(scale, BigDecimal.RoundingMode.HALF_EVEN)

  def scalePips(v: BigDecimal, cp: CurrencyPair): BigDecimal = rescale(v, pipScale(cp))

  def scalePips(v: BigDecimal, instrument: Instrument): BigDecimal = scalePips(v, instrument.asInstanceOf[CurrencyPair])

  def pretty(scale: Int, v: BigDecimal): String = {
    val fmt = new DecimalFormat()
    fmt.setMinimumFractionDigits(0)
    fmt.setMaximumFractionDigits(scale)
    fmt.format(v.doubleValue())
  }

  object Me extends Party("me")
  object Unknown extends Party("???")
}