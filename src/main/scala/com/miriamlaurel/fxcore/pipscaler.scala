package com.miriamlaurel.fxcore

import scala.math._
import java.text.DecimalFormat
import com.miriamlaurel.fxcore.numbers.Decimal

/**
 * @author Alexander Temerev
 */
package object pipscaler {

  private val XAU_USD = CurrencyPair("XAU/USD")

  def pipScale(cp: CurrencyPair) = {
    if (cp == XAU_USD) 0 else cp.secondary.currency.getDefaultFractionDigits + 2
  }

  def pipValue(cp: CurrencyPair) = Decimal(pow(10, -pipScale(cp)))

  def asPips(cp: CurrencyPair, v: Decimal): Decimal = v / pipValue(cp)

  def asPips(instrument: Instrument, v: Decimal): Decimal = asPips(instrument.asInstanceOf[CurrencyPair], v)

  def asPips(q: Quote, v: Decimal): Decimal = asPips(q.instrument, v)

  def fromPips(cp: CurrencyPair, v: Decimal): Decimal = v * pipValue(cp)

  def fromPips(instrument: Instrument, v: Decimal): Decimal = fromPips(instrument.asInstanceOf[CurrencyPair], v)
  
  def fromPips(q: Quote, v: Decimal): Decimal = fromPips(q.instrument, v)

  def rescale(v: Decimal, scale: Int): Decimal = v.setScale(scale, BigDecimal.RoundingMode.HALF_EVEN)

  def scalePips(v: Decimal, cp: CurrencyPair): Decimal = rescale(v, pipScale(cp))

  def pretty(scale: Int, v: Decimal):String = {
    val fmt = new DecimalFormat()
    fmt.setMinimumFractionDigits(0)
    fmt.setMaximumFractionDigits(scale)
    return fmt.format(v.doubleValue)
  }
}