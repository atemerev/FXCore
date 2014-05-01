package com.miriamlaurel.fxcore.instrument

import com.miriamlaurel.fxcore.asset._

object Instrument {
  def apply(base: AssetClass, counter: AssetClass) = (base, counter) match {
    case (b: Currency, c: Currency) => CurrencyPair(b, c)
    case (b: Metal, c: Currency) => MetalInstrument(b, c)
    case _ => throw new IllegalArgumentException("Asset classes not recognized: %s to %s".format(
      base.getClass.getSimpleName, counter.getClass.getSimpleName))
  }

  // Poor man's dependent types...

  def apply(base: Currency, counter: Currency): CurrencyPair = apply(base.asInstanceOf[AssetClass], counter.asInstanceOf[AssetClass]).asInstanceOf[CurrencyPair]
  def apply(base: Metal, counter: Currency): MetalInstrument = apply(base.asInstanceOf[AssetClass], counter.asInstanceOf[AssetClass]).asInstanceOf[MetalInstrument]
}

trait Instrument {
  def base: AssetClass
  def counter: AssetClass
  def reverse: Instrument = Instrument(counter, base)
}