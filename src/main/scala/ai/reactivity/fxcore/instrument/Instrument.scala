package ai.reactivity.fxcore.instrument

import ai.reactivity.fxcore.asset._

object Instrument {
  def apply(base: AssetClass, counter: AssetClass) = (base, counter) match {
    case (b: Currency, c: Currency) => CurrencyPair(b, c)
    case _ => throw new IllegalArgumentException("Asset classes not recognized: %s to %s".format(
      base.getClass.getSimpleName, counter.getClass.getSimpleName))
  }

  // Poor man's dependent types...

  def apply(base: Currency, counter: Currency): CurrencyPair = apply(base.asInstanceOf[AssetClass], counter.asInstanceOf[AssetClass]).asInstanceOf[CurrencyPair]
}

trait Instrument {
  def base: AssetClass

  def counter: AssetClass

  def reverse: Instrument = Instrument(counter, base)
}