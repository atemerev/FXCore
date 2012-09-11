package com.miriamlaurel.fxcore

/**
 * @author Alexander Temerev
 */
trait Instrument {
  def base: AssetClass
  def counter: AssetClass
  def reverse: Instrument = Instrument(counter, base)
}

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

case class CurrencyPair(override val base: Currency, override val counter: Currency)
        extends Instrument {

  override def toString = base.toString + "/" + counter.toString
}

object CurrencyPair {
  def apply(ticker: String): CurrencyPair = {
    require(ticker.contains("/"))
    val tokens = ticker.split("/")
    CurrencyPair(Currency(tokens(0)), Currency(tokens(1)))
  }
}

case class MetalInstrument(override val base: Metal, override val counter: Currency) extends Instrument

object MetalInstrument {
  def apply(ticker: String): MetalInstrument = {
    require(ticker.contains("/"))
    val tokens = ticker.split("/")
    MetalInstrument(Metal(tokens(0)), Currency(tokens(1)))
  }
}