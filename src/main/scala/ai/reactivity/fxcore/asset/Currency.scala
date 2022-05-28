package ai.reactivity.fxcore.asset

import ai.reactivity.fxcore.instrument.CurrencyPair

case class Currency(code: String) extends AssetClass {

  def /(quote: Currency) = CurrencyPair(this, quote)

  override def toString = code
}
