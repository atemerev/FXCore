package ai.reactivity.fxcore.instrument

import ai.reactivity.fxcore.asset.Currency

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
