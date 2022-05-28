package ai.reactivity.fxcore.test

import ai.reactivity.fxcore._
import ai.reactivity.fxcore.asset.Currency
import ai.reactivity.fxcore.instrument.{CurrencyPair, Instrument}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InstrumentTest extends AnyFunSuite with Matchers {

  test("currencies and metals should compare properly to each other") {
    AUD should equal(Currency("AUD"))
    Currency("XAY") should not equal Currency("XAU")
  }

  test("instruments should compare properly to each other") {
    EURUSD should equal(CurrencyPair("EUR/USD"))
    EURUSD should equal(Instrument(EUR, USD))
  }

  test("more comparisons and reverses") {
    CurrencyPair("EUR/USD") should equal(Instrument(Currency("EUR"), Currency("USD")))
    EURUSD should equal(Instrument(USD, EUR).reverse)
  }

  test("basic cryptocurrencies test") {
    Currency("ETH") should equal(Ether)
    CurrencyPair("ETH/USD") should equal(Instrument(Ether, USD))
    CurrencyPair("ETH/USD") should equal(Instrument(Currency("ETH"), Currency("USD")))
    CurrencyPair("ETH/BTC") should equal(Instrument(Ether, Bitcoin))
  }
}
