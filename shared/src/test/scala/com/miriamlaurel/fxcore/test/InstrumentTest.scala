package com.miriamlaurel.fxcore.test

import com.miriamlaurel.fxcore._
import com.miriamlaurel.fxcore.asset.{Currency, Gold}
import com.miriamlaurel.fxcore.instrument.{CurrencyPair, Instrument, MetalInstrument}
import org.scalatest.{FunSuite, Matchers}

class InstrumentTest extends FunSuite with Matchers {

  test("currencies and metals should compare properly to each other") {
    AUD should equal(Currency("AUD"))
    Currency("XAY") should not equal Currency("XAU")
  }

  test("instruments should compare properly to each other") {
    EURUSD should equal(CurrencyPair("EUR/USD"))
    EURUSD should equal(Instrument(EUR, USD))
    MetalInstrument("XAU/USD") should equal(Instrument(Gold, USD))
  }

  test("more comparisons and reverses") {
    CurrencyPair("EUR/USD") should equal(Instrument(Currency("EUR"), Currency("USD")))
    EURUSD should equal(Instrument(USD, EUR).reverse)
    MetalInstrument("XAU/USD") should equal(Instrument(Gold, Currency("USD")))
  }

  test("basic cryptocurrencies test") {
    Currency("ETH") should equal(Ether)
    CurrencyPair("ETH/USD") should equal(Instrument(Ether, USD))
    CurrencyPair("ETH/USD") should equal(Instrument(Currency("ETH"), Currency("USD")))
    CurrencyPair("ETH/BTC") should equal(Instrument(Ether, Bitcoin))
  }
}
