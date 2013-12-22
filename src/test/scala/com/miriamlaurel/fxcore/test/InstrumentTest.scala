package com.miriamlaurel.fxcore.test

import org.scalatest.FunSuite
import org.scalatest.Matchers
import com.miriamlaurel.fxcore._
import com.miriamlaurel.fxcore.Currency
import com.miriamlaurel.fxcore.Instrument

/**
 * @author Alexander Temerev
 */
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

}
