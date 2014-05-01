package com.miriamlaurel.fxcore.test

import org.scalatest.Matchers
import org.scalatest.FunSuite
import com.miriamlaurel.fxcore.market.{Snapshot, Market}
import com.miriamlaurel.fxcore.instrument.CurrencyPair

/**
 * @author Alexander Temerev
 */
class MarketTest extends FunSuite with Matchers {
  test("Trivial cases") {
    val snapshot = Snapshot("1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.25215,2000000,1.2522,1000000,ASKS,1.25245,2000000,1.25246,1000000")
    val market = Market(snapshot)
    val best = market.quote(CurrencyPair("EUR/USD"))
    best.get.bid.get should equal(BigDecimal("1.2522"))
    best.get.ask.get should equal(BigDecimal("1.25245"))
    val self = market.quote(CurrencyPair("USD/USD"))
    self.get.bid.get should equal(BigDecimal(1))
    self.get.ask.get should equal(BigDecimal(1))
  }

  test("Reverse quote") {
    val snapshot = Snapshot("1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.25215,2000000,1.2522,1000000,ASKS,1.25245,2000000,1.25246,1000000")
    val market = Market(snapshot)
    val best = market.quote(CurrencyPair("USD/EUR"))
    best.get.normalize(1).bid.get should equal(BigDecimal("0.79844"))
    best.get.normalize(1).ask.get should equal(BigDecimal("0.79859"))
    val unknown = market.quote(CurrencyPair("EUR/CHF"))
    unknown.isDefined should equal(false)
  }

  test("Cross currency pair quote") {
    val eurSnapshot = Snapshot("1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.25215,2000000,1.2522,1000000,ASKS,1.25245,2000000,1.25246,1000000")
    val chfSnapshot = Snapshot("1273787999997,USD/CHF,BIDS,1.04075,2000000,1.04079,1000000,ASKS,1.04082,2000000,1.04091,1000000")
    val market = Market(eurSnapshot, chfSnapshot)
    val cross = market.quote(CurrencyPair("EUR/CHF"))
    cross.isDefined should equal(true)
    cross.get.normalize(1).bid.get should equal(BigDecimal("1.30328"))
    cross.get.normalize(1).ask.get should equal(BigDecimal("1.30358"))
  }
}