package com.miriamlaurel.fxcore.test

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import com.miriamlaurel.fxcore._
import com.miriamlaurel.fxcore.numbers._

/**
 * @author Alexander Temerev
 */
class MarketTest extends FunSuite with ShouldMatchers {
  test("Trivial cases") {
    val lane = Lane("1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.25215,2000000,1.2522,1000000,ASKS,1.25245,2000000,1.25246,1000000")
    val market = Market(lane)
    val best = market.bestQuote(CurrencyPair("EUR/USD"))
    best.get.bid.get should equal(Decimal("1.2522"))
    best.get.ask.get should equal(Decimal("1.25245"))
    val self = market.bestQuote(CurrencyPair("USD/USD"))
    self.get.bid.get should equal(Decimal(1))
    self.get.ask.get should equal(Decimal(1))
  }

  test("Reverse quote") {
    val lane = Lane("1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.25215,2000000,1.2522,1000000,ASKS,1.25245,2000000,1.25246,1000000")
    val market = Market(lane)
    val best = market.bestQuote(CurrencyPair("USD/EUR"))
    best.get.normalize(1).bid.get should equal(Decimal("0.79844"))
    best.get.normalize(1).ask.get should equal(Decimal("0.79859"))
    val unknown = market.bestQuote(CurrencyPair("EUR/CHF"))
    unknown.isDefined should equal(false)
  }

  test("Cross currency pair quote") {
    val eurLane = Lane("1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.25215,2000000,1.2522,1000000,ASKS,1.25245,2000000,1.25246,1000000")
    val chfLane = Lane("1273787999997,USD/CHF,BIDS,1.04075,2000000,1.04079,1000000,ASKS,1.04082,2000000,1.04091,1000000")
    val market = Market(eurLane, chfLane)
    val cross = market.bestQuote(CurrencyPair("EUR/CHF"))
    cross.isDefined should equal(true)
    cross.get.normalize(1).bid.get should equal(Decimal("1.30328"))
    cross.get.normalize(1).ask.get should equal(Decimal("1.30358"))
  }
}