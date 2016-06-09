package com.miriamlaurel.fxcore.test

import com.miriamlaurel.fxcore.instrument.CurrencyPair
import com.miriamlaurel.fxcore.market.{Market, OrderBook}
import org.scalatest.{FunSuite, Matchers}

class MarketTest extends FunSuite with Matchers {
  test("Trivial cases") {
    val book = OrderBook("1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.25215,2000000,1.2522,1000000,ASKS,1.25245,2000000,1.25246,1000000")
    val market = Market(book)
    val best = market.quote(CurrencyPair("EUR/USD"))
    best.get.bid.get should equal(BigDecimal("1.2522"))
    best.get.ask.get should equal(BigDecimal("1.25245"))
    val self = market.quote(CurrencyPair("USD/USD"))
    self.get.bid.get should equal(BigDecimal(1))
    self.get.ask.get should equal(BigDecimal(1))
  }

  test("Reverse quote") {
    val book = OrderBook("1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.25215,2000000,1.2522,1000000,ASKS,1.25245,2000000,1.25246,1000000")
    val market = Market(book)
    val best = market.quote(CurrencyPair("USD/EUR"))
    best.get.normalize(1).bid.get should equal(BigDecimal("0.79844"))
    best.get.normalize(1).ask.get should equal(BigDecimal("0.79859"))
    val unknown = market.quote(CurrencyPair("EUR/CHF"))
    unknown.isDefined should equal(false)
  }

  test("Cross currency pair quote") {
    val eurbook = OrderBook("1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.25215,2000000,1.2522,1000000,ASKS,1.25245,2000000,1.25246,1000000")
    val chfbook = OrderBook("1273787999997,USD/CHF,BIDS,1.04075,2000000,1.04079,1000000,ASKS,1.04082,2000000,1.04091,1000000")
    val market = Market(eurbook, chfbook)
    val cross = market.quote(CurrencyPair("EUR/CHF"))
    cross.isDefined should equal(true)
    cross.get.normalize(1).bid.get should equal(BigDecimal("1.30328"))
    cross.get.normalize(1).ask.get should equal(BigDecimal("1.30358"))
  }
}