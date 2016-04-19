package com.miriamlaurel.fxcore.test

import com.miriamlaurel.fxcore.market.{OrderBook, Quote}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Alexander Temerev
  */
class OrderBookTest extends FunSuite with Matchers {

  private val orderBook = OrderBook("1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.2522,1000000,1.2523,1000000,ASKS,1.2524,1000000,1.25246,1000000")

  test("trim should leave minimum amount of best orders totalling greater than trim amount") {
    "1273787999996,EUR/USD,BIDS,1.2523,1000000,ASKS,1.2524,1000000" should equal(orderBook.trim(BigDecimal(500000)).toString)
  }

  test("quote to a limit should respect trimming rules") {
    orderBook.quote(1) should equal(orderBook.best)
    orderBook.quote(2000000) should equal(Quote(orderBook.instrument, Some(BigDecimal("1.25225")), Some(BigDecimal("1.25243")), orderBook.timestamp))
  }
}