package com.miriamlaurel.fxcore.test

import java.time.Instant

import com.miriamlaurel.fxcore.market._
import org.scalatest.{FunSuite, Matchers}
import com.miriamlaurel.fxcore._

class OrderBookTest extends FunSuite with Matchers {

  private val orderBook = OrderBook("1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.2522,1000000,1.2523,1000000,ASKS,1.2524,1000000,1.25246,1000000")

  test("trim should leave minimum amount of best orders totalling greater than trim amount") {
    "1273787999996,EUR/USD,BIDS,1.2523,1000000,ASKS,1.2524,1000000" should equal(orderBook.trim(BigDecimal(500000)).toString)
  }

  test("quote to a limit should respect trimming rules") {
    orderBook.quote(1) should equal(orderBook.best)
    orderBook.quote(2000000) should equal(Quote(orderBook.instrument, Some(BigDecimal("1.25225")), Some(BigDecimal("1.25243")), orderBook.timestamp))
  }

  test("remove best bid from order book") {
    val newBook = orderBook - OrderKey(Me, EURUSD, QuoteSide.Bid, "0")
    "1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.2522,1000000,ASKS,1.2524,1000000,1.25246,1000000" should equal(newBook.toString)
  }

  test("remove mid bid from order book") {
    val newBook = orderBook - OrderKey(Me, EURUSD, QuoteSide.Bid, "2")
    "1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.2522,1000000,1.2523,1000000,ASKS,1.2524,1000000,1.25246,1000000" should equal(newBook.toString)
  }

  test("remove best ask from order book") {
    val newBook = orderBook - OrderKey(Me, EURUSD, QuoteSide.Ask, "0")
    "1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.2522,1000000,1.2523,1000000,ASKS,1.25246,1000000" should equal(newBook.toString)
  }

  test("remove far ask from order book") {
    val newBook = orderBook - OrderKey(Me, EURUSD, QuoteSide.Ask, "1")
    "1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.2522,1000000,1.2523,1000000,ASKS,1.2524,1000000" should equal(newBook.toString)
  }

  test("add mid bid to order book") {
    val newBook = orderBook + (Order(OrderKey(Me, EURUSD, QuoteSide.Bid, "*"), BigDecimal("500000"), BigDecimal("1.25214")), Instant.ofEpochMilli(1273787999996L))
    "1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.25214,500000,1.2522,1000000,1.2523,1000000,ASKS,1.2524,1000000,1.25246,1000000" should equal(newBook.toString)
    val newBook2 = newBook + (Order(OrderKey(Me, EURUSD, QuoteSide.Bid, "*"), BigDecimal("1000000"), BigDecimal("1.25214")), Instant.ofEpochMilli(1273787999996L))
    "1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.25214,1000000,1.2522,1000000,1.2523,1000000,ASKS,1.2524,1000000,1.25246,1000000" should equal(newBook2.toString)
    val newBook3 = newBook2 + (Order(OrderKey(Me, EURUSD, QuoteSide.Bid, "*2"), BigDecimal("1000"), BigDecimal("1.25214")), Instant.ofEpochMilli(1273787999996L))
    "1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.25214,1000,1.25214,1000000,1.2522,1000000,1.2523,1000000,ASKS,1.2524,1000000,1.25246,1000000" should equal(newBook3.toString)
  }
}