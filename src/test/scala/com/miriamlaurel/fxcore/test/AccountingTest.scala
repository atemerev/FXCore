package com.miriamlaurel.fxcore.test

import com.miriamlaurel.fxcore._
import com.miriamlaurel.fxcore.market.{Market, OrderBook, QuoteSide}
import com.miriamlaurel.fxcore.instrument.CurrencyPair
import com.miriamlaurel.fxcore.asset.Currency
import com.miriamlaurel.fxcore.portfolio.{MergePositions, NonStrictPortfolio, Position, StrictPortfolio}
import com.miriamlaurel.fxcore.accounting.Ledger
import org.scalatest.{FunSuite, Matchers}

class AccountingTest extends FunSuite with Matchers {

  val market = Market(
    snapshot("EUR/USD", "1.3000", "1.3050"),
    snapshot("USD/CHF", "1.2000", "1.2050"),
    snapshot("USD/JPY", "125.00", "125.50")
  )

  test("Conversion") {
    val market = Market(
      snapshot("EUR/USD", "1.4430", "1.4432"),
      snapshot("USD/CHF", "1.1529", "1.1532"),
      snapshot("USD/JPY", "113.265", "113.29")
    )
    market.quote(CurrencyPair("CHF/JPY")).get.bid.get.setScale(3, BigDecimal.RoundingMode.HALF_EVEN) should equal(BigDecimal("98.218"))
    market.convert(Money("1 CHF"), Currency("JPY"), QuoteSide.Bid).get.setScale(3) should equal(Money("98.218 JPY"))
  }

  test("USD/JPY accounting") {
    var p = new StrictPortfolio
    val opening = Position(CurrencyPair("USD/JPY"), BigDecimal("125.50"), BigDecimal("500000"))
    p = (p << opening)._1
    val q = market.quote(CurrencyPair("USD/JPY")).get
    opening.profitLoss(q).get should equal(Money("-250000 JPY"))
    opening.profitLossIn(Currency("USD"), market).get should equal(Money("-2000 USD"))
    opening.profitLossIn(Currency("CHF"), market).get should equal(Money("-2410 CHF"))
  }

  test("Merge subtract") {
    val p1 = Position(CurrencyPair("GBP/USD"), BigDecimal(2), BigDecimal(1000))
    val p2 = Position(CurrencyPair("GBP/USD"), BigDecimal(3), BigDecimal(-300))
    val r = (p1 merge p2)._1.get
    val r2 = (p2 merge p1)._1.get
    val pl = (p1 merge p2)._2.amount
    r.primary should equal(r2.primary)
    r.secondary should equal(r2.secondary)
    r.primary.amount should equal(BigDecimal(700))
    r.secondary.amount should equal(BigDecimal(-1400))
    pl should equal(BigDecimal(300))
  }

  test("Non-strict portfolio") {
    var portfolio = new NonStrictPortfolio
    val p1 = Position(EURUSD, BigDecimal("1.3050"), BigDecimal(1000))
    val p2 = Position(EURUSD, BigDecimal("1.3100"), BigDecimal(2000))
    portfolio = (portfolio << p1)._1
    portfolio = (portfolio << p2)._1
    portfolio.positions(EURUSD).size should equal(2)
    val pc1 = Position(EURUSD, BigDecimal("1.3000"), BigDecimal(-1000), Some(p1.id))
    val pc2 = Position(EURUSD, BigDecimal("1.3000"), BigDecimal(-2000), Some(p2.id))
    portfolio = (portfolio << pc1)._1
    portfolio.positions(EURUSD).size should equal(1)
    portfolio = (portfolio << pc2)._1
    portfolio.positions(EURUSD).size should equal(0)
  }

  test("Equal and opposite positions") {
    var portfolio = new NonStrictPortfolio
    val eurUsd: CurrencyPair = CurrencyPair("EUR/USD")
    val p1 = Position(eurUsd, BigDecimal("1.3050"), BigDecimal(1000))
    val p2 = Position(eurUsd, BigDecimal("1.3100"), BigDecimal(-1000))
    portfolio = (portfolio << p1)._1
    portfolio = (portfolio << p2)._1
    portfolio.positions(eurUsd).size should equal(2)
  }

  test("Portfolio-level position merging") {
    var portfolio = new NonStrictPortfolio
    val eurUsd: CurrencyPair = CurrencyPair("EUR/USD")
    val p1 = Position(eurUsd, BigDecimal("1.3050"), BigDecimal(1000))
    val p2 = Position(eurUsd, BigDecimal("1.3100"), BigDecimal(1000))
    val p3 = Position(eurUsd, BigDecimal("1.3300"), BigDecimal(1000))
    portfolio = (portfolio << p1)._1
    portfolio = (portfolio << p2)._1
    portfolio = (portfolio << p3)._1
    val (newPortfolio, diff) = portfolio.mergePositions(Set(p1.id, p2.id, p3.id))
    newPortfolio.positions.size should equal(1)
    val merged = newPortfolio.positions.head
    merged.absoluteAmount should equal(BigDecimal(3000))
    diff.actions.head.isInstanceOf[MergePositions] should equal(true)
    val action = diff.actions.head.asInstanceOf[MergePositions]
    action.result.get.absoluteAmount should equal(3000)
    action.result.get.price should equal(BigDecimal("1.3150"))
    action.adjustment should equal(Zilch)
    portfolio = newPortfolio
    val p4 = Position(eurUsd, BigDecimal("1.3100"), BigDecimal(-1000))
    portfolio = (portfolio << p4)._1
    portfolio.positions.size should equal(2)
    val (newPortfolio2, diff2) = portfolio.mergePositions(Set(merged.id, p4.id))
    newPortfolio2.positions.size should equal(1)
    val merged2 = newPortfolio2.positions.head
    merged2.absoluteAmount should equal(BigDecimal(2000))
    diff2.actions.head.asInstanceOf[MergePositions].adjustment should equal(Money("-5 USD"))
  }

  def snapshot(instrument: String, bid: String, ask: String) =
    OrderBook(System.currentTimeMillis + "," + instrument + ",BIDS," + bid + ",1000000,ASKS," + ask + ",1000000")
}