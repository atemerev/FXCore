package com.miriamlaurel.fxcore.test

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.fixture.FunSuite
import com.miriamlaurel.fxcore._
import com.miriamlaurel.fxcore.numbers._

/**
 * @author Alexander Temerev
 */

class AccountingTest extends FunSuite with ShouldMatchers {

  type FixtureParam = Market

  def withFixture(test: OneArgTest) {
    val market = Market(
      lane("EUR/USD", "1.3000", "1.3050"),
      lane("USD/CHF", "1.2000", "1.2050"),
      lane("USD/JPY", "125.00", "125.50")
    )
    test(market)
  }

  test("Conversion") {
    () => {
      val market = Market(
        lane("EUR/USD", "1.4430", "1.4432"),
        lane("USD/CHF", "1.1529", "1.1532"),
        lane("USD/JPY", "113.265", "113.29")
        )
      market.bestQuote(CurrencyPair("CHF/JPY")).get.bid.get.setScale(3) should equal(Decimal("98.218"))
      market.convert(Money("1 CHF"), Currency("JPY"), OfferSide.Bid).get.setScale(3) should equal(Money("98.218 JPY"))
    }
  }

  test("USD/JPY accounting") {
    market => {
      var p = new StrictPortfolio
      val opening = new Position(CurrencyPair("USD/JPY"), Decimal("125.50"), Decimal("500000"))
      p = (p << opening)._1
      val q = market.bestQuote(CurrencyPair("USD/JPY")).get
      opening.profitLoss(q).get should equal(Money("-250000 JPY"))
      opening.profitLossIn(Currency("USD"), market).get should equal(Money("-2000 USD"))
      opening.profitLossIn(Currency("CHF"), market).get should equal(Money("-2410 CHF"))
    }
  }

  test("Merge subtract") {
    market => {
      val p1 = new Position(CurrencyPair("GBP/USD"), Decimal(2), Decimal(1000))
      val p2 = new Position(CurrencyPair("GBP/USD"), Decimal(3), Decimal(-300))
      val r = (p1 merge p2)._1.get
      val r2 = (p2 merge p1)._1.get
      val pl = (p1 merge p2)._2.amount
      r.primary should equal(r2.primary)
      r.secondary should equal(r2.secondary)
      r.primary.amount should equal(Decimal(700))
      r.secondary.amount should equal(Decimal(-1400))
      pl should equal(Decimal(300))
    }
  }

  test("Account with non-strict portfolio") {
    market => {
      val portfolio = new NonStrictPortfolio
      var account = new Account(portfolio)
      val eurUsd: CurrencyPair = CurrencyPair("EUR/USD")
      val p1 = new Position(eurUsd, Decimal("1.3050"), Decimal(1000))
      val p2 = new Position(eurUsd, Decimal("1.3100"), Decimal(2000))
      account = (account << (p1, market)).get
      account = (account << (p2, market)).get
      account.balance should equal(Money("0 USD"))
      account.portfolio.positions(eurUsd).size should equal(2)
      val pc1 = new Position(eurUsd, Decimal("1.3000"), Decimal(-1000), Some(p1.uuid))
      val pc2 = new Position(eurUsd, Decimal("1.3000"), Decimal(-2000), Some(p2.uuid))
      account = (account << (pc1, market)).get
      account.portfolio.positions(eurUsd).size should equal(1)
      account.balance should equal(Money("-5 USD"))
      account = (account << (pc2, market)).get
      account.portfolio.positions(eurUsd).size should equal(0)
      account.balance should equal(Money("-25 USD"))
      account = (account << (p1, market)).get
      account.balance should equal(Money("-25 USD"))
    }
  }

  test("Equal and opposite positions") {
    market => {
      val portfolio = new NonStrictPortfolio
      var account = new Account(portfolio)
      val eurUsd: CurrencyPair = CurrencyPair("EUR/USD")
      val p1 = new Position(eurUsd, Decimal("1.3050"), Decimal(1000))
      val p2 = new Position(eurUsd, Decimal("1.3100"), Decimal(-1000))
      account = (account << (p1, market)).get
      account = (account << (p2, market)).get
      account.portfolio.positions(eurUsd).size should equal(2)
    }
  }

  test("Portfolio-level position merging") {
    market => {
      var portfolio = new NonStrictPortfolio
      val eurUsd: CurrencyPair = CurrencyPair("EUR/USD")
      val p1 = new Position(eurUsd, Decimal("1.3050"), Decimal(1000))
      val p2 = new Position(eurUsd, Decimal("1.3100"), Decimal(1000))
      val p3 = new Position(eurUsd, Decimal("1.3300"), Decimal(1000))
      portfolio = (portfolio << p1)._1
      portfolio = (portfolio << p2)._1
      portfolio = (portfolio << p3)._1
      val (newPortfolio, diff) = portfolio.mergePositions(Set(p1.uuid, p2.uuid, p3.uuid))
      newPortfolio.positions.size should equal(1)
      val merged = newPortfolio.positions.head
      merged.amount should equal(Decimal(3000))
      diff.actions.head.isInstanceOf[MergePositions] should equal(true)
      val action = diff.actions.head.asInstanceOf[MergePositions]
      action.result.get.amount should equal(3000)
      action.result.get.price should equal(Decimal("1.3150"))
      action.adjustment should equal(Zilch)
      portfolio = newPortfolio
      val p4 = new Position(eurUsd, Decimal("1.3100"), Decimal(-1000))
      portfolio = (portfolio << p4)._1
      portfolio.positions.size should equal(2)
      val (newPortfolio2, diff2) = portfolio.mergePositions(Set(merged.uuid, p4.uuid))
      newPortfolio2.positions.size should equal(1)
      val merged2 = newPortfolio2.positions.head
      merged2.amount should equal(Decimal(2000))
      diff2.actions.head.asInstanceOf[MergePositions].adjustment should equal(Money("-5 USD"))
    }
  }

  def lane(instrument: String, bid: String, ask: String) =
    Lane(System.currentTimeMillis + "," + instrument + ",BIDS," + bid + ",1000000,ASKS," + ask + ",1000000")
}