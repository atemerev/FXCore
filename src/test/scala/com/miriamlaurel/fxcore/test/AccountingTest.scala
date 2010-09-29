package com.miriamlaurel.fxcore.test

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.fixture.FixtureFunSuite
import com.miriamlaurel.fxcore._
import com.miriamlaurel.fxcore.numbers._

/**
 * @author Alexander Temerev
 */

/*
    public @Before void init() {
        account = new Account(Money.of("100000 CHF"));
        account.setName("test");
        market = new Market("USD");
        quote("EUR/USD", "1.3000", "1.3050");
        quote("USD/CHF", "1.2000", "1.2050");
        quote("USD/JPY", "125.00", "125.50");
        account.setMarket(market);
    }

    public @Test void testJpy() throws Exception {
        Position opening = new Position(USD_JPY, new Monetary("125.50"), new Monetary("500000"));
        opening.setId(id++);
        opening.lockPosition(market, account.getAccountingAsset());
        opening.setAccountId(account.getId());
        Position processed = account.processPosition(opening);
        Assert.assertEquals("Positions size should be 1", 1, account.getAllPositionsMap().size());
        Position opened = account.getAllPositionsMap().values().iterator().next();
        Assert.assertNotNull("First opened position must not be null", opened);
        Assert.assertEquals("Processed and opened positions should be equal", processed, opened);
        Assert.assertEquals(Money.of("-250000 JPY"), opened.profitLoss(market));
        Position closing = new Position(USD_JPY, new Monetary("125.00"), new Monetary("-500000"));
        closing.lockPosition(market, account.getAccountingAsset());
        closing.setAccountId(account.getId());
        closing.setId(opening.getId());
        account.processPosition(closing);
        Assert.assertEquals(Money.of("97590 CHF"), account.getBalance());
    }

    public @Test void testChfJpy() throws Exception {
        quote("EUR/USD", "1.4430", "1.4432");
        quote("USD/CHF", "1.1529", "1.1532");
        quote("USD/JPY", "113.265", "113.29");
        Monetary ask = market.getPrice(CurrencyAsset.getInstance("CHF"),
                CurrencyAsset.getInstance("JPY"),
                QuoteSide.BID).getValue();
        Assert.assertEquals(new Monetary("98.218"), ask.setScale(3));
    }

 */

class AccountingTest extends FixtureFunSuite with ShouldMatchers {
  type FixtureParam = (Market, StrictPortfolio)

  def withFixture(test: OneArgTest) {
    val market = Market(
      lane("EUR/USD", "1.3000", "1.3050"),
      lane("USD/CHF", "1.2000", "1.2050"),
      lane("USD/JPY", "125.00", "125.50")
    )
    val portfolio = new StrictPortfolio
    test((market, portfolio))
  }

  test("Conversion") {
    () => {
      val market = Market(
        lane("EUR/USD", "1.4430", "1.4432"),
        lane("USD/CHF", "1.1529", "1.1532"),
        lane("USD/JPY", "113.265", "113.29")
        )
      market.bestQuote(CurrencyPair("CHF/JPY")).get.bid.get.setScale(3) should equal(Decimal("98.218"))
      market.convert(Money("1 CHF"), CurrencyAsset("JPY"), OfferSide.Bid).get.setScale(3) should equal(Money("98.218 JPY")) 
    }
  }

  test("USD/JPY accounting") {
    fixture => {
      val (market, portfolio) = fixture
      var p: StrictPortfolio = portfolio
      val opening = new Position(CurrencyPair("USD/JPY"), Decimal("125.50"), Decimal("500000"))
      p = (p << opening)._1
      val q = market.bestQuote(CurrencyPair("USD/JPY")).get
      opening.profitLoss(q).get should equal(Money("-250000 JPY"))
      opening.profitLossIn(CurrencyAsset("USD"), market).get should equal(Money("-2000 USD"))
      opening.profitLossIn(CurrencyAsset("CHF"), market).get should equal(Money("-2410 CHF"))
    }
  }

  test("Merge subtract") {
    fixture => {
      val (market, portfolio) = fixture
      var pf: StrictPortfolio = portfolio
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

  def lane(instrument: String, bid: String, ask: String) =
    Lane(System.currentTimeMillis + "," + instrument + ",BIDS," + bid + ",1000000,ASKS," + ask + ",1000000")
}