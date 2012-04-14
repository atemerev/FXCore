package com.miriamlaurel.fxcore.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import com.miriamlaurel.fxcore.numbers.Decimal
import com.miriamlaurel.fxcore.{Quote, Lane}

/**
 * @author Alexander Temerev
 */
class LaneTest extends FunSuite with ShouldMatchers {

  private val lane = Lane("1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.2522,1000000,1.2523,1000000,ASKS,1.2524,1000000,1.25246,1000000")

  test("trim should leave minimum amount of best offers totalling greater than trim amount") {
    "1273787999996,EUR/USD,BIDS,1.2523,1000000,ASKS,1.2524,1000000" should equal (lane.trim(Decimal(500000)).toString)
  }

  test("quote to a limit should respect trimming rules") {
    lane.quote(1) should equal (lane.bestQuote)
    lane.quote(2000000) should equal (Quote(lane.instrument, Some(Decimal("1.25225")), Some(Decimal("1.25243")), lane.timestamp))
  }
}