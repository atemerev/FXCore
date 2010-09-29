package com.miriamlaurel.fxcore.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import com.miriamlaurel.fxcore.numbers.Decimal
import com.miriamlaurel.fxcore.Lane

/**
 * @author Alexander Temerev
 */
class LaneTest extends FunSuite with ShouldMatchers {
  test("trim should leave minimum amount of best offers totalling greater than trim amount") {
    val lane = Lane("1273787999996,EUR/USD,BIDS,1.25208,1000000,1.25212,2000000,1.25213,1000000,1.25215,2000000,1.2522,1000000,ASKS,1.25245,2000000,1.25246,1000000")
    "1273787999996,EUR/USD,BIDS,1.2522,1000000,ASKS,1.25245,2000000" should equal (lane.trim(Decimal(500000)).toString)
  }
}