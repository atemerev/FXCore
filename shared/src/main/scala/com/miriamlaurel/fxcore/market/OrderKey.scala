package com.miriamlaurel.fxcore.market

import com.miriamlaurel.fxcore.instrument.Instrument
import com.miriamlaurel.fxcore.party.Party

class OrderKey(val party: Party, val instrument: Instrument, val side: QuoteSide.Value, val id: String, val execId: Option[String] = None) {

  override def toString = "Order %s %s %s (%s)".format(party, instrument, side, id)

  def canEqual(other: Any): Boolean = other.isInstanceOf[OrderKey]

  override def equals(other: Any): Boolean = other match {
    case that: OrderKey =>
      (that canEqual this) &&
        party == that.party &&
        instrument == that.instrument &&
        side == that.side &&
        id == that.id
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(party, instrument, side, id)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object OrderKey {
  def apply(party: Party, instrument: Instrument, side: QuoteSide.Value, id: String, execId: Option[String] = None) = new OrderKey(party, instrument, side, id, execId)
}