package com.miriamlaurel.fxcore.market

import com.miriamlaurel.fxcore.instrument.Instrument
import com.miriamlaurel.fxcore.party.Party

case class OrderKey(party: Party, instrument: Instrument, side: QuoteSide.Value, id: String, execId: Option[String] = None) {
  override def toString = "Order %s %s %s (%s)".format(party, instrument, side, id)
}