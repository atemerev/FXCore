package com.miriamlaurel.fxcore.market

import com.miriamlaurel.fxcore.instrument.Instrument
import com.miriamlaurel.fxcore.party.Party

case class OrderKey(party: Party, instrument: Instrument, side: QuoteSide.Value, id: String, execId: Option[String] = None) {

  override def toString: String = "Order %s %s %s (%s)".format(party, instrument, side, id)

  def canEqual(other: Any): Boolean = other.isInstanceOf[OrderKey]

  override def equals(other: Any): Boolean = other match {
    case OrderKey(`party`, `instrument`, `side`, `id`, _) => true
    case _ => false
  }

  override def hashCode(): Int = (party, instrument, side, id).##
}