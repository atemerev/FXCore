package com.miriamlaurel.fxcore.market

import java.util.UUID

import com.miriamlaurel.fxcore.{SafeDouble, Timestamp}
import com.miriamlaurel.fxcore.instrument.Instrument
import com.miriamlaurel.fxcore.party.Party

sealed trait OrderOp extends Timestamp {
  def instrument: Instrument
  def party: Party
  def id: String
}

case class AddOrder(order: Order, override val timestamp: Long = System.currentTimeMillis()) extends OrderOp {
  override def instrument: Instrument = order.key.instrument
  override def party: Party = order.key.party
  override def id = order.key.id
}

case class ChangeOrder(orderKey: OrderKey, newAmount: Option[SafeDouble] = None, newPrice: Option[SafeDouble] = None, override val timestamp: Long = System.currentTimeMillis()) extends OrderOp {
  override def instrument: Instrument = orderKey.instrument
  override def party: Party = orderKey.party
  override def id = orderKey.id
}

case class RemoveOrder(orderKey: OrderKey, override val timestamp: Long = System.currentTimeMillis()) extends OrderOp {
  override def instrument: Instrument = orderKey.instrument
  override def party: Party = orderKey.party
  override def id = orderKey.id
}

case class ReplaceParty(party: Party,
                        orderBook: OrderBook,
                        override val id: String = UUID.randomUUID().toString,
                        override val timestamp: Long = System.currentTimeMillis()) extends OrderOp {
  override def instrument: Instrument = orderBook.instrument
}