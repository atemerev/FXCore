package com.miriamlaurel.fxcore.market

import java.util.UUID

import com.miriamlaurel.fxcore.instrument.Instrument
import com.miriamlaurel.fxcore.party.Party

sealed trait OrderOp {
  def instrument: Instrument
  def party: Party
  def id: String
}

case class AddOrder(order: Order) extends OrderOp {
  override def instrument: Instrument = order.key.instrument
  override def party: Party = order.key.party
  override def id = order.key.id
}

case class ChangeOrder(newOrder: Order) extends OrderOp {
  override def instrument: Instrument = newOrder.key.instrument
  override def party: Party = newOrder.key.party
  override def id = newOrder.key.id
}

case class RemoveOrder(orderKey: OrderKey) extends OrderOp {
  override def instrument: Instrument = orderKey.instrument
  override def party: Party = orderKey.party
  override def id = orderKey.id
}

case class ReplaceParty(party: Party, orderBook: OrderBook,
                        override val id: String = UUID.randomUUID().toString) extends OrderOp {
  override def instrument: Instrument = orderBook.instrument
}