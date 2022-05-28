package ai.reactivity.fxcore.market

import java.util.UUID

import ai.reactivity.fxcore.{SafeDouble, Timestamp}
import ai.reactivity.fxcore.instrument.Instrument
import ai.reactivity.fxcore.party.Party

trait OrderOp extends Timestamp {
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

case class MatchOrders(takerKey: OrderKey, makerKey: OrderKey, amount: SafeDouble, price: SafeDouble) extends OrderOp {
  require(makerKey.instrument == takerKey.instrument)
  require(makerKey.side != takerKey.side)

  override def instrument: Instrument = takerKey.instrument

  override def party: Party = takerKey.party

  override def id: String = takerKey.id
}