package com.miriamlaurel.fxcore.market

import com.miriamlaurel.fxcore.SafeDouble
import com.miriamlaurel.fxcore.instrument.Instrument
import com.miriamlaurel.fxcore.party.Party

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

class OrderBook private(val instrument: Instrument,
                        val bids: SortedMap[SafeDouble, Map[OrderKey, Order]] = SortedMap()(OrderBook.DESCENDING),
                        val asks: SortedMap[SafeDouble, Map[OrderKey, Order]] = SortedMap()(OrderBook.ASCENDING),
                        val byKey: Map[OrderKey, Order] = Map.empty) {

  lazy val bestBid: Option[SafeDouble] = for (h <- bids.headOption) yield h._1
  lazy val bestAsk: Option[SafeDouble] = for (h <- asks.headOption) yield h._1
  lazy val best = Quote(instrument, bestBid, bestAsk)

  def isFull: Boolean = bids.nonEmpty && asks.nonEmpty

  def apply(op: OrderOp): OrderBook = op match {
    case AddOrder(order) ⇒ this.addOrder(order)
    case ChangeOrder(order) ⇒ this.addOrder(order)
    case RemoveOrder(key) ⇒ this removeOrderById key.id
    case ReplaceParty(party, partyBook, _) ⇒ this.replaceParty(party, partyBook)
  }

  def addOrder(order: Order): OrderBook = {
    require(instrument == order.key.instrument, "Order instrument should match order book instrument")
    val line = if (order.key.side == QuoteSide.Bid) bids else asks
    byKey.get(order.key) match {
      // order with same key exists, need to be replaced
      case Some(existingOrder) ⇒
        val oldPrice = existingOrder.price
        val newPrice = order.price
        val removedOldPrice = line(oldPrice) - existingOrder.key
        val addedNewPrice = if (line.contains(newPrice)) line(newPrice) + (order.key -> order) else Map(order.key -> order)
        val tmpLine = if (removedOldPrice.isEmpty) line - oldPrice else line + (oldPrice -> removedOldPrice)
        val newLine = tmpLine + (newPrice -> addedNewPrice)
        val newByKey = byKey + (order.key -> order)
        if (order.key.side == QuoteSide.Bid) new OrderBook(instrument, newLine, asks, newByKey)
        else new OrderBook(instrument, bids, newLine, newByKey)
      // no order with same key; adding new order to the book
      case None ⇒
        val newLine = line.get(order.price) match {
          case Some(orders) ⇒ line + (order.price -> (orders + (order.key -> order)))
          case None ⇒ line + (order.price -> Map(order.key -> order))
        }
        val newByKey = byKey + (order.key -> order)
        if (order.key.side == QuoteSide.Bid) new OrderBook(instrument, newLine, asks, newByKey)
        else new OrderBook(instrument, bids, newLine, newByKey)
    }
  }

  def removeOrder(key: OrderKey): OrderBook = byKey.get(key) match {
    case Some(order) ⇒
      val line = if (order.key.side == QuoteSide.Bid) bids else asks
      val removedOld = line(order.price) - key
      val newLine = if (removedOld.isEmpty) line - order.price else line + (order.price -> removedOld)
      val newByKey = byKey - key
      if (order.key.side == QuoteSide.Bid) new OrderBook(instrument, newLine, asks, newByKey)
      else new OrderBook(instrument, bids, newLine, newByKey)
    case None ⇒ this
  }

  def removeOrderById(orderId: String): OrderBook = {
    val toRemove = byKey.keys.filter(_.id == orderId)
    toRemove.foldLeft(this)((b: OrderBook, k: OrderKey) ⇒ b removeOrder k)
  }

  def replaceParty(party: Party, theirBook: OrderBook): OrderBook = {
    val filteredByParty = theirBook.byKey.filter(party == _._1.party)
    val removed = filteredByParty.keys.foldLeft(this)((b: OrderBook, k: OrderKey) ⇒ b removeOrder k)
    filteredByParty.values.foldLeft(removed)((b: OrderBook, o: Order) ⇒ b addOrder o)
  }

  def diff(prev: OrderBook): Iterable[OrderOp] = {
    @tailrec
    def compare(side: QuoteSide.Value, remainingCurrent: Iterable[Order], remainingPrev: Iterable[Order], acc: List[OrderOp]): List[OrderOp] = {
      if (remainingCurrent.isEmpty && remainingPrev.isEmpty) {
        acc
      } else if (remainingCurrent.isEmpty && remainingPrev.nonEmpty) {
        compare(side, remainingCurrent, remainingPrev.tail, RemoveOrder(remainingPrev.head.key) :: acc)
      } else if (remainingCurrent.nonEmpty && remainingPrev.isEmpty) {
        compare(side, remainingCurrent.tail, remainingPrev, AddOrder(remainingCurrent.head) :: acc)
      } else {
        val current = remainingCurrent.head
        val prev = remainingPrev.head
        if (current.price == prev.price && current.amount == prev.amount) {
          compare(side, remainingCurrent.tail, remainingPrev.tail, acc)
        } else if (current.price == prev.price) {
          compare(side, remainingCurrent.tail, remainingPrev.tail, RemoveOrder(prev.key) :: (AddOrder(current) :: acc))
        } else if (side == QuoteSide.Bid && current.price > prev.price || side == QuoteSide.Ask && current.price < prev.price) {
          compare(side, remainingCurrent.tail, remainingPrev, AddOrder(current) :: acc)
        } else if (side == QuoteSide.Bid && current.price < prev.price || side == QuoteSide.Ask && current.price > prev.price) {
          compare(side, remainingCurrent, remainingPrev.tail, RemoveOrder(prev.key) :: acc)
        } else {
          throw new IllegalStateException("Should not happen: all possible order arrangements in compared books should be accounted for...")
        }
      }
    }

    val currentBids = this.bids.values.flatMap(_.values)
    val prevBids = prev.bids.values.flatMap(_.values)
    val currentAsks = this.asks.values.flatMap(_.values)
    val prevAsks = prev.asks.values.flatMap(_.values)
    val bidOps = compare(QuoteSide.Bid, currentBids, prevBids, List.empty)
    val askOps = compare(QuoteSide.Ask, currentAsks, prevAsks, List.empty)
    bidOps ::: askOps
  }

  @tailrec
  private def slice(side: QuoteSide.Value, amount: SafeDouble, taken: SafeDouble, acc: List[Order], excludeId: Option[String]): List[Order] = {
    val line = if (side == QuoteSide.Bid) bids else asks
    if (line.isEmpty) acc else {
      val first = line.head._2.head._2
      val newAcc = excludeId match {
        case Some(id) ⇒ if (id == first.key.id) acc else first :: acc
        case None ⇒ first :: acc
      }
      if ((taken + first.amount) >= amount) newAcc else (this removeOrder first.key).slice(side, amount, taken + first.amount, first :: acc, excludeId)
    }
  }

  private def slice(side: QuoteSide.Value, amount: SafeDouble, excludeId: Option[String]): List[Order] = slice(side, amount, SafeDouble(0), List.empty, excludeId)

  def slice(side: QuoteSide.Value, amount: SafeDouble): List[Order] = slice(side, amount, SafeDouble(0), List.empty, None)

  def slice(side: QuoteSide.Value, amount: SafeDouble, excludeId: String): List[Order] = slice(side, amount, SafeDouble(0), List.empty, Some(excludeId))

  def trim(amount: SafeDouble): OrderBook = OrderBook(slice(QuoteSide.Bid, amount) ::: slice(QuoteSide.Ask, amount))

  def trimLength(maxSize: Int): OrderBook = {
    val bids = this.bids.take(maxSize).flatMap(_._2.values)
    val asks = this.asks.take(maxSize).flatMap(_._2.values)
    OrderBook(bids ++ asks)
  }

  private def quote(amount: SafeDouble, excludeId: Option[String]): Quote = {
    require(amount >= 0)
    if (amount == SafeDouble(0)) best
    else {
      val sliceBid = slice(QuoteSide.Bid, amount, excludeId)
      val sliceAsk = slice(QuoteSide.Ask, amount, excludeId)
      val bid = if (sliceBid.nonEmpty) Some(weightedAvg(sliceBid)) else None
      val ask = if (sliceAsk.nonEmpty) Some(weightedAvg(sliceAsk)) else None
      Quote(instrument, bid, ask)
    }
  }

  def quote(amount: SafeDouble): Quote = quote(amount, None)
  def quote(amount: SafeDouble, excludeId: String): Quote = quote(amount, Some(excludeId))

  private def quoteSpread(amount: SafeDouble, excludeId: Option[String]): Quote = {
    require(amount >= 0)
    if (amount == SafeDouble(0)) best
    else {
      val sliceBid = slice(QuoteSide.Bid, amount, excludeId)
      val sliceAsk = slice(QuoteSide.Ask, amount, excludeId)
      val bid = if (sliceBid.nonEmpty) Some(sliceBid.head.price) else None
      val ask = if (sliceAsk.nonEmpty) Some(sliceAsk.head.price) else None
      Quote(instrument, bid, ask)
    }
  }

  def quoteSpread(amount: SafeDouble): Quote = quoteSpread(amount, None)
  def quoteSpread(amount: SafeDouble, excludeId: String): Quote = quoteSpread(amount, Some(excludeId))

  override def toString = bids.toString() + " | " + asks.toString()

  private def weightedAvg(orders: List[Order]): SafeDouble =
    orders.map(order ⇒ order.price * order.amount).foldLeft(SafeDouble(0))(_ + _) / orders.map(_.amount).foldLeft(SafeDouble(0))(_ + _)
}

object OrderBook {

  private val ASCENDING = Ordering.by((x: SafeDouble) => x)
  private val DESCENDING = ASCENDING.reverse

  def empty(instrument: Instrument): OrderBook = new OrderBook(instrument)

  def apply(orders: Iterable[Order]): OrderBook = orders.headOption match {
    case Some(order) ⇒
      val start: OrderBook = empty(order.key.instrument)
      orders.foldLeft(start)((a: OrderBook, b: Order) ⇒ a.addOrder(b))
    case None ⇒ throw new IllegalArgumentException("Can't make an order book from an empty list")
  }

  def apply(orders: Order*): OrderBook = apply(orders.toList)
}
