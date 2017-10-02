package com.miriamlaurel.fxcore.market

import com.miriamlaurel.fxcore.instrument.Instrument
import com.miriamlaurel.fxcore.market.OrderBook.Aggregate
import com.miriamlaurel.fxcore.party.Party
import com.miriamlaurel.fxcore.{SafeDouble, Timestamp}

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

class OrderBook private(val instrument: Instrument,
                        val bids: SortedMap[SafeDouble, Aggregate] = SortedMap()(OrderBook.DESCENDING),
                        val asks: SortedMap[SafeDouble, Aggregate] = SortedMap()(OrderBook.ASCENDING),
                        val byKey: Map[OrderKey, Order] = Map.empty,
                        override val timestamp: Long) extends Timestamp {

  lazy val bestBid: Option[SafeDouble] = for (h <- bids.headOption) yield h._1
  lazy val bestAsk: Option[SafeDouble] = for (h <- asks.headOption) yield h._1
  lazy val best = Quote(instrument, bestBid, bestAsk)

  lazy val collapsed = OrderBook(bids.values.map(_.collapse) ++ asks.values.map(_.collapse), timestamp)

  def isFull: Boolean = bids.nonEmpty && asks.nonEmpty

  def apply(op: OrderOp): OrderBook = op match {
    case AddOrder(order, ts) ⇒ this.addOrder(order, ts)
    case ChangeOrder(key, newAmount, newPrice, ts) ⇒ this.changeOrder(key, newAmount, newPrice, ts)
    case RemoveOrder(key, ts) ⇒ this.removeOrder(key, ts)
    case ReplaceParty(party, partyBook, _, ts) ⇒ this.replaceParty(party, partyBook, ts)
  }

  def addOrder(order: Order, timestamp: Long = System.currentTimeMillis()): OrderBook = {
    require(instrument == order.key.instrument, "Order instrument should match order book instrument")
    val line = if (order.key.side == QuoteSide.Bid) bids else asks
    byKey.get(order.key) match {
      // order with same key exists, need to be replaced
      case Some(existingOrder) ⇒
        val oldPrice = existingOrder.price
        val newPrice = order.price
        val removedOldPrice = line(oldPrice) - existingOrder.key
        val addedNewPrice = if (line.contains(newPrice)) line(newPrice) + order else Aggregate(order.price, order.key.side, order)
        val tmpLine = if (removedOldPrice.isEmpty) line - oldPrice else line + (oldPrice -> removedOldPrice)
        val newLine = tmpLine + (newPrice -> addedNewPrice)
        val newByKey = byKey + (order.key -> order)
        if (order.key.side == QuoteSide.Bid) new OrderBook(instrument, newLine, asks, newByKey, timestamp)
        else new OrderBook(instrument, bids, newLine, newByKey, timestamp)
      // no order with same key; adding new order to the book
      case None ⇒
        val newLine = line.get(order.price) match {
          case Some(orders) ⇒ line + (order.price -> (orders + order))
          case None ⇒ line + (order.price -> Aggregate(order.price, order.key.side, order))
        }
        val newByKey = byKey + (order.key -> order)
        if (order.key.side == QuoteSide.Bid) new OrderBook(instrument, newLine, asks, newByKey, timestamp)
        else new OrderBook(instrument, bids, newLine, newByKey, timestamp)
    }
  }

  def changeOrder(key: OrderKey, newAmount: Option[SafeDouble] = None, newPrice: Option[SafeDouble] = None, timestamp: Long = System.currentTimeMillis()): OrderBook = byKey.get(key) match {
    case Some(oldOrder) =>
      val newOrder = oldOrder
        .copy(amount = newAmount.getOrElse(oldOrder.amount))
        .copy(price = newPrice.getOrElse(oldOrder.price))
      addOrder(newOrder, timestamp)
    case None => this
  }

  def removeOrder(key: OrderKey, timestamp: Long = System.currentTimeMillis()): OrderBook = byKey.get(key) match {
    case Some(order) ⇒
      val line = if (order.key.side == QuoteSide.Bid) bids else asks
      val removedOld = line(order.price) - key
      val newLine = if (removedOld.isEmpty) line - order.price else line + (order.price -> removedOld)
      val newByKey = byKey - key
      if (order.key.side == QuoteSide.Bid) new OrderBook(instrument, newLine, asks, newByKey, timestamp)
      else new OrderBook(instrument, bids, newLine, newByKey, timestamp)
    case None ⇒ this
  }

  def removeOrderById(orderId: String, timestamp: Long = System.currentTimeMillis()): OrderBook = {
    val toRemove = byKey.keys.filter(_.id == orderId)
    toRemove.foldLeft(this)((b: OrderBook, k: OrderKey) ⇒ b removeOrder k)
  }

  def replaceParty(party: Party, theirBook: OrderBook, timestamp: Long = System.currentTimeMillis()): OrderBook = {
    val filteredByParty = theirBook.byKey.filter(party == _._1.party)
    val removed = filteredByParty.keys.foldLeft(this)((b: OrderBook, k: OrderKey) ⇒ b removeOrder k)
    filteredByParty.values.foldLeft(removed)((b: OrderBook, o: Order) ⇒ b addOrder o)
  }

  def diff(prev: OrderBook, changeExisting: Boolean = false): Iterable[OrderOp] = {
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

    val currentBids = this.bids.values.flatMap(_.orders)
    val prevBids = prev.bids.values.flatMap(_.orders)
    val currentAsks = this.asks.values.flatMap(_.orders)
    val prevAsks = prev.asks.values.flatMap(_.orders)
    val bidOps = compare(QuoteSide.Bid, currentBids, prevBids, List.empty)
    val askOps = compare(QuoteSide.Ask, currentAsks, prevAsks, List.empty)
    bidOps ::: askOps
  }

  def subtractOrder(order: Order): OrderBook = {
    val line = if (order.key.side == QuoteSide.Bid) bids else asks
    val (newLine, newByKey) = line.get(order.price) match {
      case Some(agg) =>
        val newAgg = agg - order.amount
        if (newAgg.totalAmount == 0) {
          (line - order.price, byKey - order.key)
        } else {
          (line + (order.price -> newAgg), byKey)
        }
      case None => (line, byKey)
    }
    if (order.key.side == QuoteSide.Bid) new OrderBook(instrument, newLine, asks, newByKey, timestamp) else new OrderBook(instrument, bids, newLine, newByKey, timestamp)
  }

  def subtract(other: OrderBook): OrderBook = other.byKey.values.foldLeft(this)(_.subtractOrder(_))

  @tailrec
  private def slice(side: QuoteSide.Value, amount: SafeDouble, taken: SafeDouble, acc: List[Order], excludeId: Option[String]): List[Order] = {
    val line = if (side == QuoteSide.Bid) bids else asks
    if (line.isEmpty) acc
    else {
      val first = line.head._2.orders.head
      val (newAcc, grabbedAmount) = excludeId match {
        case Some(id) ⇒ if (id == first.key.id) (acc, taken) else (first :: acc, taken + first.amount)
        case None ⇒ (first :: acc, taken + first.amount)
      }
      if (grabbedAmount == amount) {
        newAcc
      } else if (grabbedAmount > amount) {
        val amountDiff = grabbedAmount - amount
        val amended = first.copy(amount = first.amount - amountDiff)
        amended :: acc
      } else {
        (this removeOrder first.key).slice(side, amount, grabbedAmount, first :: acc, excludeId)
      }
    }
  }

  private def slice(side: QuoteSide.Value, amount: SafeDouble, excludeId: Option[String]): List[Order] = slice(side, amount, SafeDouble(0), List.empty, excludeId)

  def slice(side: QuoteSide.Value, amount: SafeDouble): List[Order] = slice(side, amount, SafeDouble(0), List.empty, None)

  def slice(side: QuoteSide.Value, amount: SafeDouble, excludeId: String): List[Order] = slice(side, amount, SafeDouble(0), List.empty, Some(excludeId))

  def trim(amount: SafeDouble): OrderBook = OrderBook(slice(QuoteSide.Bid, amount) ::: slice(QuoteSide.Ask, amount))

  def trimLength(maxSize: Int): OrderBook = {
    val bids = this.bids.take(maxSize).flatMap(_._2.orders)
    val asks = this.asks.take(maxSize).flatMap(_._2.orders)
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

  override def toString: String = instrument.toString + ": " + bids.toSeq.mkString(",") + " | " + asks.toSeq.mkString(",")

  private def weightedAvg(orders: List[Order]): SafeDouble =
    orders.map(order ⇒ order.price * order.amount).foldLeft(SafeDouble(0))(_ + _) / orders.map(_.amount).foldLeft(SafeDouble(0))(_ + _)

  def canEqual(other: Any): Boolean = other.isInstanceOf[OrderBook]

  override def equals(other: Any): Boolean = other match {
    case that: OrderBook =>
      (that canEqual this) &&
        instrument == that.instrument &&
        bids == that.bids &&
        asks == that.asks
    case _ => false
  }

  override def hashCode(): Int = Seq(instrument, bids, asks).##
}

object OrderBook {


  class Aggregate private(val price: SafeDouble, val side: QuoteSide.Value, entries: Map[OrderKey, Order], val totalAmount: SafeDouble) {

    lazy val orders: Iterable[Order] = entries.values

    lazy val collapse: Order = {
      val fst = entries.head
      val key = fst._1
      Order(key, totalAmount, price)
    }

    require(isEmpty || totalAmount > 0)

    def +(newEntry: Order): Aggregate = {

      require(this.price == newEntry.price)
      require(this.side == newEntry.key.side)

      val newTotal = entries.get(newEntry.key) match {
        case Some(existing) ⇒ totalAmount - existing.amount + newEntry.amount
        case None ⇒ totalAmount + newEntry.amount
      }
      new Aggregate(price, side, entries + (newEntry.key -> newEntry), newTotal)
    }

    def -(orderKey: OrderKey): Aggregate = {
      val newEntries = entries - orderKey
      if (newEntries.isEmpty) new Aggregate(price, side, Map.empty, 0)
      else {
        val newAmount = entries.get(orderKey) match {
          case Some(existing) ⇒ totalAmount - existing.amount
          case None ⇒ totalAmount
        }
        new Aggregate(price, side, newEntries, newAmount)
      }
    }

    def -(amount: SafeDouble): Aggregate = {
      val exactMatch = orders.find(_.amount == amount)
      exactMatch match {
        case Some(o) => this - o.key
        case None => orders.find(_.amount > amount) match {
          case Some(largerOrder) => this + largerOrder.copy(amount = largerOrder.amount - amount)
          case None => this
        }
      }
    }

    def isEmpty: Boolean = entries.isEmpty

    def size: Int = entries.size

    override def toString: String = "(" + entries.values.map(_.amount).mkString(",") + ")"

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: Aggregate ⇒ this.orders equals that.orders
      case _ ⇒ false
    }

    override def hashCode(): Int = this.entries.values.hashCode()
  }

  object Aggregate {
    def apply(price: SafeDouble, side: QuoteSide.Value, orders: Order*): Aggregate = {
      val empty = new Aggregate(price, side, Map.empty, 0)
      orders.foldLeft(empty)((agg: Aggregate, ord: Order) ⇒ agg + ord)
    }
  }

  private val ASCENDING = Ordering.by((x: SafeDouble) => x.toDouble)
  private val DESCENDING = ASCENDING.reverse

  def empty(instrument: Instrument, timestamp: Long = System.currentTimeMillis()): OrderBook = new OrderBook(instrument = instrument, timestamp = timestamp)

  def apply(orders: Iterable[Order], timestamp: Long = System.currentTimeMillis()): OrderBook = orders.headOption match {
    case Some(order) ⇒
      val start: OrderBook = empty(order.key.instrument, timestamp)
      orders.foldLeft(start)((a: OrderBook, b: Order) ⇒ a.addOrder(b))
    case None ⇒ throw new IllegalArgumentException("Can't make an order book from an empty list")
  }

  def apply(orders: Order*): OrderBook = apply(orders.toList, System.currentTimeMillis())

  def apply(timestamp: Long, orders: Order*): OrderBook = apply(orders.toList, timestamp)
}
