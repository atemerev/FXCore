package com.miriamlaurel.fxcore

import numbers.Decimal

/**
 * @author Alexander Temerev
 */
sealed abstract class Signal(val refQuote: Quote) extends TimeEvent {
  override val timestamp = refQuote.timestamp
  def side: Int
  def reverse: Signal
  def refPrice: Option[Decimal]
}

case class BuySignal(ref: Quote) extends Signal(ref) {
  override def side = 1
  override def reverse = SellSignal(ref)
  override def toString = "Buy signal @ " + ref + ", " + ref.timestamp
  def refPrice = ref.ask
}
case class SellSignal(ref: Quote) extends Signal(ref) {
  override def side = -1
  override def reverse = BuySignal(ref)
  override def toString = "Sell signal @ " + ref + ", " + ref.timestamp
  def refPrice = ref.bid
}
case class CloseSignal(ref: Quote) extends Signal(ref) {
  override def side = 0
  override def reverse = throw new NoSuchElementException("Can't reverse a close signal")
  override def toString = "Close signal @ " + ref + ", " + ref.timestamp
  def refPrice = None
}
