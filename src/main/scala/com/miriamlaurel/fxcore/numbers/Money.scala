package com.miriamlaurel.fxcore.numbers

import com.miriamlaurel.fxcore.{CurrencyAsset, Asset}

/**
 * @author Alexander Temerev
 */
sealed abstract class Money extends Ordered[Money] {
  def amount: Decimal
  def +(that: Money): Money
  def -(that: Money): Money
  def *(that: Decimal): Money
  def /(that: Decimal): Money
  def setScale(scale: Int): Money
  def unary_- : Money
  def abs: Money
}

case object Zilch extends Money {
  val amount: Decimal = 0
  def /(that: Decimal) = Zilch
  def *(that: Decimal) = Zilch
  def -(that: Money) = -that
  def +(that: Money) = that
  def setScale(scale: Int) = Zilch
  def abs = Zilch
  def unary_- = Zilch
  def compare(that: Money) = that match {
    case Zilch => 0
    case m: Monetary => if (m.amount > 0) -1 else 1
  }

  override def toString = "0"
}

case class Monetary(amount: Decimal, asset: Asset) extends Money {

  def +(that: Money) = that match {
    case Zilch => this
    case m: Monetary => {
      require(this.asset == m.asset)
      val sum = this.amount + m.amount
      if (sum == 0) Zilch else new Monetary(sum, asset)
    }
  }

  def -(that: Money) = that match {
    case Zilch => this
    case m: Monetary => {
      require(this.asset == m.asset)
      val diff = this.amount - m.amount
      if (diff == null) Zilch else new Monetary(diff, asset)
    }
  }

  def *(that: Decimal) = if (that == 0) Zilch else new Monetary(amount * that, asset)

  def /(that: Decimal) = new Monetary(this.amount / that, asset)

  def setScale(scale: Int) = Money(amount.setScale(scale), asset)

  def unary_- = new Monetary(-amount, asset)

  def abs = new Monetary(amount.abs, asset)

  def compare(that: Money) = that match {
    case Zilch => if (amount > 0) 1 else -1
    case m: Monetary => {
      require(this.asset == m.asset, "Assets do not match")
      this.amount compare m.amount
    }
  }

  override def toString = amount.toString + " " + asset.toString
}

object Money {
  def apply(amount: Decimal, asset: Asset): Money = if (amount == 0) Zilch else Monetary(amount, asset)
  def apply(s: String):Money = {
    val tokens = s.split(" ")
    apply(Decimal(tokens(0)), CurrencyAsset(tokens(1)))
  }
}