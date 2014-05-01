package com.miriamlaurel.fxcore

import com.miriamlaurel.fxcore.asset.{Currency, AssetClass}

sealed trait Money extends Ordered[Money] {

  val ZERO = BigDecimal(0)
  val ONE = BigDecimal(1)

  def amount: BigDecimal
  def +(that: Money): Money
  def -(that: Money): Money
  def *(that: BigDecimal): Money
  def /(that: BigDecimal): Money
  def setScale(scale: Int): Money
  def unary_- : Money
  def abs: Money
}

case object Zilch extends Money {
  val amount: BigDecimal = ZERO
  def /(that: BigDecimal) = Zilch
  def *(that: BigDecimal) = Zilch
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

case class Monetary(amount: BigDecimal, asset: AssetClass) extends Money {

  def +(that: Money) = that match {
    case Zilch => this
    case m: Monetary =>
      require(this.asset == m.asset)
      val sum = this.amount + m.amount
      if (sum == ZERO) Zilch else Monetary(sum, asset)
  }

  def -(that: Money) = that match {
    case Zilch => this
    case m: Monetary =>
      require(this.asset == m.asset)
      val diff = this.amount - m.amount
      if (diff == null) Zilch else Monetary(diff, asset)
  }

  def *(that: BigDecimal) = if (that == ZERO) Zilch else Monetary(amount * that, asset)

  def /(that: BigDecimal) = Monetary(this.amount / that, asset)

  def setScale(scale: Int) = Money(amount.setScale(scale, BigDecimal.RoundingMode.HALF_EVEN), asset)

  def unary_- = Monetary(-amount, asset)

  def abs = Monetary(amount.abs, asset)

  def compare(that: Money) = that match {
    case Zilch => if (amount > 0) 1 else -1
    case m: Monetary =>
      require(this.asset == m.asset, "Assets do not match")
      this.amount compare m.amount
  }

  override def toString = amount.toString + " " + asset.toString
}

object Money {

  val ZERO = BigDecimal(0)
  val ONE = BigDecimal(1)

  def apply(amount: BigDecimal, asset: AssetClass): Money = if (amount == ZERO) Zilch else Monetary(amount, asset)

  def apply(s: String): Money = {
    val tokens = s.split(" ")
    apply(BigDecimal(tokens(0)), Currency(tokens(1)))
  }
}