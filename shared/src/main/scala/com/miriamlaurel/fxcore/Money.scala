package com.miriamlaurel.fxcore

import com.miriamlaurel.fxcore.asset.{AssetClass, Currency}

sealed trait Money extends Ordered[Money] {

  def amount: SafeDouble

  def +(that: Money): Money

  def -(that: Money): Money

  def *(that: SafeDouble): Money

  def /(that: SafeDouble): Money

  def unary_- : Money

  def abs: Money
}

case object Zilch extends Money {
  val amount: SafeDouble = 0

  def /(that: SafeDouble) = Zilch

  def *(that: SafeDouble) = Zilch

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

case class Monetary(amount: SafeDouble, asset: AssetClass) extends Money {

  def +(that: Money) = that match {
    case Zilch => this
    case m: Monetary =>
      require(this.asset == m.asset)
      val sum = this.amount + m.amount
      if (sum == 0) Zilch else Monetary(sum, asset)
  }

  def -(that: Money) = that match {
    case Zilch => this
    case m: Monetary =>
      require(this.asset == m.asset)
      val diff = this.amount - m.amount
      if (diff == 0) Zilch else Monetary(diff, asset)
  }

  def *(that: SafeDouble) = if (that == 0) Zilch else Monetary(amount * that, asset)

  def /(that: SafeDouble) = Monetary(this.amount / that, asset)

  def unary_- = Monetary(-amount, asset)

  def abs = Monetary(math.abs(amount), asset)

  def compare(that: Money) = that match {
    case Zilch => if (amount > 0) 1 else -1
    case m: Monetary =>
      require(this.asset == m.asset, "Assets do not match")
      this.amount compare m.amount
  }

  override def toString = amount.toString + " " + asset.toString
}

object Money {

  def apply(amount: SafeDouble, asset: AssetClass): Money = if (amount == 0) Zilch else Monetary(amount, asset)

  def apply(s: String): Money = {
    val tokens = s.split(" ")
    apply(tokens(0).toDouble, Currency(tokens(1)))
  }
}