package com.miriamlaurel.fxcore.numbers

import java.math.MathContext
import com.miriamlaurel.fxcore.{Currency, AssetClass}

/**
 * @author Alexander Temerev
 */
case class Decimal(bd: java.math.BigDecimal) extends BigDecimal(bd, MathContext.DECIMAL64) {

  def this(bd: BigDecimal) = this(bd.bigDecimal.stripTrailingZeros)
  def this(s: String) = this(BigDecimal(s, MathContext.DECIMAL64))
  def this(i: Int) = this(BigDecimal(i))
  def this(l: Long) = this(BigDecimal(l))

  def toPlainString = if (this == 0) "0" else bigDecimal.toPlainString

  def +(that: Decimal): Decimal = Decimal(super.+(that))
  def -(that: Decimal): Decimal = Decimal(super.-(that))
  def *(that: Decimal): Decimal = Decimal(super.*(that))
  def /(that: Decimal): Decimal = Decimal(super./(that))

  def %(that: Decimal): Decimal = Decimal(super.%(that))

  def min(that: Decimal): Decimal = Decimal(super.min(that))
  def max(that: Decimal): Decimal = Decimal(super.max(that))

  override def abs: Decimal = Decimal(super.abs)

  override def setScale(scale: Int): Decimal = Decimal(super.setScale(scale, BigDecimal.RoundingMode.HALF_EVEN))

  def reciprocal: Decimal = Decimal(1) / this

  override def toString() = bd.toPlainString
}

object Decimal {

  implicit object Ord extends Ordering[Decimal] {
    def compare(x: Decimal, y: Decimal) = x compare y
  }

  def apply(bd: BigDecimal) = new Decimal(bd)
  def apply(s: String) = new Decimal(s)
  def apply(i: Int) = new Decimal(i)
  def apply(l: Long) = new Decimal(l)

  implicit def enrichBigDecimal(bd: BigDecimal): Decimal = Decimal(bd)
  implicit def wrapInt(i: Int): Decimal = Decimal(i)
  implicit def wrapLong(l: Long): Decimal = Decimal(l)
  implicit def wrapOptInt(oi: Option[Int]): Option[Decimal] = if (oi.isDefined) Some(Decimal(oi.get)) else None
  implicit def wrapOptLong(ol: Option[Long]): Option[Decimal] = if (ol.isDefined) Some(Decimal(ol.get)) else None
  implicit def wrapString(s: String) = Decimal(s)

  // Implicit unwrapping like Decimal -> Int is dangerous due to precision loss.
}

/**
 * @author Alexander Temerev
 */
sealed trait Money extends Ordered[Money] {
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

case class Monetary(amount: Decimal, asset: AssetClass) extends Money {

  def +(that: Money) = that match {
    case Zilch => this
    case m: Monetary => {
      require(this.asset == m.asset)
      val sum = this.amount + m.amount
      if (sum == 0) Zilch else Monetary(sum, asset)
    }
  }

  def -(that: Money) = that match {
    case Zilch => this
    case m: Monetary => {
      require(this.asset == m.asset)
      val diff = this.amount - m.amount
      if (diff == null) Zilch else Monetary(diff, asset)
    }
  }

  def *(that: Decimal) = if (that == 0) Zilch else Monetary(amount * that, asset)

  def /(that: Decimal) = Monetary(this.amount / that, asset)

  def setScale(scale: Int) = Money(amount.setScale(scale), asset)

  def unary_- = Monetary(-amount, asset)

  def abs = Monetary(amount.abs, asset)

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
  def apply(amount: Decimal, asset: AssetClass): Money = if (amount == 0) Zilch else Monetary(amount, asset)
  def apply(s: String):Money = {
    val tokens = s.split(" ")
    apply(Decimal(tokens(0)), Currency(tokens(1)))
  }
}