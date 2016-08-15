package com.miriamlaurel.fxcore

class SafeDouble(private val dbl: Double) extends AnyVal with Ordered[SafeDouble] {

  def value = dbl

  def /(that: SafeDouble): SafeDouble = SafeDouble.apply(this.dbl / that.dbl)

  def +(that: SafeDouble): SafeDouble = SafeDouble.apply(this.dbl + that.dbl)

  def -(that: SafeDouble): SafeDouble = SafeDouble.apply(this.dbl - that.dbl)

  def *(that: SafeDouble): SafeDouble = SafeDouble.apply(this.dbl * that.dbl)

  def unary_-(): SafeDouble = SafeDouble.apply(-this.dbl)

  def toInt(x: SafeDouble): Int = x.dbl.toInt

  def toLong(x: SafeDouble): Long = x.dbl.toLong

  def toFloat(x: SafeDouble): Float = x.dbl.toFloat

  def toDouble(x: SafeDouble): Double = x.dbl

  // Yes, we can do that!
  override def compare(that: SafeDouble): Int = Ordering.Double.compare(this.dbl, that.dbl)

  // And that!
  override def toString = dbl.toString
}

object SafeDouble {

  import scala.language.implicitConversions
  val DEFAULT_SCALE_FACTOR: Double = 1e8

  def apply(value: Double)(implicit factor: Double = DEFAULT_SCALE_FACTOR): SafeDouble = {
    if (value > Long.MaxValue / factor || value < -Long.MaxValue / factor) new SafeDouble(value)
    else
      new SafeDouble((if (value < 0) value * factor - 0.5 else value * factor + 0.5).toLong / factor)
  }

  implicit def fromDouble(dbl: Double): SafeDouble = SafeDouble.apply(dbl)
}
