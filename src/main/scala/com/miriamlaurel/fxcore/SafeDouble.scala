package com.miriamlaurel.fxcore

/*
 * Idea is taken from OpenHFT:
 *
 * https://github.com/OpenHFT/Java-Lang/blob/master/lang/src/main/java/net/openhft/lang/Maths.java
 */

class SafeDouble(private val dbl: Double) extends AnyVal with Ordered[SafeDouble] {

  def value: Double = dbl

  def /(that: SafeDouble): SafeDouble = SafeDouble.apply(this.dbl / that.dbl)

  def +(that: SafeDouble): SafeDouble = SafeDouble.apply(this.dbl + that.dbl)

  def -(that: SafeDouble): SafeDouble = SafeDouble.apply(this.dbl - that.dbl)

  def *(that: SafeDouble): SafeDouble = SafeDouble.apply(this.dbl * that.dbl)

  def unary_-(): SafeDouble = SafeDouble.apply(-this.dbl)

  def signum: Int = dbl.sign.toInt

  def toInt: Int = dbl.toInt

  def toLong: Long = dbl.toLong

  def toFloat: Float = dbl.toFloat

  def toDouble: Double = dbl

  // Yes, we can do that!
  override def compare(that: SafeDouble): Int = java.lang.Double.compare(this.dbl, that.dbl)

  // And that!
  def ==(that: SafeDouble): Boolean = this.dbl == that.dbl
  def ==(that: Int): Boolean = this.dbl == that.toDouble
  def ==(that: Long): Boolean = this.dbl == that.toDouble

  override def toString: String = if (dbl == dbl.toInt) dbl.toInt.toString else BigDecimal.valueOf(dbl).bigDecimal.stripTrailingZeros.toPlainString
}

object SafeDouble {

  import scala.language.implicitConversions
  val DEFAULT_SCALE_FACTOR: Double = 1e8

  implicit def fromDouble(dbl: Double): SafeDouble = SafeDouble.apply(dbl)
  implicit def toDouble(safe: SafeDouble): Double = safe.toDouble
  implicit def fromInt(int: Int): SafeDouble = SafeDouble.apply(int)
  implicit def fromLong(long: Long): SafeDouble = SafeDouble.apply(long.toDouble)

  implicit val numeric: SafeDoubleNumeric.type = SafeDoubleNumeric

  def apply(value: Double)(implicit factor: Double = DEFAULT_SCALE_FACTOR): SafeDouble = {
    if (value > Long.MaxValue / factor || value < -Long.MaxValue / factor) new SafeDouble(value)
    else
      new SafeDouble((if (value < 0) value * factor - 0.5 else value * factor + 0.5).toLong / factor)
  }

  implicit class Extension(val dbl: Double) extends AnyVal {
    def safe: SafeDouble = SafeDouble.apply(dbl)
  }
}
