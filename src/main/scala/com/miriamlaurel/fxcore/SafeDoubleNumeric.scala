package com.miriamlaurel.fxcore

object SafeDoubleNumeric extends Fractional[SafeDouble] {
  override def plus(x: SafeDouble, y: SafeDouble): SafeDouble = x + y

  override def minus(x: SafeDouble, y: SafeDouble): SafeDouble = x - y

  override def times(x: SafeDouble, y: SafeDouble): SafeDouble = x * y

  override def div(x: SafeDouble, y: SafeDouble): SafeDouble = x / y

  override def negate(x: SafeDouble): SafeDouble = -x

  override def fromInt(x: Int): SafeDouble = SafeDouble(x)

  override def toInt(x: SafeDouble): Int = x.value.toInt

  override def toLong(x: SafeDouble): Long = x.value.toLong

  override def toFloat(x: SafeDouble): Float = x.value.toFloat

  override def toDouble(x: SafeDouble): Double = x.value

  override def compare(x: SafeDouble, y: SafeDouble): Int = x.compare(y)

  override def parseString(str: String): Option[SafeDouble] = str.toDoubleOption.map(SafeDouble.apply)
}
