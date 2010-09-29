package com.miriamlaurel.fxcore

import numbers.Decimal

package object analysis {

  type Indicator = Lane => Array[Option[Decimal]]
  type Filter = Decimal => Decimal
  type Adviser = Lane => Option[Signal]
}