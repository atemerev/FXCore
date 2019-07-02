package com.miriamlaurel.fxcore.portfolio

import java.util.UUID

import com.miriamlaurel.fxcore.instrument.Instrument
import com.miriamlaurel.fxcore.market.Quote
import com.miriamlaurel.fxcore.{Money, Zilch}

/**
  * A "non-strict" portfolio allows multiple positions for the same instrument. This behavior is consistent with
  * position handling by most market makers.
  */
class NonStrictPortfolio protected(protected val details: Map[Instrument, Map[UUID, Position]]) extends Portfolio {

  def this() = this(Map())

  override lazy val positions: Iterable[Position] = details.flatMap(_._2.values)

  override def positions(instrument: Instrument): Iterable[Position] =
    details.getOrElse(instrument, Map[UUID, Position]()).values

  override def apply(diff: PortfolioDiff): NonStrictPortfolio = {
    var newDetails = details
    for (action <- diff.actions) {
      action match {
        case AddPosition(p) =>
          val byInstrument = newDetails.getOrElse(p.instrument, Map[UUID, Position]())
          require(!(byInstrument contains p.id))
          newDetails = newDetails + (p.instrument -> (byInstrument + (p.id -> p)))
        case RemovePosition(p) =>
          val byInstrument = newDetails.getOrElse(p.instrument, Map[UUID, Position]())
          require(byInstrument contains p.id)
          newDetails = newDetails + (p.instrument -> (byInstrument - p.id))
        case _ => // Ignore
      }
    }
    new NonStrictPortfolio(newDetails)
  }

  /*!
  We can merge positions with same instrument into one. This method return new non-strict portfolio (with
  merged positions) and a diff value containing all changes have been made. Merge operation can produce ("realize")
  profit or loss, which is stored in a diff value as an "adjustment".
   */
  def mergePositions(ids: Set[UUID]): (NonStrictPortfolio, PortfolioDiff) = {
    val toMerge = positions.filter(position => ids.contains(position.id))
    if (toMerge.isEmpty) (this, PortfolioDiff())
    else {
      require(toMerge.map(_.instrument).toSet.size == 1, "Can't merge positions with different instruments")
      val instrument = toMerge.head.instrument
      var merged: Option[Position] = None
      var adjustment: Money = Zilch
      toMerge.foreach(position => {
        merged match {
          case None => merged = Some(position)
          case Some(pos) =>
            val (p, m) = pos.merge(position)
            merged = p
            adjustment = adjustment + m
        }
      })
      var newMap = details(instrument) -- ids
      merged match {
        case Some(position) => newMap = newMap + (position.id -> position)
        case None => // do nothing
      }
      val newDetails = details + (instrument -> newMap)
      val newPortfolio = new NonStrictPortfolio(newDetails)
      val diff = PortfolioDiff(MergePositions(instrument, toMerge.toSet, merged, adjustment))
      (newPortfolio, diff)
    }
  }

  def profitLossFor(instrument: Instrument, quote: Quote): Option[Money] = {
    val pls = positions(instrument).map(_.profitLoss(quote))
    if (pls.exists(_.isEmpty)) None else Some(pls.map(_.get).foldLeft(Zilch: Money)(_ + _))
  }

  override def <<(newPosition: Position): (NonStrictPortfolio, PortfolioDiff) = {
    val oldPosition = newPosition.matching match {
      case None => None
      case Some(uuid) => for (byInstrument <- details.get(newPosition.instrument);
                             p <- byInstrument.get(uuid)) yield p
    }
    val diff = newPosition diff oldPosition
    (this (diff), diff)
  }
}
