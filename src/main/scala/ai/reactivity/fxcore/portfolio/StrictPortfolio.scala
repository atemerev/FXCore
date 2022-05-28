package ai.reactivity.fxcore.portfolio

import ai.reactivity.fxcore.instrument.Instrument

/**
  * A "strict" portfolio doesn't store different positions for the same instrument; only aggregate positions are
  * stored. This behavior is consistent with position handling by most ECNs.
  * *
  * Even more "stricter" portfolio implementation is possible for forex market, which automatically collapses positions
  * by currencies in its instruments' currency pairs; for example, EUR/CHF can be splitted to EUR/USD and USD/CHF
  * positions. This "stricter" portfolio is normally used by banks, but we don't need it at the moment.
  */
class StrictPortfolio protected(val map: Map[Instrument, Position]) extends Portfolio {
  def this() = this(Map())

  lazy val positions = map.values

  def apply(diff: PortfolioDiff): StrictPortfolio = {
    var newMap = map
    for (action <- diff.actions) {
      action match {
        case AddPosition(p) =>
          require(!(newMap contains p.instrument))
          newMap = newMap + (p.instrument -> p)
        case RemovePosition(p) =>
          require(newMap contains p.instrument)
          newMap = newMap - p.instrument
        case _ => // Ignore
      }
    }
    new StrictPortfolio(newMap)
  }

  def <<(newPosition: Position): (StrictPortfolio, PortfolioDiff) = {
    val oldPosition = map.get(newPosition.instrument)
    val diff = newPosition diff oldPosition
    (this (diff), diff)
  }

  def positions(instrument: Instrument): Iterable[Position] = position(instrument) match {
    case Some(pos) => List(pos)
    case None => List()
  }

  /*!
  There can be exactly 0 or 1 positions for each instrument...
   */
  def position(instrument: Instrument): Option[Position] = map.get(instrument)

  def size = map.size
}
