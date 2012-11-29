package com.miriamlaurel.fxcore

package object currencies {
  val USD = Currency("USD")
  val EUR = Currency("EUR")
  val GBP = Currency("GBP")
  val JPY = Currency("JPY")
  val AUD = Currency("AUD")
  val CHF = Currency("CHF")
  val CAD = Currency("CAD")
  val NZD = Currency("NZD")
  val SEK = Currency("SEK")
  val NOK = Currency("NOK")
}

package object currencypairs {
  val EURUSD = CurrencyPair("EUR/USD")
  val GBPUSD = CurrencyPair("GBP/USD")
  val USDJPY = CurrencyPair("USD/JPY")
  val USDCHF = CurrencyPair("USD/CHF")
}