package com.miriamlaurel

import java.text.DecimalFormat

import com.miriamlaurel.fxcore.asset.Currency
import com.miriamlaurel.fxcore.instrument.CurrencyPair
import com.miriamlaurel.fxcore.party.Party

import scala.BigDecimal
import scala.math._

package object fxcore {

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

  val Bitcoin = Currency("BTC")
  val BTC = Bitcoin

  val Litecoin = Currency("LTC")
  val LTC = Litecoin

  val Ether = Currency("ETH")
  val ETH = Ether

  val Dash = Currency("DASH")
  val DASH = Dash

  val Namecoin = Currency("NMC")
  val NMC = Namecoin

  val Stellar = Currency("STR")
  val STR = Stellar

  val EURUSD = CurrencyPair("EUR/USD")
  val GBPUSD = CurrencyPair("GBP/USD")
  val USDJPY = CurrencyPair("USD/JPY")
  val USDCHF = CurrencyPair("USD/CHF")

  val BTCUSD = CurrencyPair("BTC/USD")
  val LTCUSD = CurrencyPair("LTC/USD")
  val ETHUSD = CurrencyPair("ETH/USD")
  val ETHBTC = CurrencyPair("ETH/BTC")


  object Me extends Party("me")

  object Unknown extends Party("???")

}