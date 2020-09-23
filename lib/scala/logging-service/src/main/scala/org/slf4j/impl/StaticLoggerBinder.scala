package org.slf4j.impl

import org.enso.loggingservice.WSLoggerFactory
import org.slf4j.ILoggerFactory
import org.slf4j.spi.LoggerFactoryBinder

class StaticLoggerBinder extends LoggerFactoryBinder {
  private val factory         = new WSLoggerFactory
  private val factoryClassStr = classOf[WSLoggerFactory].getName

  override def getLoggerFactory: ILoggerFactory = factory
  override def getLoggerFactoryClassStr: String = factoryClassStr
}

object StaticLoggerBinder {
  lazy val singleton                   = new StaticLoggerBinder
  def getSingleton: StaticLoggerBinder = singleton
}
