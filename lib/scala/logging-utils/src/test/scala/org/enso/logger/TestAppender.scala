package org.enso.logger

import ch.qos.logback.core.read.ListAppender
import ch.qos.logback.classic.spi.ILoggingEvent

import scala.jdk.CollectionConverters._

class TestAppender extends ListAppender[ILoggingEvent] {

  def size(): Int = {
    this.list.size();
  }

  def allEvents(): List[TestLogMessage] = {
    this.list.asScala.toList.map(event =>
      TestLogMessage(event.getLevel(), event.getFormattedMessage())
    )
  }
}
