package org.enso.interpreter.runtime.progress;

import static org.junit.Assert.assertEquals;

import java.lang.System.Logger.Level;
import org.enso.logger.ObservedMessage;
import org.junit.Test;
import org.slf4j.LoggerFactory;

public class ObservedMessageTest {
  @Test
  public void observeAlsoWhatHappensInSystemLogger() throws Exception {
    var slf4j = LoggerFactory.getLogger("my.test.logger");
    var logger = System.getLogger(slf4j.getName());
    var arr =
        ObservedMessage.collect(
            slf4j,
            () -> {
              logger.log(Level.ERROR, "OK");
            });
    assertEquals("One message", 1, arr.size());
  }

  @Test
  public void messageWithoutAnyArgumentsInSystemLogger() throws Exception {
    var slf4j = LoggerFactory.getLogger("my.test.logger");
    var logger = System.getLogger(slf4j.getName());
    var msg = "strange / message with {} various elements";
    var arr =
        ObservedMessage.collect(
            slf4j,
            () -> {
              logger.log(Level.WARNING, msg, (Object[]) null);
            });
    assertEquals("One message", 1, arr.size());
    assertEquals("The right message", msg, arr.get(0).getFormattedMessage());
  }

  @Test
  public void messageWithWrongArgsInSystemLogger() throws Exception {
    var slf4j = LoggerFactory.getLogger("my.test.logger");
    var logger = System.getLogger(slf4j.getName());
    var msg = "strange / message with {0:number} various elements";
    var arr =
        ObservedMessage.collect(
            slf4j,
            () -> {
              logger.log(Level.INFO, msg, "not a number");
            });
    assertEquals("Two messages", 2, arr.size());
    assertEquals(
        "First of all parsing problem is returned",
        org.slf4j.event.Level.WARN,
        arr.get(0).getLevel());
    assertEquals("Then the original message is logged", msg, arr.get(1).getFormattedMessage());
    assertEquals("With its level", org.slf4j.event.Level.INFO, arr.get(1).getLevel());
  }
}
