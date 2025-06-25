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
}
