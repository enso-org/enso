package org.enso.logging.service.logback.telemetry.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

import org.enso.logging.service.logback.telemetry.LogFormatter;
import org.enso.logging.service.logback.telemetry.LogMessage;
import org.junit.Test;

public class TestLogFormatter {
  @Test
  public void shouldNotTransformLog_WithIncorrectMessage() {
    var logMessage = createLogMessage("Message - arg=1", 1);
    var json = LogFormatter.transform(logMessage);
    assertThat("was not transformed", json, is(nullValue()));
  }

  @Test
  public void shouldNotTransformLog_WithoutArguments() {
    var logMessage = createLogMessage("Message: arg=1");
    var json = LogFormatter.transform(logMessage);
    assertThat("was not transformed", json, is(nullValue()));
  }

  @Test
  public void shouldNotTransformLog_WithRestrictedMetadata() {
    var logMessage = createLogMessage("Message: type=1", 1);
    var json = LogFormatter.transform(logMessage);
    assertThat("was not transformed", json, is(nullValue()));
  }

  @Test
  public void shouldNotTransformLog_WithoutArguments_InMessage() {
    var logMessage = createLogMessage("Message", 1);
    var json = LogFormatter.transform(logMessage);
    assertThat("was not transformed", json, is(nullValue()));
  }

  @Test
  public void shouldNotTransformLog_WithIncorrectArgumentDelimiter() {
    var logMessage = createLogMessage("Message: arg1=1; arg2=2", 1, 2);
    var json = LogFormatter.transform(logMessage);
    assertThat("was not transformed", json, is(nullValue()));
  }

  @Test
  public void shouldFillLoggerName() {
    var loggerName = "org.enso.telemetry.MyLogger";
    var logMessage = new LogMessage(loggerName, "msg: arg=1", new Object[] {1});
    var json = LogFormatter.transform(logMessage);
    assertThat(json.get("metadata").get("loggerName").asText(), is(loggerName));
  }

  @Test
  public void shouldFillMessage() {
    var logMessage = createLogMessage("Message: arg=1", 1);
    var json = LogFormatter.transform(logMessage);
    assertThat(json.get("message").asText(), is("Message"));
  }

  @Test
  public void shouldFillMessage_WithMoreWords() {
    var logMessage = createLogMessage("This message has more words: arg=1", 1);
    var json = LogFormatter.transform(logMessage);
    assertThat(json.get("message").asText(), is("This message has more words"));
  }

  @Test
  public void shouldFillArgumentToMetadata() {
    var logMessage = createLogMessage("Message: arg=1", 1);
    var json = LogFormatter.transform(logMessage);
    assertThat(json.get("metadata").get("arg").asInt(), is(1));
  }

  @Test
  public void shouldFillMoreArgumentsToMetadata() {
    var logMessage = createLogMessage("Message: arg1=1, arg2=2", 1, 2);
    var json = LogFormatter.transform(logMessage);
    assertThat(json.get("metadata").get("arg1").asInt(), is(1));
    assertThat(json.get("metadata").get("arg2").asInt(), is(2));
  }

  @Test
  public void shouldFillMoreArgumentsToMetadata_WithWhiteSpacesAroundArguments() {
    var logMessage = createLogMessage("Message: arg1 = 1 , arg2  =  2", 1, 2);
    var json = LogFormatter.transform(logMessage);
    assertThat(json.get("metadata").get("arg1").asInt(), is(1));
    assertThat(json.get("metadata").get("arg2").asInt(), is(2));
  }

  @Test
  public void shouldFillBooleanArgumentToMetadata() {
    var logMessage = createLogMessage("Message: arg=true", true);
    var json = LogFormatter.transform(logMessage);
    assertThat("was transformed", json, is(notNullValue()));
    assertThat(json.get("metadata").get("arg").asBoolean(), is(true));
  }

  @Test
  public void shouldFillStringArgumentToMetadata() {
    var logMessage = createLogMessage("Message: arg=\"some string\"", "some string");
    var json = LogFormatter.transform(logMessage);
    assertThat("was transformed", json, is(notNullValue()));
    assertThat(json.get("metadata").get("arg").asText(), is("some string"));
  }

  /**
   * When there is a clash between argument value inside message and inside {@link
   * LogMessage#arguments}, the argument from log message has precedence.
   */
  @Test
  public void shouldFillArgument_FromLogEventArgArray() {
    var logMessage = createLogMessage("Message: arg=42", 1);
    var json = LogFormatter.transform(logMessage);
    assertThat("was transformed", json, is(notNullValue()));
    assertThat(json.get("metadata").get("arg").asInt(), is(1));
  }

  private static LogMessage createLogMessage(String message, Object... args) {
    return new LogMessage("org.enso.telemetry.MyLogger", message, args);
  }
}
