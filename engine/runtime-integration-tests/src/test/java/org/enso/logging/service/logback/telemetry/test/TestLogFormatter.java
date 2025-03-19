package org.enso.logging.service.logback.telemetry.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

import com.fasterxml.jackson.databind.node.ObjectNode;
import java.util.Map;
import org.enso.logging.service.logback.telemetry.LogFormatter;
import org.enso.logging.service.logback.telemetry.LogMessage;
import org.junit.Assert;
import org.junit.Test;

public class TestLogFormatter {
  @Test
  public void shouldNotTransformLog_WithIncorrectMessage() {
    var logMessage = createLogMessage("Message - arg={}", 1);
    var json = LogFormatter.transform(logMessage);
    assertThat("was not transformed", json, is(nullValue()));
  }

  @Test
  public void shouldNotTransformLog_WithoutArguments() {
    var logMessage = createLogMessage("Message: arg={}");
    var json = LogFormatter.transform(logMessage);
    assertThat("was not transformed", json, is(nullValue()));
  }

  @Test
  public void shouldNotTransformLog_WithRestrictedMetadata() {
    var logMessage = createLogMessage("Message: type={}", 1);
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
    var logMessage = createLogMessage("Message: arg1={}; arg2={}", 2);
    var json = LogFormatter.transform(logMessage);
    assertThat("was not transformed", json, is(nullValue()));
  }

  @Test
  public void shouldFillLoggerName() {
    var loggerName = "org.enso.telemetry.MyLogger";
    var logMessage = new LogMessage(loggerName, "msg: arg={}", new Object[] {1});
    var json = LogFormatter.transform(logMessage);
    assertThat(json.get("metadata").get("loggerName").asText(), is(loggerName));
  }

  @Test
  public void shouldFillMessage() {
    var logMessage = createLogMessage("Message: arg={}", 1);
    var json = LogFormatter.transform(logMessage);
    assertMessage(json, "Message", Map.of("arg", 1));
  }

  @Test
  public void shouldFillMessage_WithMoreWords() {
    var logMessage = createLogMessage("This message has more words: arg={}", 1);
    var json = LogFormatter.transform(logMessage);
    assertMessage(json, "This message has more words", Map.of("arg", 1));
  }

  @Test
  public void shouldFillArgumentToMetadata() {
    var logMessage = createLogMessage("Message: arg={}", 1);
    var json = LogFormatter.transform(logMessage);
    assertMessage(json, "Message", Map.of("arg", 1));
  }

  @Test
  public void shouldFillMoreArgumentsToMetadata() {
    var logMessage = createLogMessage("Message: arg1={}, arg2={}", 1, 2);
    var json = LogFormatter.transform(logMessage);
    assertMessage(json, "Message", Map.of("arg1", 1, "arg2", 2));
  }

  @Test
  public void shouldFillMoreArgumentsToMetadata_WithWhiteSpacesAroundArguments() {
    var logMessage = createLogMessage("Message: arg1 = {} , arg2  =  {}", 1, 2);
    var json = LogFormatter.transform(logMessage);
    assertMessage(json, "Message", Map.of("arg1", 1, "arg2", 2));
  }

  @Test
  public void shouldFillBooleanArgumentToMetadata() {
    var logMessage = createLogMessage("Message: arg={}", true);
    var json = LogFormatter.transform(logMessage);
    assertMessage(json, "Message", Map.of("arg", true));
  }

  @Test
  public void shouldFillStringArgumentToMetadata() {
    var logMessage = createLogMessage("Message: arg={}", "some string");
    var json = LogFormatter.transform(logMessage);
    assertMessage(json, "Message", Map.of("arg", "some string"));
  }

  private static LogMessage createLogMessage(String message, Object... args) {
    return new LogMessage("org.enso.telemetry.MyLogger", message, args);
  }

  private static void assertMessage(
      ObjectNode json, String message, Map<String, Object> arguments) {
    assertThat(json, is(notNullValue()));
    assertThat("Has message property", json.has("message"), is(notNullValue()));
    assertThat("Has message property", json.get("message").asText(), is(message));
    var meta = json.get("metadata");
    for (var arg : arguments.entrySet()) {
      var argName = arg.getKey();
      assertThat("Has argument " + argName, meta.has(argName), is(true));
      var argValFromJson = meta.get(argName);
      switch (arg.getValue()) {
        case Integer i -> assertThat(argValFromJson.asInt(), is(i));
        case Boolean b -> assertThat(argValFromJson.asBoolean(), is(b));
        case String s -> assertThat(argValFromJson.asText(), is(s));
        default -> Assert.fail("Unsupported argument type: " + arg.getValue());
      }
    }
  }
}
