package org.enso.logging.service.telemetry;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.List;
import java.util.Map;
import org.enso.logging.service.common.ApiMessage;
import org.enso.logging.service.common.LogMessage;
import org.junit.Assert;
import org.junit.Test;

public class TestLogFormatter {
  @Test
  public void shouldNotTransformLog_WithIncorrectMessage() {
    var logMessage = createLogMessage("Message - arg={}", 1);
    var json = TelemetryLogFormatter.transform(logMessage);
    assertThat("was not transformed", json, is(nullValue()));
  }

  @Test
  public void shouldNotTransformLog_WithoutArguments() {
    var logMessage = createLogMessage("Message: arg={}");
    var json = TelemetryLogFormatter.transform(logMessage);
    assertThat("was not transformed", json, is(nullValue()));
  }

  @Test
  public void shouldNotTransformLog_WithRestrictedMetadata() {
    var logMessage = createLogMessage("Message: type={}", 1);
    var json = TelemetryLogFormatter.transform(logMessage);
    assertThat("was not transformed", json, is(nullValue()));
  }

  @Test
  public void shouldNotTransformLog_WithoutArguments_InMessage() {
    var logMessage = createLogMessage("Message", 1);
    var json = TelemetryLogFormatter.transform(logMessage);
    assertThat("was not transformed", json, is(nullValue()));
  }

  @Test
  public void shouldNotTransformLog_WithIncorrectArgumentDelimiter() {
    var logMessage = createLogMessage("Message: arg1={}; arg2={}", 2);
    var json = TelemetryLogFormatter.transform(logMessage);
    assertThat("was not transformed", json, is(nullValue()));
  }

  @Test
  public void shouldFillLoggerName() {
    var loggerName = "org.enso.telemetry.MyLogger";
    var logMessage = new LogMessage(loggerName, "msg: arg={}", new Object[] {1});
    var json = serialize(logMessage);
    assertThat(json, containsString("loggerName"));
    assertThat(json, containsString(loggerName));
  }

  @Test
  public void shouldFillMessage() throws JsonProcessingException {
    var logMessage = createLogMessage("Message: arg={}", 1);
    var json = serialize(logMessage);
    assertMessage(json, "Message", Map.of("arg", 1));
  }

  @Test
  public void shouldFillMessage_WithMoreWords() throws JsonProcessingException {
    var logMessage = createLogMessage("This message has more words: arg={}", 1);
    var json = serialize(logMessage);
    assertMessage(json, "This message has more words", Map.of("arg", 1));
  }

  @Test
  public void shouldFillArgumentToMetadata() throws JsonProcessingException {
    var logMessage = createLogMessage("Message: arg={}", 1);
    var json = serialize(logMessage);
    assertMessage(json, "Message", Map.of("arg", 1));
  }

  @Test
  public void shouldFillMoreArgumentsToMetadata() throws JsonProcessingException {
    var logMessage = createLogMessage("Message: arg1={}, arg2={}", 1, 2);
    var json = serialize(logMessage);
    assertMessage(json, "Message", Map.of("arg1", 1, "arg2", 2));
  }

  @Test
  public void shouldFillMoreArgumentsToMetadata_WithWhiteSpacesAroundArguments()
      throws JsonProcessingException {
    var logMessage = createLogMessage("Message: arg1 = {} , arg2  =  {}", 1, 2);
    var json = serialize(logMessage);
    assertMessage(json, "Message", Map.of("arg1", 1, "arg2", 2));
  }

  @Test
  public void shouldFillBooleanArgumentToMetadata() throws JsonProcessingException {
    var logMessage = createLogMessage("Message: arg={}", true);
    var json = serialize(logMessage);
    assertMessage(json, "Message", Map.of("arg", true));
  }

  @Test
  public void shouldFillStringArgumentToMetadata() throws JsonProcessingException {
    var logMessage = createLogMessage("Message: arg={}", "some string");
    var json = serialize(logMessage);
    assertMessage(json, "Message", Map.of("arg", "some string"));
  }

  @Test
  public void integerArgumentIsInteger_NotText() {
    var logMessage = createLogMessage("Message: arg={}", 42);
    var json = serialize(logMessage);
    assertThat(json, allOf(containsString("42"), not(containsString("\"42\""))));
  }

  @Test
  public void booleanArgumentIsBoolean_NotText() {
    var logMessage = createLogMessage("Message: arg={}", true);
    var json = serialize(logMessage);
    assertThat(json, allOf(containsString("true"), not(containsString("\"true\""))));
  }

  private static LogMessage createLogMessage(String message, Object... args) {
    return new LogMessage("org.enso.telemetry.MyLogger", message, args);
  }

  private static String serialize(LogMessage logMessage) {
    var log = TelemetryLogFormatter.transform(logMessage);
    var payload = ApiMessage.createPayload(List.of(log));
    return ApiMessage.serializePayload(payload);
  }

  private static void assertMessage(
      String serialized, String message, Map<String, Object> arguments)
      throws JsonProcessingException {
    var objectMapper = new ObjectMapper();
    var json = objectMapper.readTree(serialized);
    assertThat(json, is(notNullValue()));
    var firstLog = json.get("logs").get(0);
    assertThat("Has message property", firstLog.has("message"), is(notNullValue()));
    assertThat("Has message property", firstLog.get("message").asText(), is(message));
    var meta = firstLog.get("metadata");
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
