package org.enso.logging.service.opensearch;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;

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
  public void shouldTransformLog_WithASingleArgs() throws JsonProcessingException {
    var logMessage = createLogMessage("Message - arg={}", 1);
    var json = serialize(logMessage);
    assertMessage(json, "Message - arg=1", Map.of());
  }

  @Test
  public void shouldTransformWithoutArguments() throws JsonProcessingException {
    var logMessage = createLogMessage("Message: arg={}");
    var json = serialize(logMessage);
    assertMessage(json, "Message: arg={}", Map.of());
  }

  @Test
  public void shouldTransformLog_WithoutArguments_InMessage() throws JsonProcessingException {
    var logMessage = createLogMessage("Message", 1);
    var json = serialize(logMessage);
    assertMessage(json, "Message", Map.of("extra-arg-0", 1));
  }

  @Test
  public void shouldFillLoggerName() {
    var loggerName = "org.enso.runtime.Engine";
    var logMessage = new LogMessage(loggerName, "msg: arg={}", new Object[] {1});
    var json = serialize(logMessage);
    assertThat(json, containsString("loggerName"));
    assertThat(json, containsString(loggerName));
  }

  @Test
  public void shouldFillLoggerLevel() {
    var loggerName = "org.enso.runtime.Engine";
    var logLevel = "DEBUG";
    var logMessage =
        new LogMessage(
            loggerName, "Test message for org {}", new Object[] {"test-org"}, logLevel, Map.of());
    var json = serialize(logMessage);
    assertThat(json, containsString("logLevel"));
    assertThat(json, containsString(logLevel));
  }

  @Test
  public void shouldIncludeMDCProperties() {
    var loggerName = "org.enso.runtime.Engine";
    var logLevel = "DEBUG";
    var projectIdKey = "project-id";
    var projectIdValue = "0000-1111-2222-3333-4444";
    var logMessage =
        new LogMessage(
            loggerName,
            "Test message for org {}",
            new Object[] {"test-org"},
            logLevel,
            Map.of(projectIdKey, projectIdValue));
    var json = serialize(logMessage);
    assertThat(json, containsString("\"" + projectIdKey + "\":\"" + projectIdValue + "\""));
  }

  @Test
  public void shouldFillMessage() throws JsonProcessingException {
    var logMessage = createLogMessage("Message: arg={}", 1);
    var json = serialize(logMessage);
    assertMessage(json, "Message: arg=1", Map.of());
  }

  @Test
  public void integerArgumentIsInteger_NotText() {
    var logMessage = createLogMessage("Message: arg={}", 42);
    var json = serialize(logMessage);
    assertThat(json, allOf(containsString("42"), not(containsString("\"42\""))));
  }

  private static LogMessage createLogMessage(String message, Object... args) {
    return new LogMessage("org.enso.runtime.Engine", message, args);
  }

  private static String serialize(LogMessage logMessage) {
    var log = OpenSearchLogFormatter.transform(logMessage);
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
