package org.enso.logging.service.logback.telemetry;

import ch.qos.logback.classic.spi.ILoggingEvent;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Class responsible for transforming {@link ch.qos.logback.classic.spi.ILoggingEvent log event} to
 * {@link com.fasterxml.jackson.databind.node.ObjectNode JSON} payload.
 *
 * <p>The format of the message is expected to be {@code <message> ; <args>}, where {@code
 * <message>} is an arbitrary string and {@code <args>} is a comma-separated list of argument names.
 * The size of the argument names must match the size of {@link ILoggingEvent#getArgumentArray() arg
 * array} passed to the log event.
 *
 * <p>A correct usage is for example: {@code logger.trace("Hello world; name, age", "Alice", 42)}.
 *
 * <p>Note that it is important that all the {@link ILoggingEvent#getArgumentArray() arguments}
 * passed to the log event are {@link java.io.Serializable serializable}.
 */
final class LogFormatter {
  private LogFormatter() {}

  private static final Logger LOGGER = LoggerFactory.getLogger(LogFormatter.class);
  private static final String KIND = "Telemetry";

  /**
   * Transforms the given log event to JSON payload.
   *
   * @param logEvent the log event to transform.
   * @return null if the logevent was in incorrect format.
   */
  static ObjectNode transform(ILoggingEvent logEvent) {
    var items = logEvent.getMessage().split(";");
    if (items.length != 2) {
      LOGGER.warn("Incorrect log message format: {}", logEventToString(logEvent));
      return null;
    }
    var msg = items[0];
    var argsStr = items[1];
    var argNames = new ArrayList<String>();
    for (var argName : argsStr.split(",")) {
      argNames.add(argName.trim());
    }
    if (argNames.size() != logEvent.getArgumentArray().length) {
      LOGGER.warn("Incorrect number of arguments: {}", logEventToString(logEvent));
      return null;
    }
    var payload = new ObjectNode(JsonNodeFactory.instance);
    payload.set("message", TextNode.valueOf(msg));
    payload.set("kind", TextNode.valueOf(KIND));
    var metadata = constructMetadata(logEvent.getArgumentArray(), argNames);
    payload.set("metadata", metadata);
    return payload;
  }

  private static String logEventToString(ILoggingEvent event) {
    return String.format(
        "{loggerName='%s', message='%s', arguments=%s}",
        event.getLoggerName(), event.getMessage(), Arrays.toString(event.getArgumentArray()));
  }

  private static ObjectNode constructMetadata(Object[] args, List<String> argNames) {
    assert args.length == argNames.size();
    var meta = new ObjectNode(JsonNodeFactory.instance);
    for (int i = 0; i < args.length; i++) {
      var argName = argNames.get(i);
      var arg = args[i];
      meta.set(argName, objectToJson(arg));
    }
    return meta;
  }

  private static JsonNode objectToJson(Object obj) {
    return switch (obj) {
      case Long l -> JsonNodeFactory.instance.numberNode(l);
      case Integer i -> JsonNodeFactory.instance.numberNode(i);
      case Double d -> JsonNodeFactory.instance.numberNode(d);
      case String s -> JsonNodeFactory.instance.textNode(s);
      case null -> JsonNodeFactory.instance.nullNode();
      default -> TextNode.valueOf(obj.toString());
    };
  }
}
