package org.enso.logging.service.telemetry;

import ch.qos.logback.classic.spi.ILoggingEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.enso.logging.service.common.ApiMessage;
import org.enso.logging.service.common.LogMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Class responsible for transforming {@link LogMessage log messages} to {@link
 * com.fasterxml.jackson.databind.node.ObjectNode JSON} payload.
 *
 * <p>The format of the message is expected to be {@code <message> ; <args>}, where {@code
 * <message>} is an arbitrary string and {@code <args>} is a comma-separated list of {@code
 * <argname>=<value>} mapping. The size of the argument names must match the size of {@link
 * ILoggingEvent#getArgumentArray() arg array} passed to the log event.
 *
 * <p>A correct usage is for example: {@code logger.trace("Hello world: name={}, age={}", "Alice",
 * 42)}.
 *
 * <p>Note that it is important that all the {@link ILoggingEvent#getArgumentArray() arguments}
 * passed to the log event are {@link java.io.Serializable serializable}.
 */
public final class TelemetryLogFormatter {
  private TelemetryLogFormatter() {}

  private static final Logger LOGGER = LoggerFactory.getLogger(TelemetryLogFormatter.class);
  private static final Set<String> RESTRICTED_METADATA = Set.of("type", "loggerName");
  private static final String MESSAGE_DELIMITER = ":";
  private static final String ARGS_DELIMITER = ",";
  private static final String ARG_VALUE_DELIMITER = "=";

  /**
   * Transforms the given log event to JSON payload.
   *
   * @param logMessage the log event to transform.
   * @return null if the logevent was in incorrect format.
   */
  public static ApiMessage.Log transform(LogMessage logMessage) {
    var items = logMessage.message().split(MESSAGE_DELIMITER);
    if (items.length != 2) {
      LOGGER.warn("Incorrect log message format: {}", logEventToString(logMessage));
      return null;
    }
    var msg = items[0];
    var argsStr = items[1];
    var arguments = parseArguments(argsStr);
    if (arguments == null || arguments.isEmpty()) {
      LOGGER.warn("Incorrect arguments format: {}", logEventToString(logMessage));
      return null;
    }
    if (arguments.size() != logMessage.arguments().length) {
      LOGGER.warn("Incorrect number of arguments: {}", logEventToString(logMessage));
      return null;
    }
    if (arguments.stream().map(Argument::name).anyMatch(RESTRICTED_METADATA::contains)) {
      LOGGER.warn("Restricted metadata in arguments: {}", logEventToString(logMessage));
      return null;
    }
    var metadata = constructMetadata(logMessage.arguments(), arguments, logMessage);
    return ApiMessage.createTelemetryLog(msg, metadata);
  }

  private static List<Argument> parseArguments(String argsStr) {
    var args = new ArrayList<Argument>();
    for (var argItem : argsStr.split(ARGS_DELIMITER)) {
      var items = argItem.split(ARG_VALUE_DELIMITER);
      if (items.length != 2) {
        LOGGER.warn("Incorrect argument format: {}", argsStr);
        return null;
      }
      var argName = items[0].trim();
      var argValue = items[1].trim();
      args.add(new Argument(argName, argValue));
    }
    return args;
  }

  private static String logEventToString(LogMessage msg) {
    return String.format(
        "{loggerName='%s', message='%s', arguments=%s}",
        msg.loggerName(),
        msg.message(),
        msg.arguments() == null ? "[]" : Arrays.toString(msg.arguments()));
  }

  private static Map<String, Object> constructMetadata(
      Object[] args, List<Argument> arguments, LogMessage logMessage) {
    assert args.length == arguments.size();
    var meta = new HashMap<String, Object>();
    meta.put("loggerName", logMessage.loggerName());
    for (int i = 0; i < args.length; i++) {
      var argName = arguments.get(i).name;
      var argValue = args[i];
      meta.put(argName, argValue);
    }
    return meta;
  }

  /** Argument parsed from the log message. */
  private record Argument(String name, String value) {}
}
