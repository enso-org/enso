package org.enso.logging.service.opensearch;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import org.enso.logging.service.common.ApiMessage;
import org.enso.logging.service.common.LogMessage;

public class OpenSearchLogFormatter {

  private OpenSearchLogFormatter() {}

  public static ApiMessage.Log transform(LogMessage msg) {
    String transformedMsg = msg.message();
    Map<String, Object> metadata;
    if (msg.arguments() == null) {
      metadata = constructMetadata(new Object[] {}, msg.loggerName(), msg.logLevel(), msg.mdc());
    } else {
      var args = msg.arguments();
      var i = 0;
      var done = false;
      while (i < args.length && !done) {
        var arg = args[i];
        var idx = transformedMsg.indexOf("{}");
        if (idx == -1) {
          done = true;
        } else {
          // Ignore's the index of the argument if one makes a mistake of using it
          if (arg != null) {
            transformedMsg =
                transformedMsg.replaceFirst("\\{\\}", Matcher.quoteReplacement(arg.toString()));
          }
          i++;
        }
      }
      var remainingArgs =
          i < args.length ? Arrays.copyOfRange(args, i, args.length) : new Object[] {};
      metadata = constructMetadata(remainingArgs, msg.loggerName(), msg.logLevel(), msg.mdc());
    }
    return ApiMessage.createEngineLog(transformedMsg, metadata);
  }

  private static Map<String, Object> constructMetadata(
      Object[] remainingArgs, String loggerName, String logLevel, Map<String, String> mdc) {
    var meta = new HashMap<String, Object>();
    meta.put("loggerName", loggerName);
    meta.put("logLevel", logLevel);
    if (mdc != null) {
      meta.putAll(mdc);
    }
    for (int i = 0; i < remainingArgs.length; i++) {
      meta.put("extra-arg-" + i, remainingArgs[i]);
    }
    return meta;
  }
}
