package org.enso.base.json;

public class Printer {
  /**
   * Escapes a string into an RFC-8259 compliant format.
   *
   * @param string the string to escape
   * @return the original string with special characters escaped.
   */
  public static String json_escape(String string) {
    StringBuilder builder = new StringBuilder();
    builder.append("\"");
    string
        .chars()
        .forEach(
            ch -> {
              switch (ch) {
                case '\\':
                  builder.append("\\\\");
                  break;
                case '\"':
                  builder.append("\\\"");
                  break;
                case '\b':
                  builder.append("\\b");
                  break;
                case '\f':
                  builder.append("\\f");
                  break;
                case '\n':
                  builder.append("\\n");
                  break;
                case '\r':
                  builder.append("\\r");
                  break;
                case '\t':
                  builder.append("\\t");
                  break;
                default:
                  if (ch <= 0x1F) {
                    builder.append(String.format("\\u%08X", ch));
                  } else {
                    builder.append((char) ch);
                  }
              }
            });
    builder.append("\"");
    return builder.toString();
  }
}
