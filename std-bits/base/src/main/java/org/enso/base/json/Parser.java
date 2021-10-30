package org.enso.base.json;

import java.util.ArrayDeque;
import java.util.Deque;

public class Parser {

  /** An exception thrown when an unexpected token is encountered in JSON. */
  public static class UnexpectedTokenException extends RuntimeException {
    /**
     * Creates a new instance of this error.
     *
     * @param position the position in input where the exception occured.
     * @param expected a description of expected tokens.
     */
    public UnexpectedTokenException(int position, String expected) {
      super("Unexpected token at position " + position + ". Expected " + expected + ".");
    }
  }

  /** An exception thrown when the input ends unexpectedly. */
  public static class UnexpectedEndOfInputException extends RuntimeException {
    /** Creates a new instance of this error. */
    public UnexpectedEndOfInputException() {
      super("Unexpected end of input.");
    }

    /**
     * Creates a new instance of this error.
     *
     * @param expected a description of expected tokens.
     */
    public UnexpectedEndOfInputException(String expected) {
      super("Unexpected end of input. Expected " + expected + ".");
    }
  }

  /**
   * A consumer of parsing events. Called iteratively, whenever one of the events occurs in parsing.
   * An event may either denote a parsed value or a start or end of a new nesting level.
   */
  public interface JsonConsumer {
    void on_start_object();

    void on_key(String name);

    void on_end_object();

    void on_start_array();

    void on_end_array();

    void on_double(double n);

    void on_long(long n);

    void on_string(String str);

    void on_true();

    void on_false();

    void on_null();
  }

  private enum State {
    ANY,
    ARRAY_END_OR_VALUE,
    ARRAY_END_OR_COMMA,
    ARRAY_VALUE,
    OBJECT_KEY_OR_END,
    OBJECT_VALUE,
    OBJECT_END_OR_COMMA,
    OBJECT_KEY
  }

  /**
   * Parses a JSON string, iteratively calling the provided consumer on each JSON event.
   *
   * <p>Note that this parser internally checks the integrity of the parsed JSON, therefore it is
   * guaranteed that no invalid sequences of events can be reported in the consumer. In case a an
   * invalid sequence of characters is reported, an {@link UnexpectedEndOfInputException} or {@link
   * UnexpectedTokenException} is thrown instead.
   *
   * @param jsonString the string to parse.
   * @param consumer the consumer for reported events.
   */
  public static void parse(String jsonString, JsonConsumer consumer) {
    char[] chars = jsonString.toCharArray();
    Deque<State> state = new ArrayDeque<>();
    state.push(State.ANY);
    int position = 0;
    while (!state.isEmpty()) {
      State current = state.pop();
      position = consumeWhiteSpace(chars, position);
      assertInput(chars, position);
      switch (current) {
        case ANY:
          position = consumeAny(chars, position, consumer, state);
          break;
        case ARRAY_END_OR_VALUE:
          position = consumeArrayEndOrValue(chars, position, consumer, state);
          break;
        case ARRAY_END_OR_COMMA:
          position = consumeArrayEndOrComa(chars, position, consumer, state);
          break;
        case ARRAY_VALUE:
          state.push(State.ARRAY_END_OR_COMMA);
          position = consumeAny(chars, position, consumer, state);
          break;
        case OBJECT_KEY_OR_END:
          position = consumeObjectKeyOrEnd(chars, position, consumer, state);
          break;
        case OBJECT_VALUE:
          state.push(State.OBJECT_END_OR_COMMA);
          position = consumeAny(chars, position, consumer, state);
          break;
        case OBJECT_END_OR_COMMA:
          position = consumeObjectEndOrComma(chars, position, consumer, state);
          break;
        case OBJECT_KEY:
          position = consumeObjectKey(chars, position, consumer, state);
          break;
      }
    }
    position = consumeWhiteSpace(chars, position);
    if (position < chars.length) {
      throw new UnexpectedTokenException(position, "end of input");
    }
  }

  private static int consumeObjectEndOrComma(
      char[] chars, int position, JsonConsumer consumer, Deque<State> state) {
    if (chars[position] == '}') {
      consumer.on_end_object();
      position++;
      return position;
    } else if (chars[position] == ',') {
      state.push(State.OBJECT_KEY);
      position++;
      return position;
    }
    throw new UnexpectedTokenException(position, "a comma or a closing brace");
  }

  private static int consumeObjectKey(
      char[] chars, int position, JsonConsumer consumer, Deque<State> state) {
    position = consumeString(chars, position, consumer, true);
    state.push(State.OBJECT_VALUE);
    position = consumeWhiteSpace(chars, position);
    assertInput(chars, position);
    if (chars[position] == ':') {
      position++;
      return position;
    } else {
      throw new UnexpectedTokenException(position, "a colon");
    }
  }

  private static int consumeObjectKeyOrEnd(
      char[] chars, int position, JsonConsumer consumer, Deque<State> state) {
    if (chars[position] == '}') {
      consumer.on_end_object();
      position++;
      return position;
    }
    return consumeObjectKey(chars, position, consumer, state);
  }

  private static int consumeArrayEndOrValue(
      char[] chars, int position, JsonConsumer consumer, Deque<State> state) {
    if (chars[position] == ']') {
      consumer.on_end_array();
      position++;
      return position;
    }
    state.push(State.ARRAY_END_OR_COMMA);
    return consumeAny(chars, position, consumer, state);
  }

  private static int consumeArrayEndOrComa(
      char[] chars, int position, JsonConsumer consumer, Deque<State> state) {
    switch (chars[position]) {
      case ']':
        consumer.on_end_array();
        position++;
        return position;
      case ',':
        state.push(State.ARRAY_VALUE);
        position++;
        return position;
      default:
        throw new UnexpectedTokenException(position, "a comma or a closing bracket");
    }
  }

  private static int consumeAny(
      char[] chars, int position, JsonConsumer consumer, Deque<State> state) {
    switch (chars[position]) {
      case '[':
        consumer.on_start_array();
        position++;
        state.push(State.ARRAY_END_OR_VALUE);
        return position;
      case '{':
        consumer.on_start_object();
        position++;
        state.push(State.OBJECT_KEY_OR_END);
        return position;
      case '"':
        return consumeString(chars, position, consumer, false);
      case '-':
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
        return consumeNumber(chars, position, consumer);
      case 'n':
        return consumeNull(chars, position, consumer);
      case 't':
        return consumeTrue(chars, position, consumer);
      case 'f':
        return consumeFalse(chars, position, consumer);
    }
    throw new UnexpectedTokenException(position, "a start of a JSON value");
  }

  private static int consumeNull(char[] chars, int position, JsonConsumer consumer) {
    if (position + 3 < chars.length) {
      boolean match =
          chars[position] == 'n'
              && chars[position + 1] == 'u'
              && chars[position + 2] == 'l'
              && chars[position + 3] == 'l';
      if (match) {
        consumer.on_null();
        return position + 4;
      }
      throw new UnexpectedTokenException(position, "a null");
    }
    throw new UnexpectedEndOfInputException("a null");
  }

  private static int consumeTrue(char[] chars, int position, JsonConsumer consumer) {
    if (position + 3 < chars.length) {
      boolean match =
          chars[position] == 't'
              && chars[position + 1] == 'r'
              && chars[position + 2] == 'u'
              && chars[position + 3] == 'e';
      if (match) {
        consumer.on_true();
        return position + 4;
      }
      throw new UnexpectedTokenException(position, "a true");
    }
    throw new UnexpectedEndOfInputException("a true");
  }

  private static int consumeFalse(char[] chars, int position, JsonConsumer consumer) {
    if (position + 4 < chars.length) {
      boolean match =
          chars[position] == 'f'
              && chars[position + 1] == 'a'
              && chars[position + 2] == 'l'
              && chars[position + 3] == 's'
              && chars[position + 4] == 'e';
      if (match) {
        consumer.on_false();
        return position + 5;
      }
      throw new UnexpectedTokenException(position, "a false");
    }
    throw new UnexpectedEndOfInputException("a false");
  }

  private static int consumeString(
      char[] chars, int position, JsonConsumer consumer, boolean isKey) {
    if (chars[position] != '"') {
      throw new UnexpectedTokenException(position, "a string");
    }
    position++;
    StringBuilder bldr = new StringBuilder();
    while (position < chars.length) {
      switch (chars[position]) {
        case '"':
          position++;
          if (isKey) {
            consumer.on_key(bldr.toString());
          } else {
            consumer.on_string(bldr.toString());
          }
          return position;
        case '\\':
          position++;
          position = consumeEscape(chars, position, bldr);
          break;
        default:
          bldr.append(chars[position]);
          position++;
      }
    }
    throw new UnexpectedEndOfInputException("a closing quote");
  }

  private static int consumeEscape(char[] chars, int position, StringBuilder builder) {
    if (position >= chars.length) {
      throw new UnexpectedEndOfInputException("an escape sequence");
    }
    switch (chars[position]) {
      case '"':
        builder.append('"');
        position++;
        return position;
      case '\\':
        builder.append('\\');
        position++;
        return position;
      case '/':
        builder.append('/');
        position++;
        return position;
      case 'b':
        builder.append('\u0008');
        position++;
        return position;
      case 'f':
        builder.append('\u000C');
        position++;
        return position;
      case 'n':
        builder.append('\n');
        position++;
        return position;
      case 'r':
        builder.append('\r');
        position++;
        return position;
      case 't':
        builder.append('\t');
        position++;
        return position;
      case 'u':
        position++;
        return consumeHexEscape(chars, position, builder);
      default:
        throw new UnexpectedTokenException(position, "a valid escape character");
    }
  }

  private static int consumeHexEscape(char[] chars, int position, StringBuilder builder) {
    if (position + 3 >= chars.length) {
      throw new UnexpectedEndOfInputException("four hexadecimal digits");
    }
    char c = 0;
    for (int i = 0; i < 4; i++) {
      c *= 16;
      char current = Character.toLowerCase(chars[position]);
      if (current >= '0' && current <= '9') {
        c += current - '0';
      } else if (current >= 'a' && current <= 'f') {
        c += 10 + current - 'a';
      } else {
        throw new UnexpectedTokenException(position, "a hexadecimal digit");
      }
      position++;
    }
    builder.append(c);
    return position;
  }

  private static int consumeDigits(char[] chars, int position, StringBuilder bldr) {
    if (position >= chars.length || !Character.isDigit(chars[position])) {
      throw new UnexpectedTokenException(position, "a digit");
    }
    while (position < chars.length) {
      if (Character.isDigit(chars[position])) {
        bldr.append(chars[position]);
        position++;
      } else {
        return position;
      }
    }
    return position;
  }

  private static int consumeNumber(char[] chars, int position, JsonConsumer consumer) {
    StringBuilder bldr = new StringBuilder();
    if (chars[position] == '-') {
      bldr.append('-');
      position++;
    }
    position = consumeDigits(chars, position, bldr);
    if (position < chars.length && chars[position] == '.') {
      bldr.append('.');
      position++;
      position = consumeDigits(chars, position, bldr);
    }
    if (position < chars.length && Character.toLowerCase(chars[position]) == 'e') {
      bldr.append('E');
      position++;
      if (position >= chars.length) {
        throw new UnexpectedEndOfInputException("an exponent");
      }
      if (chars[position] == '+' || chars[position] == '-') {
        bldr.append(chars[position]);
        position++;
      }
      position = consumeDigits(chars, position, bldr);
    }

    String strNum = bldr.toString();

    try {
      consumer.on_long(Long.parseLong(strNum, 10));
    } catch (NumberFormatException e) {
      consumer.on_double(Double.parseDouble(strNum));
    }
    return position;
  }

  private static void assertInput(char[] chars, int position) {
    if (position >= chars.length) {
      throw new UnexpectedEndOfInputException();
    }
  }

  private static int consumeWhiteSpace(char[] chars, int position) {
    while (position < chars.length) {
      switch (chars[position]) {
        case '\t':
        case '\n':
        case '\r':
        case ' ':
          position++;
          break;
        default:
          return position;
      }
    }
    return position;
  }
}
