package org.enso.syntax2;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import net.java.html.js.JavaScriptBody;

public final class Parser implements AutoCloseable {
  private static Object jsParser;
  public static void init(Object parser) {
    jsParser = parser;
  }

  private Parser() {
  }

  private static long allocState() {
    return 0;
  }

  @JavaScriptBody(args = {"result"}, body = """
  result.free();
  """)
  private static void freeState(Object result) {
  }

  @JavaScriptBody(args = { "parser", "input" }, body = """
  debugger;
  var result = parser.parse_input(input);
  return result;
  """)
  private static Object parseInput(Object parser, String input) {
    throw new IllegalStateException("parseInput");
  }

  @JavaScriptBody(args = { "parser", "result" }, body = """
  return parser.base(result).toString();
  """)
  private static String getLastInputBase(Object parser, Object result) {
    throw new IllegalStateException("getLastInputBase");
  }

  private static long getMetadata(Object parser, Object result) {
    return 0L;
  }

  @JavaScriptBody(args = { "parser", "result" }, body = """
  return parser.ast(result);
  """)
  private static Object ast(Object parser, Object result) {
    throw new IllegalStateException("ast");
  }

  @JavaScriptBody(args = { "ast" }, body = """
  return ast.length;
  """)
  private static int length(Object ast) {
    throw new IllegalStateException("ast");
  }

  @JavaScriptBody(args = { "ast", "index" }, body = """
  return ast[index];
  """)
  private static int at(Object ast, int index) {
    throw new IllegalStateException("at");
  }
  private static void copy(Object ast, byte[] arr) {
    for (int i = 0; i < arr.length; i++) {
        arr[i] = (byte) at(ast, i);
    }
  }

  static long getUuidLow(Object metadata, long codeOffset, long codeLength) {
    return 0L;
  }

  static long getUuidHigh(Object metadata, long codeOffset, long codeLength) {
    return 0L;
  }

  public static Parser create() {
    return new Parser();
  }

  public Tree parse(CharSequence input) {
    var result = parseInput(jsParser, input.toString());
    var base = Long.valueOf(getLastInputBase(jsParser, result));
    var metadata = getMetadata(jsParser, result);
    var data = ast(jsParser, result);
    var len = length(data);
    byte[] arr = new byte[len];
    copy(data, arr);
    var message = new Message(ByteBuffer.wrap(arr), input, base, metadata);
    var tree = Tree.deserialize(message);
    freeState(result);
    return tree;
  }

  @Override
  public void close() {
  }
}
