package org.enso.interpreter.runtime.data.text;

import com.ibm.icu.text.BreakIterator;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.util.ArrayDeque;
import java.util.Deque;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import org.enso.interpreter.dsl.Builtin;

/** The main runtime type for Enso's Text. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
public final class Text implements TruffleObject {
  private static final Lock LOCK = new ReentrantLock();
  private volatile Object contents;
  private volatile int length = -1;

  private Text(String string) {
    this.contents = string;
  }

  private Text(ConcatRope contents) {
    this.contents = contents;
  }

  @Builtin.Method(description = """
  Computes the number of characters in the text.

    ! What is a Character?
      A character is defined as an Extended Grapheme Cluster, see Unicode
      Standard Annex 29. This is the smallest unit that still has semantic
      meaning in most text-processing applications.

    > Example
      Getting the length of the string "건반(Korean)".

          "건반(Korean)".length
   """)
  public long length() {
    int l = length;
    if (l == -1) {
      l = computeLength();
      length = l;
    }
    return l;
  }

  /**
   * Wraps a string in an instance of Text.
   *
   * @param string the string to wrap.
   * @return a Text corresponding to the original string.
   */
  public static Text create(String string) {
    return new Text(string);
  }

  /**
   * Creates a new Text by concatenating two other texts.
   *
   * @param t1 the left operand.
   * @param t2 the right operand.
   * @return a Text representing concatenation of t1 and t2.
   */
  public static Text create(Text t1, Text t2) {
    return new Text(new ConcatRope(t1.contents, t2.contents));
  }

  /**
   * Creates a new Text by concatenating a text and a string.
   *
   * @param t1 the left operand.
   * @param t2 the right operand.
   * @return a Text representing concatenation of t1 and t2.
   */
  public static Text create(Text t1, String t2) {
    return new Text(new ConcatRope(t1.contents, t2));
  }

  /**
   * Creates a new Text by concatenating a text and a string.
   *
   * @param t1 the left operand.
   * @param t2 the right operand.
   * @return a Text representing concatenation of t1 and t2.
   */
  public static Text create(String t1, Text t2) {
    return new Text(new ConcatRope(t1, t2.contents));
  }

  /**
   * Creates a new Text by concatenating two strings.
   *
   * @param t1 the left operand.
   * @param t2 the right operand.
   * @return a Text representing concatenation of t1 and t2.
   */
  public static Text create(String t1, String t2) {
    return new Text(new ConcatRope(t1, t2));
  }

  /**
   * Adds a string to this text.
   *
   * @param other the string add.
   * @return the concatenation of this and the requested string.
   */
  public Text add(String other) {
    return new Text(new ConcatRope(this.contents, other));
  }

  /**
   * Adds a text to this text.
   *
   * @param other the text add.
   * @return the concatenation of this and the requested text.
   */
  public Text add(Text other) {
    return new Text(new ConcatRope(this.contents, other.contents));
  }

  @ExportMessage
  boolean isString() {
    return true;
  }

  @ExportMessage
  String asString(@Cached("build()") @Cached.Shared("strings") ToJavaStringNode toJavaStringNode) {
    return toJavaStringNode.execute(this);
  }

  @CompilerDirectives.TruffleBoundary
  private int computeLength() {
    BreakIterator iter = BreakIterator.getCharacterInstance();
    iter.setText(toString());
    int len = 0;
    while (iter.next() != BreakIterator.DONE) {
      len++;
    }
    return len;
  }

  @CompilerDirectives.TruffleBoundary
  @ExportMessage
  String toDisplayString(
      boolean allowSideEffects,
      @Cached("build()") @Cached.Shared("strings") ToJavaStringNode toJavaStringNode) {
    String str = toJavaStringNode.execute(this);
    // TODO This should be more extensible
    String replaced =
        str.replace("'", "\\'")
            .replace("\n", "\\n")
            .replace("\t", "\\t")
            .replace("\u0007", "\\a")
            .replace("\u0008", "\\b")
            .replace("\u000c", "\\f")
            .replace("\r", "\\r")
            .replace("\u000B", "\\v")
            .replace("\u001B", "\\e");
    return "'" + replaced + "'";
  }

  private void setContents(String contents) {
    assert length == -1 || length == contents.length();
    this.contents = contents;
  }

  @Override
  public String toString() {
    Object c = this.contents;
    if (c instanceof String s) {
      return s;
    } else {
      return flattenIfNecessary(this);
    }
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().text();
  }

  /**
   * Converts text to a Java String. For use outside of Truffle Nodes.
   *
   * @param text the text to convert.
   * @return the result of conversion.
   */
  @CompilerDirectives.TruffleBoundary
  private static String flattenIfNecessary(Text text) {
    LOCK.lock();
    String result;
    try {
      Object c = text.contents;
      if (c instanceof String s) {
        result = s;
      } else {
        Deque<Object> workStack = new ArrayDeque<>();
        StringBuilder bldr = new StringBuilder();
        workStack.push(c);
        while (!workStack.isEmpty()) {
          Object item = workStack.pop();
          if (item instanceof String) {
            bldr.append((String) item);
          } else {
            ConcatRope rope = (ConcatRope) item;
            workStack.push(rope.getRight());
            workStack.push(rope.getLeft());
          }
        }
        result = bldr.toString();
        text.setContents(result);
      }
    } finally {
      LOCK.unlock();
    }
    return result;
  }
}
