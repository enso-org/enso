package org.enso.interpreter.runtime.data.text;

import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.strings.TruffleString.Encoding;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.polyglot.common_utils.Core_Text_Utils;

import com.ibm.icu.text.Normalizer2;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/** The main runtime type for Enso's Text. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
public final class Text implements EnsoObject {
  private static final Lock LOCK = new ReentrantLock();
  private volatile Object contents;
  private volatile int length = -1;
  private volatile FcdNormalized fcdNormalized = FcdNormalized.UNKNOWN;

  private enum FcdNormalized {
    YES,
    NO,
    UNKNOWN
  }

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

  @Builtin.Method(description = """
  Checks whether this text is in FCD normalized form.

  > Example
    Check if the string is normalized

        "14.95€".is_normalized
  """)
  @CompilerDirectives.TruffleBoundary
  public boolean is_normalized() {
    switch (fcdNormalized) {
      case YES -> {
        return true;
      }
      case NO -> {
        return false;
      }
      case UNKNOWN -> {
        Normalizer2 normalizer = Normalizer2.getNFDInstance();
        boolean isNormalized = normalizer.isNormalized(toString());
        setFcdNormalized(isNormalized);
        return isNormalized;
      }
    }
    return false;
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

  @ExportMessage
  TruffleString asTruffleString(
      @Cached TruffleString.FromJavaStringNode fromJavaStringNode) {
    return fromJavaStringNode.execute(toString(), Encoding.UTF_16);
  }

  @CompilerDirectives.TruffleBoundary
  private int computeLength() {
    return Core_Text_Utils.computeGraphemeLength(toString());
  }

  @CompilerDirectives.TruffleBoundary
  @ExportMessage
  String toDisplayString(
      boolean allowSideEffects,
      @Cached("build()") @Cached.Shared("strings") ToJavaStringNode toJavaStringNode) {
    String str = toJavaStringNode.execute(this);
    return Core_Text_Utils.prettyPrint(str);
  }

  @ExportMessage
  Type getMetaObject(@CachedLibrary("this") InteropLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().text();
  }

  @ExportMessage
  boolean hasMetaObject() {
    return true;
  }

  private void setContents(String contents) {
    assert length == -1 || length == contents.length();
    this.contents = contents;
  }

  private void setFcdNormalized(boolean flag) {
    if (flag) {
      fcdNormalized = FcdNormalized.YES;
    } else {
      fcdNormalized = FcdNormalized.NO;
    }
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
  Type getType(@CachedLibrary("this") TypesLibrary thisLib, @Cached(value="1") int ignore) {
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
