package org.enso.interpreter.runtime.data.text;

import com.ibm.icu.text.Normalizer2;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.strings.TruffleString.Encoding;
import java.util.ArrayDeque;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.builtin.BuiltinObject;
import org.enso.polyglot.common_utils.Core_Text_Utils;

/** Runtime representation of Enso's Text. */
@ExportLibrary(InteropLibrary.class)
public final class Text extends BuiltinObject {
  private static final Text EMPTY = new Text("");
  private Object contents;
  private int length = -1;
  private byte fcdNormalized;

  private Text(String string) {
    assert string != null;
    this.contents = string;
  }

  private Text(ConcatRope contents) {
    assert contents != null;
    this.contents = contents;
  }

  @Override
  protected String builtinName() {
    return "Text";
  }

  @Builtin.Method(
      description =
          """
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
      return computeAndSetLength();
    } else {
      return l;
    }
  }

  @Builtin.Method(
      description =
          """
          Checks whether this text is in FCD normalized form.

          > Example
            Check if the string is normalized

                "14.95€".is_normalized
          """)
  public boolean is_normalized() {
    return switch (fcdNormalized) {
      case 1 -> true;
      case -1 -> false;
      case 0 -> computeAndSetFcd();
      default -> false;
    };
  }

  @CompilerDirectives.TruffleBoundary
  private boolean computeAndSetFcd() {
    var normalizer = Normalizer2.getNFDInstance();
    var isNormalized = normalizer.isNormalized(toString());
    fcdNormalized = (byte) (isNormalized ? 1 : -1);
    return isNormalized;
  }

  public static Text empty() {
    return EMPTY;
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
  TruffleString asTruffleString(@Cached TruffleString.FromJavaStringNode fromJavaStringNode) {
    return fromJavaStringNode.execute(toString(), Encoding.UTF_16);
  }

  @CompilerDirectives.TruffleBoundary
  private int computeAndSetLength() {
    var l = Core_Text_Utils.computeGraphemeLength(toString());
    length = l;
    return l;
  }

  @Override
  @ExportMessage.Ignore
  public Object toDisplayString(boolean allowSideEffects) {
    return toDisplayString(allowSideEffects, ToJavaStringNode.getUncached());
  }

  @CompilerDirectives.TruffleBoundary
  @ExportMessage
  String toDisplayString(
      boolean allowSideEffects,
      @Cached("build()") @Cached.Shared("strings") ToJavaStringNode toJavaStringNode) {
    String str = toJavaStringNode.execute(this);
    return Core_Text_Utils.prettyPrint(str);
  }

  @Override
  public String toString() {
    return switch (this.contents) {
      case String s -> s;
      case ConcatRope r -> flattenAndSetContent(r);
      case null, default -> throw new NullPointerException();
    };
  }

  /**
   * Converts text to a Java String. For use outside of Truffle Nodes.
   *
   * @param c the content to flatten
   * @return the result of conversion.
   */
  @CompilerDirectives.TruffleBoundary
  private String flattenAndSetContent(Object c) {
    var workStack = new ArrayDeque<Object>();
    StringBuilder bldr = new StringBuilder();
    workStack.push(c);
    while (!workStack.isEmpty()) {
      switch (workStack.pop()) {
        case String s -> bldr.append(s);
        case ConcatRope rope -> {
          workStack.push(rope.right());
          workStack.push(rope.left());
        }
        case null, default -> throw new NullPointerException();
      }
    }
    var result = bldr.toString();
    assert length == -1 || length == result.length();
    this.contents = result;
    return result;
  }

  @Override
  public int hashCode() {
    int hash = 7 * toString().hashCode();
    return hash;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj instanceof Text other) {
      return this.toString().equals(other.toString());
    }
    return false;
  }

  private record ConcatRope(Object left, Object right) {}
}
