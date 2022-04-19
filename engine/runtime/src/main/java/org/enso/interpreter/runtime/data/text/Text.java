package org.enso.interpreter.runtime.data.text;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/** The main runtime type for Enso's Text. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(MethodDispatchLibrary.class)
public class Text implements TruffleObject {
  private volatile Object contents;
  private volatile boolean isFlat;
  private final Lock lock = new ReentrantLock();

  private Text(String string) {
    this.contents = string;
    this.isFlat = true;
  }

  private Text(ConcatRope contents) {
    this.contents = contents;
    this.isFlat = false;
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

  /** @return true if this text wraps a string literal and does not require any optimization. */
  public boolean isFlat() {
    return isFlat;
  }

  /** @param flat the new value of the isFlat flag. */
  public void setFlat(boolean flat) {
    isFlat = flat;
  }

  /** @return the contents of this text. */
  public Object getContents() {
    return contents;
  }

  /**
   * Sets the contents of this text.
   *
   * @param contents the new contents.
   */
  public void setContents(Object contents) {
    this.contents = contents;
  }

  /** @return the lock required for modification of this text. */
  public Lock getLock() {
    return lock;
  }

  @Override
  public String toString() {
    if (isFlat) {
      return (String) this.contents;
    } else {
      return ToJavaStringNode.inplaceFlatten(this);
    }
  }

  @ExportMessage
  boolean hasFunctionalDispatch() {
    return true;
  }

  @ExportMessage
  static class GetFunctionalDispatch {

    static final int CACHE_SIZE = 10;

    @CompilerDirectives.TruffleBoundary
    static Function doResolve(UnresolvedSymbol symbol) {
      Context context = getContext();
      return symbol.resolveFor(context.getBuiltins().text().getText(), context.getBuiltins().any());
    }

    static Context getContext() {
      return Context.get(null);
    }

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedSymbol == symbol",
          "function != null"
        },
        limit = "CACHE_SIZE")
    static Function resolveCached(
        Text _this,
        UnresolvedSymbol symbol,
        @Cached("symbol") UnresolvedSymbol cachedSymbol,
        @Cached("doResolve(cachedSymbol)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(Text _this, UnresolvedSymbol symbol)
        throws MethodDispatchLibrary.NoSuchMethodException {
      Function function = doResolve(symbol);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchMethodException();
      }
      return function;
    }
  }

  @ExportMessage
  public static boolean canConvertFrom(Text receiver) {
    return true;
  }

  @ExportMessage
  public static boolean hasSpecialConversion(Text receiver) {
    return false;
  }

  @ExportMessage
  static class GetConversionFunction {

    static final int CACHE_SIZE = 10;

    @CompilerDirectives.TruffleBoundary
    static Function doResolve(AtomConstructor target, UnresolvedConversion conversion) {
      Context context = getContext();
      return conversion.resolveFor(
          target, context.getBuiltins().text().getText(), context.getBuiltins().any());
    }

    static Context getContext() {
      return Context.get(null);
    }

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedTarget == target",
          "cachedConversion == conversion",
          "function != null"
        },
        limit = "CACHE_SIZE")
    static Function resolveCached(
        Text _this,
        AtomConstructor target,
        UnresolvedConversion conversion,
        @Cached("target") AtomConstructor cachedTarget,
        @Cached("conversion") UnresolvedConversion cachedConversion,
        @Cached("doResolve(cachedTarget, cachedConversion)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(Text _this, AtomConstructor target, UnresolvedConversion conversion)
        throws MethodDispatchLibrary.NoSuchConversionException {
      Function function = doResolve(target, conversion);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchConversionException();
      }
      return function;
    }
  }
}
