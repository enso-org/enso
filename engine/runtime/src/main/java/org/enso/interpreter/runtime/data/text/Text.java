package org.enso.interpreter.runtime.data.text;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

@ExportLibrary(InteropLibrary.class)
public class Text implements TruffleObject {
  private static final long UNITIALIZED = -1;

  private volatile Object contents;
  private volatile boolean isFlat;
  private long length = UNITIALIZED;
  private Lock lock = new ReentrantLock();

  private Text(String string) {
    this.contents = string;
    this.isFlat = true;
  }

  private Text(ConcatRope contents) {
    this.contents = contents;
    this.isFlat = false;
  }

  public static Text create(String string) {
    return new Text(string);
  }

  public static Text create(Text t1, Text t2) {
    return new Text(new ConcatRope(t1.contents, t2.contents));
  }

  public static Text create(Text t1, String t2) {
    return new Text(new ConcatRope(t1.contents, t2));
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
    String replaced = str.replace("\"", "\\\"");
    return "\"" + replaced + "\"";
  }

  public boolean isFlat() {
    return isFlat;
  }

  public void setFlat(boolean flat) {
    isFlat = flat;
  }

  public Object getContents() {
    return contents;
  }

  public void setContents(Object contents) {
    this.contents = contents;
  }

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
}
