package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.text.Text;

import java.io.IOException;
import java.util.stream.Collectors;

@BuiltinMethod(
    type = "Prim_Io",
    name = "get_file",
    description =
        "Takes a text representation of a path and returns a TruffleFile corresponding to it.")
public abstract class GetFileNode extends Node {
  static GetFileNode build() {
    return GetFileNodeGen.create();
  }

  abstract Object execute(Object _this, Text path);

  public static class MyFile {
    public void doThing() {
      System.out.println("foobar");
    }
  }

  @Specialization
  Object doGetFile(
      Object _this,
      Text path,
      @CachedContext(Language.class) Context ctx,
      @Cached("build()") ToJavaStringNode toJavaStringNode) {
    TruffleFile f = ctx.getEnvironment().getPublicTruffleFile(toJavaStringNode.execute(path));
    try {
      System.out.println(f.newBufferedReader().lines().collect(Collectors.joining()));
    } catch (IOException e) {
      e.printStackTrace();
    }
    Object r = ctx.getEnvironment().asGuestValue(f);
    InteropLibrary lib = InteropLibrary.getUncached();
    try {
      System.out.println(lib.getArraySize(lib.getMembers(r)));
    } catch (UnsupportedMessageException e) {
      e.printStackTrace();
    }
    System.out.println(r.getClass());
    return r;
  }
}
