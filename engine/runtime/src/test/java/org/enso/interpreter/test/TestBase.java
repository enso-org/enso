package org.enso.interpreter.test;

import static org.junit.Assert.assertNotNull;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import java.io.ByteArrayOutputStream;
import java.nio.file.Paths;
import java.util.Map;
import java.util.concurrent.Callable;
import org.enso.interpreter.EnsoLanguage;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;

public abstract class TestBase {
  protected static Context createDefaultContext() {
    var context =
        Context.newBuilder("enso")
            .allowExperimentalOptions(true)
            .allowIO(true)
            .allowAllAccess(true)
            .logHandler(new ByteArrayOutputStream())
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .build();
    final Map<String, Language> langs = context.getEngine().getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));
    return context;
  }

  /**
   * Executes the given callable in the given context. A necessity for executing artificially
   * created Truffle ASTs.
   *
   * @return Object returned from {@code callable} wrapped in {@link Value}.
   */
  protected static Value executeInContext(Context ctx, Callable<Object> callable) {
    // Force initialization of the context
    ctx.eval("enso", "42");
    ctx.getPolyglotBindings()
        .putMember(
            "testSymbol",
            (ProxyExecutable)
                (Value... args) -> {
                  try {
                    return callable.call();
                  } catch (Exception e) {
                    throw new AssertionError(e);
                  }
                });
    return ctx.getPolyglotBindings().getMember("testSymbol").execute();
  }

  /**
   * Unwraps the `receiver` field from the Value. This is a hack to allow us to test execute methods
   * of artificially created ASTs, e.g., single nodes.
   *
   * <p>Does something similar to what {@link
   * com.oracle.truffle.tck.DebuggerTester#getSourceImpl(Source)} does, but uses a different hack
   * than reflective access.
   */
  protected static Object unwrapValue(Context ctx, Value value) {
    var unwrapper = new Unwrapper();
    var unwrapperValue = ctx.asValue(unwrapper);
    unwrapperValue.execute(value);
    assertNotNull(unwrapper.args);
    return unwrapper.args[0];
  }

  /**
   * An artificial RootNode. Used for tests of nodes that need to be adopted. Just create this root
   * node inside a context, all the other nodes, and insert them via {@link
   * #insertChildren(Node...)}.
   */
  static class TestRootNode extends RootNode {
    TestRootNode() {
      super(EnsoLanguage.get(null));
    }

    void insertChildren(Node... children) {
      for (Node child : children) {
        insert(child);
      }
    }

    /** In the tests, do not execute this root node, but execute directly the child nodes. */
    @Override
    public Object execute(VirtualFrame frame) {
      throw new AssertionError("should not reach here");
    }
  }

  @ExportLibrary(InteropLibrary.class)
  static final class Unwrapper implements TruffleObject {
    Object[] args;

    @ExportMessage
    Object execute(Object[] args) {
      this.args = args;
      return this;
    }

    @ExportMessage
    boolean isExecutable() {
      return true;
    }
  }
}
