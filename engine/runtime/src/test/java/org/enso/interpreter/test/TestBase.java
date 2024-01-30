package org.enso.interpreter.test;

import static org.junit.Assert.assertNotNull;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import java.io.OutputStream;
import java.nio.file.Paths;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.logging.Level;
import org.enso.interpreter.EnsoLanguage;
import org.enso.polyglot.MethodNames.Module;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;
import org.graalvm.polyglot.proxy.ProxyExecutable;

public abstract class TestBase {
  protected static Context createDefaultContext() {
    var context = defaultContextBuilder().build();
    final Map<String, Language> langs = context.getEngine().getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));
    return context;
  }

  protected static Context createDefaultContext(OutputStream out) {
    var context = defaultContextBuilder().out(out).build();
    final Map<String, Language> langs = context.getEngine().getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));
    return context;
  }

  protected static Context.Builder defaultContextBuilder(String... languages) {
    return Context.newBuilder(languages)
        .allowExperimentalOptions(true)
        .allowIO(IOAccess.ALL)
        .allowAllAccess(true)
        .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
        .logHandler(System.err)
        .option(RuntimeOptions.STRICT_ERRORS, "true")
        .option(
            RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
            Paths.get("../../distribution/component").toFile().getAbsolutePath());
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
    var err = new Exception[1];
    ctx.getPolyglotBindings()
        .putMember(
            "testSymbol",
            (ProxyExecutable)
                (Value... args) -> {
                  try {
                    return callable.call();
                  } catch (Exception e) {
                    err[0] = e;
                    return null;
                  }
                });
    var res = ctx.getPolyglotBindings().getMember("testSymbol").execute();
    if (err[0] != null) {
      throw raise(RuntimeException.class, err[0]);
    }
    return res;
  }

  /**
   * Unwraps the `receiver` field from the Value. This is a hack to allow us to test execute methods
   * of artificially created ASTs, e.g., single nodes. More specifically, only unwrapped values are
   * eligible to be passed to node's execute methods, we cannot pass {@link Value} directly to the
   * node's execute methods.
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
   * Creates an Enso value from the given source.
   *
   * @param src One-line assignment into a variable
   * @param imports Imports, may be empty.
   */
  protected static Value createValue(Context ctx, String src, String imports) {
    if (src.lines().count() > 1 || imports == null) {
      throw new IllegalArgumentException("src should have one line, imports must not be null");
    }
    var sb = new StringBuilder();
    sb.append(imports);
    sb.append(System.lineSeparator());
    sb.append("my_var = ").append(src);
    sb.append(System.lineSeparator());
    Value tmpModule = ctx.eval("enso", sb.toString());
    return tmpModule.invokeMember(Module.EVAL_EXPRESSION, "my_var");
  }

  protected static Value createValue(Context ctx, String src) {
    return createValue(ctx, src, "");
  }

  /**
   * Evaluates the given source as if it was in an unnamed module.
   *
   * @param src The source code of the module
   * @return The value returned from the main method of the unnamed module.
   */
  protected static Value evalModule(Context ctx, String src) {
    Value module = ctx.eval(Source.create("enso", src));
    Value assocType = module.invokeMember(Module.GET_ASSOCIATED_TYPE);
    Value mainMethod = module.invokeMember(Module.GET_METHOD, assocType, "main");
    return mainMethod.execute();
  }

  /**
   * Parses the given module and returns a method by the given name from the module.
   *
   * @param moduleSrc Source of the whole module
   * @return Reference to the method.
   */
  protected static Value getMethodFromModule(Context ctx, String moduleSrc, String methodName) {
    Value module = ctx.eval(Source.create("enso", moduleSrc));
    return module.invokeMember(Module.EVAL_EXPRESSION, methodName);
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

  @SuppressWarnings("unchecked")
  private static <E extends Throwable> E raise(Class<E> clazz, Throwable t) throws E {
    throw (E) t;
  }
}
