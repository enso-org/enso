package org.enso.interpreter.test.interop;

import static org.enso.test.utils.ContextUtils.createDefaultContext;
import static org.enso.test.utils.ContextUtils.executeInContext;
import static org.enso.test.utils.ContextUtils.unwrapValue;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;

import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import java.util.List;
import java.util.function.BiConsumer;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames.Module;
import org.enso.interpreter.runtime.data.Type;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

/**
 * TODO[pm]: Ignored - https://github.com/enso-org/enso/pull/12099#issuecomment-2654281345
 *
 * <p>Tests consistency between invocation of methods on types via pure enso, and invocation of
 * methods on {@link org.enso.interpreter.runtime.data.Type} via {@link
 * com.oracle.truffle.api.interop.InteropLibrary#invokeMember(Object, String, Object...) interop
 * protocol}.
 */
@RunWith(Parameterized.class)
public final class MethodInvocationOnTypeConsistencyTest {
  private static final String SRC =
      """
      from Standard.Base.Any import all

      type My_Type
          Cons data
          method self = self.data + 2

      any_type = Any
      my_type = My_Type
      my_type_atom = My_Type.Cons 1
      """;

  private static Context ctx;
  private static Value module;
  private static Type anyType;
  private static Type myType;
  private static Object myTypeAtom;

  private final TestArgs testArgs;

  @BeforeClass
  public static void initCtx() {
    if (ctx != null) {
      return;
    }
    ctx = createDefaultContext();
    module = ctx.eval(LanguageInfo.ID, SRC);
    var anyTypeVal = module.invokeMember(Module.EVAL_EXPRESSION, "any_type");
    var myTypeVal = module.invokeMember(Module.EVAL_EXPRESSION, "my_type");
    var myTypeAtomVal = module.invokeMember(Module.EVAL_EXPRESSION, "my_type_atom");
    executeInContext(
        ctx,
        () -> {
          anyType = (Type) unwrapValue(ctx, anyTypeVal);
          myType = (Type) unwrapValue(ctx, myTypeVal);
          myTypeAtom = unwrapValue(ctx, myTypeAtomVal);
          return null;
        });
  }

  @AfterClass
  public static void closeCtx() {
    if (ctx != null) {
      ctx.close();
      ctx = null;
    }
    module = null;
    anyType = null;
    myType = null;
    myTypeAtom = null;
  }

  @Parameters(name = "{index}: expression = {0}")
  public static List<TestArgs> testArgs() {
    initCtx();
    return List.of(
        new TestArgs(
            new EnsoInvokeArgs("Any.to_display_text My_Type"),
            new InteropInvokeArgs(anyType, "to_display_text", List.of(myType)),
            (res, msg) -> {
              assertThat(msg, res.asString(), is("My_Type"));
            }),
        new TestArgs(
            new EnsoInvokeArgs("Any.to_text my_type_atom"),
            new InteropInvokeArgs(anyType, "to_text", List.of(myTypeAtom)),
            (res, msg) -> {
              assertThat(msg, res.asString(), containsString("Cons 1"));
            }),
        new TestArgs(
            new EnsoInvokeArgs("My_Type.to_display_text"),
            new InteropInvokeArgs(myType, "to_display_text", List.of()),
            (res, msg) -> {
              assertThat(msg, res.asString(), is("My_Type"));
            }),
        new TestArgs(
            new EnsoInvokeArgs("My_Type.method my_type_atom"),
            new InteropInvokeArgs(myType, "method", List.of(myTypeAtom)),
            (res, msg) -> {
              assertThat(msg, res.asInt(), is(3));
            }),
        new TestArgs(
            new EnsoInvokeArgs("Any.has_warnings my_type_atom"),
            new InteropInvokeArgs(anyType, "has_warnings", List.of(myTypeAtom)),
            (res, msg) -> {
              assertThat(msg, res.asBoolean(), is(false));
            }),
        new TestArgs(
            new EnsoInvokeArgs("My_Type.has_warnings my_type_atom"),
            new InteropInvokeArgs(myType, "has_warnings", List.of(myTypeAtom)),
            (res, msg) -> {
              assertThat(msg, res.asBoolean(), is(false));
            }));
  }

  public MethodInvocationOnTypeConsistencyTest(TestArgs testArgs) {
    this.testArgs = testArgs;
  }

  @Test
  @Ignore("https://github.com/enso-org/enso/pull/12099#issuecomment-2654281345")
  public void methodInvocationViaInterop_IsConsistentWithPureEnso() {
    executeInContext(
        ctx,
        () -> {
          assertConsistentInvoke(
              testArgs.ensoInvokeArgs, testArgs.interopInvokeArgs, testArgs.resultChecker);
          return null;
        });
  }

  private void assertConsistentInvoke(
      EnsoInvokeArgs ensoInvokeArgs,
      InteropInvokeArgs interopInvokeArgs,
      BiConsumer<Value, String> resultChecker) {
    var resFromEnso = invokeViaEnso(ensoInvokeArgs);
    resultChecker.accept(resFromEnso, "Result from pure Enso invocation check:");
    var resFromInterop = invokeViaInterop(interopInvokeArgs);
    resultChecker.accept(ctx.asValue(resFromInterop), "Result from Interop invocation check:");
  }

  private Object invokeViaInterop(InteropInvokeArgs args) {
    var interop = InteropLibrary.getUncached();
    assertThat(
        "Member " + args.method + " is invocable on " + args.receiverType,
        interop.isMemberInvocable(args.receiverType, args.method),
        is(true));
    var argsArr = args.args.toArray(Object[]::new);
    try {
      return interop.invokeMember(args.receiverType, args.method, argsArr);
    } catch (UnsupportedMessageException
        | ArityException
        | UnknownIdentifierException
        | UnsupportedTypeException e) {
      throw new AssertionError("Unexpected exception: " + e.getMessage(), e);
    }
  }

  private Value invokeViaEnso(EnsoInvokeArgs args) {
    return module.invokeMember(Module.EVAL_EXPRESSION, args.expr);
  }

  /**
   * @param expr Expression to invoke in the module
   */
  private record EnsoInvokeArgs(String expr) {}

  private record InteropInvokeArgs(Type receiverType, String method, List<Object> args) {
    private InteropInvokeArgs {
      var someArgIsValue = args.stream().anyMatch(arg -> arg instanceof Value);
      assertThat("All arguments must be passed unwrapped", someArgIsValue, is(false));
    }
  }

  public record TestArgs(
      EnsoInvokeArgs ensoInvokeArgs,
      InteropInvokeArgs interopInvokeArgs,
      BiConsumer<Value, String> resultChecker) {

    @Override
    public String toString() {
      return ensoInvokeArgs.expr;
    }
  }
}
