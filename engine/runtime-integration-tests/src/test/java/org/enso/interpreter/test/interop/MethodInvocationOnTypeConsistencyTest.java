package org.enso.interpreter.test.interop;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;

import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import java.util.ArrayList;
import java.util.List;
import java.util.function.BiConsumer;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames.Module;
import org.enso.compiler.core.ConstantsNames;
import org.enso.interpreter.runtime.data.Type;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
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

  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();
  private static Value module;
  private static Type anyType;
  private static Type myType;
  private static Object myTypeAtom;
  private static List<AutoCloseable> toClean = new ArrayList<>();

  private final TestArgs testArgs;

  @BeforeClass
  public static void initCtx() {
    module = ctxRule.eval(LanguageInfo.ID, SRC);
    var anyTypeVal = module.invokeMember(Module.EVAL_EXPRESSION, "any_type");
    var myTypeVal = module.invokeMember(Module.EVAL_EXPRESSION, "my_type");
    var myTypeAtomVal = module.invokeMember(Module.EVAL_EXPRESSION, "my_type_atom");
    anyType = (Type) ctxRule.unwrapValue(anyTypeVal);
    myType = (Type) ctxRule.unwrapValue(myTypeVal);
    myTypeAtom = ctxRule.unwrapValue(myTypeAtomVal);
  }

  @AfterClass
  public static void closeCtx() {
    module = null;
    anyType = null;
    myType = null;
    myTypeAtom = null;
    toClean.forEach(
        closable -> {
          try {
            closable.close();
          } catch (Exception ex) {
            throw new IllegalStateException(ex);
          }
        });
  }

  @Parameters(name = "{index}: expression = {0}")
  public static List<TestArgs> testArgs() {
    initCtx();
    var arr =
        List.of(
            new TestArgs(
                new EnsoInvokeArgs("Any.to_display_text My_Type"),
                new InteropInvokeArgs(anyType, ConstantsNames.TO_DISPLAY_TEXT, List.of(myType)),
                (res, msg) -> {
                  assertThat(msg, res.asString(), is("My_Type"));
                }),
            new TestArgs(
                new EnsoInvokeArgs("Any.to_text my_type_atom"),
                new InteropInvokeArgs(anyType, ConstantsNames.TO_TEXT, List.of(myTypeAtom)),
                (res, msg) -> {
                  assertThat(msg, res.asString(), containsString("Cons 1"));
                }),
            new TestArgs(
                new EnsoInvokeArgs("My_Type.to_display_text"),
                new InteropInvokeArgs(myType, ConstantsNames.TO_DISPLAY_TEXT, List.of()),
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

    arr.forEach(toClean::add);
    return arr;
  }

  public MethodInvocationOnTypeConsistencyTest(TestArgs testArgs) {
    this.testArgs = testArgs;
  }

  @Test
  @Ignore("https://github.com/enso-org/enso/pull/12099#issuecomment-2654281345")
  public void methodInvocationViaInterop_IsConsistentWithPureEnso() {
    assertConsistentInvoke(
        testArgs.ensoInvokeArgs, testArgs.interopInvokeArgs, testArgs.resultChecker);
  }

  private void assertConsistentInvoke(
      EnsoInvokeArgs ensoInvokeArgs,
      InteropInvokeArgs interopInvokeArgs,
      BiConsumer<Value, String> resultChecker) {
    var resFromEnso = invokeViaEnso(ensoInvokeArgs);
    resultChecker.accept(resFromEnso, "Result from pure Enso invocation check:");
    var resFromInterop = invokeViaInterop(interopInvokeArgs);
    resultChecker.accept(ctxRule.asValue(resFromInterop), "Result from Interop invocation check:");
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

  private static class InteropInvokeArgs implements AutoCloseable {
    Type receiverType;
    String method;
    List<Object> args;

    private InteropInvokeArgs(Type receiverType, String method, List<Object> args) {
      var someArgIsValue = args.stream().anyMatch(arg -> arg instanceof Value);
      assertThat("All arguments must be passed unwrapped", someArgIsValue, is(false));

      this.receiverType = receiverType;
      this.method = method;
      this.args = args;
    }

    @Override
    public void close() {
      this.receiverType = null;
      this.method = null;
      this.args = null;
    }
  }

  public static class TestArgs implements AutoCloseable {
    EnsoInvokeArgs ensoInvokeArgs;
    InteropInvokeArgs interopInvokeArgs;
    BiConsumer<Value, String> resultChecker;

    TestArgs(
        EnsoInvokeArgs ensoInvokeArgs,
        InteropInvokeArgs interopInvokeArgs,
        BiConsumer<Value, String> resultChecker) {
      this.ensoInvokeArgs = ensoInvokeArgs;
      this.interopInvokeArgs = interopInvokeArgs;
      this.resultChecker = resultChecker;
    }

    @Override
    public void close() {
      this.interopInvokeArgs.close();
      this.ensoInvokeArgs = null;
      this.interopInvokeArgs = null;
      this.resultChecker = null;
    }

    @Override
    public String toString() {
      return ensoInvokeArgs.expr;
    }
  }
}
