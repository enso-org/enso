package org.enso.interpreter.dsl.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import com.oracle.truffle.api.dsl.UnsupportedSpecializationException;
import com.oracle.truffle.api.nodes.Node;
import java.nio.file.Paths;
import java.util.List;
import java.util.function.Supplier;
import java.util.logging.Level;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames.TopScope;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Most of the exceptions thrown by the builtin methods, generated by {@link
 * org.enso.interpreter.dsl.MethodProcessor}, should not be caught. Only a specific subset of
 * exceptions, that we can handle, should be caught, and converted to either a {@link
 * PanicException}, or to {@link DataflowError}.
 *
 * <p>These tests checks this contract.
 */
public class ThrowableCatchTest {
  static List<Supplier<RuntimeException>> exceptionSuppliers =
      List.of(
          () -> new RuntimeException("First exception"),
          () -> new IllegalStateException("Illegal state"),
          () -> new CustomException(new PanicException("Panic", null)),
          () -> new UnsupportedSpecializationException(null, new Node[] {null}, 42));

  private static List<Class<?>> shouldBeHandledExceptionTypes =
      List.of(UnsupportedSpecializationException.class);

  static List<Supplier<Error>> errorSuppliers =
      List.of(
          () -> new Error("First error"),
          () -> new AssertionError("Assertion error"),
          CustomError::new,
          ThreadDeath::new);

  private Context ctx;
  private EnsoContext ensoCtx;

  private static class CustomException extends RuntimeException {
    CustomException(PanicException panic) {
      super(panic);
    }
  }

  private static class CustomError extends Error {}

  @Before
  public void setup() {
    ctx =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .allowIO(IOAccess.ALL)
            .allowAllAccess(true)
            .logHandler(System.err)
            .option(RuntimeOptions.STRICT_ERRORS, "true")
            .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .build();
    ensoCtx = ctx.getBindings(LanguageInfo.ID).invokeMember(TopScope.LEAK_CONTEXT).asHostObject();
    ctx.initialize(LanguageInfo.ID);
    ctx.enter();
  }

  @After
  public void tearDown() {
    ctx.leave();
    ctx.close();
  }

  @Test
  public void testMostRuntimeExceptionsCanPropagateFromBuiltinMethods() {
    var func = ThrowBuiltinMethodGen.makeFunction(EnsoLanguage.get(null));
    var funcCallTarget = func.getCallTarget();
    var emptyState = ensoCtx.emptyState();
    for (long exceptionSupplierIdx = 0;
        exceptionSupplierIdx < exceptionSuppliers.size();
        exceptionSupplierIdx++) {
      Object self = null;
      Object[] args =
          Function.ArgumentsHelper.buildArguments(
              func,
              null,
              emptyState,
              new Object[] {self, Text.create("exception"), exceptionSupplierIdx});
      try {
        funcCallTarget.call(args);
      } catch (Throwable t) {
        var expectedException = exceptionSuppliers.get((int) exceptionSupplierIdx).get();
        if (shouldExceptionBeHandled(t)) {
          expectPanicOrDataflowErrorWithMessage(t, expectedException.getMessage());
        } else {
          assertSameExceptions(
              "Thrown RuntimeException should not be modified in the builtin method",
              expectedException,
              t);
        }
      }
    }
  }

  @Test
  public void testMostErrorsCanPropagateFromBuiltinMethods() {
    var func = ThrowBuiltinMethodGen.makeFunction(EnsoLanguage.get(null));
    var funcCallTarget = func.getCallTarget();
    var emptyState = ensoCtx.emptyState();
    for (long errorSupplierIdx = 0; errorSupplierIdx < errorSuppliers.size(); errorSupplierIdx++) {
      Object self = null;
      Object[] args =
          Function.ArgumentsHelper.buildArguments(
              func, null, emptyState, new Object[] {self, Text.create("error"), errorSupplierIdx});
      try {
        funcCallTarget.call(args);
      } catch (Throwable t) {
        var expectedError = errorSuppliers.get((int) errorSupplierIdx).get();
        if (shouldExceptionBeHandled(t)) {
          expectPanicOrDataflowErrorWithMessage(t, expectedError.getMessage());
        } else {
          assertSameExceptions(
              "Thrown RuntimeException should not be modified in the builtin method",
              expectedError,
              t);
        }
      }
    }
  }

  private void assertSameExceptions(String msg, Throwable expected, Throwable actual) {
    assertEquals(msg, expected.getClass(), actual.getClass());
    assertEquals(msg, expected.getMessage(), actual.getMessage());
  }

  private static boolean shouldExceptionBeHandled(Throwable t) {
    return shouldBeHandledExceptionTypes.stream()
        .anyMatch(exceptionType -> exceptionType.isInstance(t));
  }

  private void expectPanicOrDataflowErrorWithMessage(Throwable t, String expectedMsg) {
    if (t instanceof PanicException panic) {
      assertEquals(expectedMsg, panic.getMessage());
    } else if (t instanceof DataflowError dataflowError) {
      assertEquals(expectedMsg, dataflowError.getMessage());
    } else {
      fail("Should throw only PanicException or DataflowError: " + t.getMessage());
    }
  }
}
