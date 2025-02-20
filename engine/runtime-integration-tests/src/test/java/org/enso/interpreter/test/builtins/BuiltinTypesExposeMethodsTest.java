package org.enso.interpreter.test.builtins;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.util.ArrayList;
import java.util.List;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;
import org.enso.interpreter.test.ValuesGenerator;
import org.enso.interpreter.test.ValuesGenerator.Language;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

/**
 * Gathers all the builtin objects from {@link ValuesGenerator}. From their types, gathers all their
 * methods via their {@link org.enso.interpreter.runtime.scope.ModuleScope definition scope} and
 * checks that {@link Value#canInvokeMember(String)} returns true.
 */
@RunWith(Parameterized.class)
public class BuiltinTypesExposeMethodsTest {
  private static Context ctx;

  private final Value type;

  public BuiltinTypesExposeMethodsTest(Value type) {
    this.type = type;
  }

  private static Context ctx() {
    if (ctx == null) {
      ctx = ContextUtils.createDefaultContext();
    }
    return ctx;
  }

  @Parameters(name = "{index}: {0}")
  public static Iterable<Value> generateBuiltinObjects() {
    var valuesGenerator = ValuesGenerator.create(ctx(), Language.ENSO);
    var builtinTypes = new ArrayList<Value>();
    ContextUtils.executeInContext(
        ctx(),
        () -> {
          valuesGenerator.allTypes().stream()
              .filter(
                  val -> {
                    var asType = getType(val);
                    return !shouldSkipType(asType);
                  })
              .forEach(builtinTypes::add);
          return null;
        });
    return builtinTypes;
  }

  private static Type getType(Value object) {
    var unwrapped = ContextUtils.unwrapValue(ctx(), object);
    return TypeOfNode.getUncached().findTypeOrNull(unwrapped);
  }

  @AfterClass
  public static void disposeCtx() {
    if (ctx != null) {
      ctx.close();
      ctx = null;
    }
  }

  @Test
  public void builtinExposeMethods() {
    ContextUtils.executeInContext(
        ctx(),
        () -> {
          assertThat(type, is(notNullValue()));
          var typeDefScope = getType(type).getDefinitionScope();
          var methodsDefinedInScope = typeDefScope.getMethodsForType(getType(type));
          if (methodsDefinedInScope != null) {
            for (var methodInScope : methodsDefinedInScope) {
              var methodName = methodInScope.getName();
              if (methodName.contains(".")) {
                var items = methodName.split("\\.");
                methodName = items[items.length - 1];
              }
              assertThat(
                  "Builtin type " + type + " should have members", type.hasMembers(), is(true));
              assertThat(
                  "Member " + methodName + " should be present",
                  type.hasMember(methodName),
                  is(true));
              assertThat(
                  "Member " + methodName + " should be invocable",
                  type.canInvokeMember(methodName),
                  is(true));
            }
          }
          return null;
        });
  }

  private static boolean shouldSkipType(Type type) {
    if (type == null) {
      return true;
    }
    if (!type.isBuiltin()) {
      return true;
    }
    var builtins = ContextUtils.leakContext(ctx()).getBuiltins();
    var typesToSkip =
        List.of(
            builtins.function(), builtins.dataflowError(), builtins.warning(), builtins.nothing());
    var shouldBeSkipped = typesToSkip.stream().anyMatch(toSkip -> toSkip == type);
    return shouldBeSkipped;
  }
}
