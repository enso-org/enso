package org.enso.compiler.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.List;
import java.util.function.Consumer;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames.Module;
import org.enso.compiler.core.ConstantsNames;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.test.utils.ContextUtils;
import org.junit.ClassRule;
import org.junit.Test;

public final class SelfParameterTest {

  @ClassRule public static final ContextUtils ctx = ContextUtils.createDefault();

  @Test
  public void moduleMethod_HasImplicitSelfParameter() {
    var code =
        """
        module_method x = x
        """;
    var method = moduleMethod(code, "module_method");
    assertParameterNames(method.getSchema(), List.of(ConstantsNames.SELF_ARGUMENT, "x"));
  }

  @Test
  public void moduleMethod_CanHaveExplicitSelfParameter() {
    var code =
        """
        module_method self x = x
        """;
    var methodAsFunc = moduleMethod(code, "module_method");
    var schema = methodAsFunc.getSchema();
    assertParameterNames(schema, List.of(ConstantsNames.SELF_ARGUMENT, "x"));
  }

  @Test
  public void staticMethod_HasImplicitSelfParameter() {
    var code =
        """
        type My_Type
            static_method x = x
        """;
    var method = staticMethod(code, "static_method", "My_Type");
    assertParameterNames(method.getSchema(), List.of(ConstantsNames.SELF_ARGUMENT, "x"));
  }

  @Test
  public void staticMethod_CanHaveExplicitSelfParameter() {
    var code =
        """
        type My_Type
            static_method self x = x
        """;
    var method = staticMethod(code, "static_method", "My_Type");
    assertParameterNames(method.getSchema(), List.of(ConstantsNames.SELF_ARGUMENT, "x"));
  }

  private static void assertParameters(
      FunctionSchema schema, List<Consumer<ArgumentDefinition>> paramTests) {
    var paramInfos = schema.getArgumentInfos();
    assertThat("Number of parameters mismatch", paramInfos.length, is(paramTests.size()));
    for (var i = 0; i < paramInfos.length; i++) {
      var paramTest = paramTests.get(i);
      paramTest.accept(paramInfos[i]);
    }
  }

  private static void assertParameterNames(FunctionSchema schema, List<String> expectedNames) {
    assertParameters(
        schema,
        expectedNames.stream()
            .map(
                name ->
                    (Consumer<ArgumentDefinition>)
                        (paramInfo -> assertThat(paramInfo.getName(), is(name))))
            .toList());
  }

  private Function staticMethod(String code, String methodName, String typeName) {
    var mod = ctx.context().eval(LanguageInfo.ID, code);
    var myType = mod.invokeMember(Module.GET_TYPE, typeName);
    var method = mod.invokeMember(Module.GET_METHOD, myType, methodName);
    return (Function) ctx.unwrapValue(method);
  }

  private static Function moduleMethod(String code, String module_method) {
    var mod = ctx.context().eval(LanguageInfo.ID, code);
    var modType = mod.invokeMember(Module.GET_ASSOCIATED_TYPE);
    var method = mod.invokeMember(Module.GET_METHOD, modType, module_method);
    return (Function) ctx.unwrapValue(method);
  }
}
