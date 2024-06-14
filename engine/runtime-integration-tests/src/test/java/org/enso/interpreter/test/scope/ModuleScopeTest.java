package org.enso.interpreter.test.scope;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.io.IOException;
import org.enso.common.LanguageInfo;
import org.enso.interpreter.runtime.Module;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Source;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class ModuleScopeTest {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  @Test
  public void staticMethodIsRegisteredInModuleScope() throws IOException {
    var mainSrc =
        Source.newBuilder(
                LanguageInfo.ID,
                """
        type My_Type
            Value x
        My_Type.static_method self = self.x
        """,
                "test.enso")
            .build();
    try (var ctx = ContextUtils.createDefaultContext()) {
      var mainMod = ctx.eval(mainSrc);
      var mainRuntimeMod = (Module) ContextUtils.unwrapValue(ctx, mainMod);
      var myType = mainRuntimeMod.getScope().getType("My_Type", true);
      assertThat(myType, is(notNullValue()));
      var staticMethod = mainRuntimeMod.getScope().getMethodForType(myType, "static_method");
      assertThat(staticMethod, is(notNullValue()));
    }
  }

  @Test
  public void moduleMethodIsRegisteredInModuleScope() throws IOException {
    var mainSrc =
        Source.newBuilder(
                LanguageInfo.ID, """
        module_method _ = 42
        """, "test.enso")
            .build();
    try (var ctx = ContextUtils.createDefaultContext()) {
      // ModuleScope is populated in IrToTruffle - at runtime. So we have to evaluate
      // the main module before we inspect the ModuleScope.
      var mainMod = ctx.eval(mainSrc);
      var mainRuntimeMod = (Module) ContextUtils.unwrapValue(ctx, mainMod);
      var assocType = mainRuntimeMod.getScope().getAssociatedType();
      assertThat(assocType, is(notNullValue()));
      var moduleMethod = mainRuntimeMod.getScope().getMethodForType(assocType, "module_method");
      assertThat(moduleMethod, is(notNullValue()));
    }
  }
}
