package org.enso.interpreter.caches;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.enso.compiler.CompilerTest;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.test.TestBase;
import org.enso.polyglot.CompilationStage;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.junit.BeforeClass;
import org.junit.Test;

public class ModuleCacheTest extends TestBase {
  private static Context ctx;

  public ModuleCacheTest() {}

  @BeforeClass
  public static void initializeContext() throws Exception {
    ctx = defaultContextBuilder().option(RuntimeOptions.DISABLE_IR_CACHES, "true").build();
  }

  @Test
  public void testCompareList() throws Exception {
    var ensoCtx =
        (EnsoContext)
            ctx.getBindings(LanguageInfo.ID)
                .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
                .asHostObject();
    var name = "Standard.Base.Data.List";

    var v =
        ctx.eval("enso", """
    import Standard.Base.Data.List

    empty = List.List.Nil
    """)
            .invokeMember(MethodNames.Module.EVAL_EXPRESSION, "empty");
    assertEquals("List", v.getMetaObject().getMetaSimpleName());

    var option = ensoCtx.findModule(name);
    assertTrue("Module found", option.isPresent());
    var module = option.get();
    var ir = module.getIr().duplicate(true, true, true, true);
    var cm = new ModuleCache.CachedModule(ir, CompilationStage.AFTER_CODEGEN, module.getSource());
    byte[] arr = module.getCache().serialize(ensoCtx, cm);

    var meta = new ModuleCache.Metadata("hash", "code", CompilationStage.AFTER_CODEGEN.toString());
    var cachedIr = module.getCache().deserialize(ensoCtx, arr, meta, null);
    assertNotNull("IR read", cachedIr);
    CompilerTest.assertIR(name, ir, cachedIr.moduleIR());
  }

  @Test
  public void testCompareWithWarning() throws Exception {
    var ensoCtx =
        (EnsoContext)
            ctx.getBindings(LanguageInfo.ID)
                .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
                .asHostObject();
    var name = "TestWarning";
    var code =
        Source.newBuilder("enso", """
      empty x = 42
      """, "TestWarning.enso").build();

    var v = ctx.eval(code).invokeMember(MethodNames.Module.EVAL_EXPRESSION, "empty").execute(-1);
    assertEquals(42, v.asInt());

    var option = ensoCtx.findModule(name);
    assertTrue("Module found", option.isPresent());
    var module = option.get();
    var ir = module.getIr().duplicate(true, true, true, true);
    var cm = new ModuleCache.CachedModule(ir, CompilationStage.AFTER_CODEGEN, module.getSource());
    byte[] arr = module.getCache().serialize(ensoCtx, cm);

    var meta = new ModuleCache.Metadata("hash", "code", CompilationStage.AFTER_CODEGEN.toString());
    var cachedIr = module.getCache().deserialize(ensoCtx, arr, meta, null);
    assertNotNull("IR read", cachedIr);
    CompilerTest.assertIR(name, ir, cachedIr.moduleIR());
  }
}
