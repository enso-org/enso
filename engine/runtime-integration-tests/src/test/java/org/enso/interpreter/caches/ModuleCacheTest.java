package org.enso.interpreter.caches;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.nio.ByteBuffer;
import org.enso.common.CompilationStage;
import org.enso.common.MethodNames;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.test.CompilerTests;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Source;
import org.junit.ClassRule;
import org.junit.Test;

public class ModuleCacheTest {
  @ClassRule
  public static final ContextUtils ctxRule =
      ContextUtils.newBuilder()
          .withModifiedContext(ctxBldr -> ctxBldr.option(RuntimeOptions.DISABLE_IR_CACHES, "true"))
          .build();

  public ModuleCacheTest() {}

  @Test
  public void testCompareList() throws Exception {
    var ensoCtx = ctxRule.ensoContext();
    var name = "Standard.Base.Data.List";

    var v =
        ctxRule
            .eval(
                "enso",
                """
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

    var mc = module.getCache().asSpi(ModuleCache.class);
    byte[] arr = mc.serialize(ensoCtx, cm);

    var meta = new ModuleCache.Metadata("hash", "code", CompilationStage.AFTER_CODEGEN.toString());
    var cachedIr = mc.deserialize(ensoCtx, ByteBuffer.wrap(arr), meta, null);
    assertNotNull("IR read", cachedIr);
    CompilerTests.assertIR(name, ir, cachedIr.moduleIR());
  }

  @Test
  public void testCompareWithWarning() throws Exception {
    var ensoCtx = ctxRule.ensoContext();
    var name = "TestWarning";
    var code =
        Source.newBuilder(
                "enso",
                """
                empty x = 42
                """,
                "TestWarning.enso")
            .build();

    var v =
        ctxRule.eval(code).invokeMember(MethodNames.Module.EVAL_EXPRESSION, "empty").execute(-1);
    assertEquals(42, v.asInt());

    var option = ensoCtx.findModule(name);
    assertTrue("Module found", option.isPresent());
    var module = option.get();
    var ir = module.getIr().duplicate(true, true, true, true);
    var cm = new ModuleCache.CachedModule(ir, CompilationStage.AFTER_CODEGEN, module.getSource());
    var mc = module.getCache().asSpi(ModuleCache.class);
    byte[] arr = mc.serialize(ensoCtx, cm);

    var meta = new ModuleCache.Metadata("hash", "code", CompilationStage.AFTER_CODEGEN.toString());
    var cachedIr = mc.deserialize(ensoCtx, ByteBuffer.wrap(arr), meta, null);
    assertNotNull("IR read", cachedIr);
    CompilerTests.assertIR(name, ir, cachedIr.moduleIR());
  }
}
