package org.enso.interpreter.runtime;

import static org.junit.Assert.assertTrue;

import java.util.List;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.polyglot.PolyglotContext;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.TypeLiteral;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class ListLibrariesTest {
  private static Context ctx;

  @BeforeClass
  public static void initCtx() {
    ctx = ContextUtils.createDefaultContext();
  }

  @AfterClass
  public static void closeCtx() {
    ctx.close();
  }

  @Test
  public void listLibraries() {
    var b = ctx.getBindings(LanguageInfo.ID);
    var libs = b.invokeMember(MethodNames.TopScope.LOCAL_LIBRARIES);
    assertTrue("Array of lib names: " + libs, libs.hasArrayElements());
    var list = libs.as(new TypeLiteral<List<String>>() {});
    assertTrue("At least five libs: " + list, list.size() >= 5);

    assertTrue("Base found " + list, list.contains("Standard.Base"));
    assertTrue("Table found " + list, list.contains("Standard.Table"));
    assertTrue("DB found " + list, list.contains("Standard.Database"));
    assertTrue("AWS found " + list, list.contains("Standard.AWS"));
    assertTrue("Geo found " + list, list.contains("Standard.Geo"));
  }

  @Test
  public void evaluateDefaultReplScript() {
    var pc = new PolyglotContext(ctx);
    final var fnName = "main_fn_name__";
    var module = pc.evalReplModule(fnName);
    var result = module.evalExpression(fnName);
    assertTrue("Returns Nothing", result.isNull());
  }
}
