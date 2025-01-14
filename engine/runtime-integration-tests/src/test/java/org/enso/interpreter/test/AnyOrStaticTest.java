package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class AnyOrStaticTest {
  private static final ByteArrayOutputStream out = new ByteArrayOutputStream();
  private Context ctx;

  @Before
  public void prepareCtx() {
    ctx = ContextUtils.createDefaultContext(out);
    out.reset();
  }

  @After
  public void disposeCtx() {
    ctx.close();
  }

  @Test
  public void methodOnModuleAny() throws Exception {
    var code =
        """
    from Standard.Base import Any

    check_is v t = "check of "+v.to_text+" and "+t.to_text
    Any.check_is self t = "got to Any for "+self.to_text+" and "+t.to_text

    dispatch a b = Checker.check_is a b
    """;
    var src = Source.newBuilder(LanguageInfo.ID, code, "Checker.enso").build();

    var module = ctx.eval(src);

    var dispatch = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "dispatch");
    var where = dispatch.execute("FirstArg", "SecondArg");
    assertTrue("String returned " + where, where.isString());
    assertEquals("check of FirstArg and SecondArg", where.asString());
  }

  @Test
  public void methodOnTypeAndAny() throws Exception {
    var code =
        """
    from Standard.Base import Any

    type Type_With_Check
        check_is v t = "check of "+v.to_text+" and "+t.to_text

    Any.check_is self t = "got to Any for "+self.to_text+" and "+t.to_text

    dispatch a = Type_With_Check.check_is a
    """;
    var src = Source.newBuilder(LanguageInfo.ID, code, "Typer.enso").build();

    var module = ctx.eval(src);

    var dispatch = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "dispatch");
    var where = dispatch.execute("FirstArg");
    assertTrue("String returned " + where, where.isString());
    assertEquals("got to Any for Type_With_Check and FirstArg", where.asString());
  }
}
