package org.enso.base.polyglot.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.enso.base.polyglot.EnsoMeta;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.PolyglotException;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class EnsoMetaTest {
  @ClassRule public static final ContextUtils ctx = ContextUtils.createDefault();

  @BeforeClass
  public static void importAll() {
    ctx.eval("enso", "from Standard.Base import all");
  }

  @Test
  public void loadErrorType() {
    var errorType = EnsoMeta.getType("Standard.Base.Error", "Error");
    assertTrue("Is meta object", errorType.isMetaObject());
    var fqn = errorType.getMetaQualifiedName();
    assertEquals("Standard.Base.Error.Error", fqn);
    var error = errorType.invokeMember("throw", "error message");
    assertTrue("An error was created", error.isException());
    try {
      throw error.throwException();
    } catch (PolyglotException ex) {
      assertEquals("Converted to panic with the same exception", "error message", ex.getMessage());
    }
  }
}
