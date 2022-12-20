package org.enso.interpreter.test;

import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URI;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.enso.interpreter.runtime.type.ConstantsGen;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;

public class MetaObjectTest {
  private Context ctx;

  @Before
  public void prepareCtx() throws Exception {
    Engine eng =
        Engine.newBuilder()
            .allowExperimentalOptions(true)
            .logHandler(new ByteArrayOutputStream())
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .build();
    this.ctx = Context.newBuilder().engine(eng).allowIO(true).allowAllAccess(true).build();
    final Map<String, Language> langs = ctx.getEngine().getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));
  }

  @Test
  public void checkingAtomMetaObject() throws Exception {
    final URI uri = new URI("memory://callback.enso");
    final Source src = Source.newBuilder("enso", """
    type Atm
        Data x
        End

    data = Atm.Data 5
    end = Atm.End
    """, "atom_test.enso")
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);

    var data = module.invokeMember("eval_expression", "data");
    assertFalse("Non-null result", data.isNull());
    var meta = data.getMetaObject();
    assertNotNull("Non-null meta for " + data, meta);
    assertFalse("Non-null type", meta.isNull());
    assertTrue("Is meta", meta.isMetaObject());
    assertEquals("Atm", meta.getMetaSimpleName());
    assertEquals("atom_test.Atm", meta.getMetaQualifiedName());
    assertFalse("Isn't instance of itself", meta.isMetaInstance(meta));
    assertTrue("Is instance", meta.isMetaInstance(data));
    assertTrue("Eigen-type", meta.getMetaObject().isMetaObject());
    assertEquals("Eigen-type is equal", meta.getMetaObject(), meta.getMetaObject().getMetaObject());

    var end = module.invokeMember("eval_expression", "end");
    assertEquals("Same meta", end.getMetaObject(), data.getMetaObject());
  }

  @Test
  public void checkAllConstantGenValuesArePresent() throws Exception {
    var g = ValuesGenerator.create(ctx);
    var expecting = new HashSet<String>();
    for (var f : ConstantsGen.class.getFields()) {
      var s = (String) f.get(null);
      expecting.add(s);
    }
    var w = new StringBuilder();
    var f = new StringWriter();
    var err = new PrintWriter(f);
    for (var t : g.allTypes()) {
      try {
        var n = t.getMetaQualifiedName();
        assertNotNull("Type " + t + " has meta name", n);
        if (!expecting.remove(n)) {
          w.append("\nNo type removed for ").append(n);
        }
      } catch (AssertionError ex) {
        err.println(ex.getMessage());
      }
    }

    if (f.toString().length() > 0) {
      fail(f.toString());
    }

    if (!expecting.isEmpty()) {
      fail("Should be empty: " + expecting + w);
    }
  }

  @Test
  public void checkAllTypesHaveSomeValues() throws Exception {
    var g = ValuesGenerator.create(ctx);
    var expecting = new HashSet<Value>();
    expecting.addAll(g.allTypes());
    var successfullyRemoved = new HashSet<Value>();
    var w = new StringBuilder();
    for (var v : g.allValues()) {
      checkValue(v, expecting, successfullyRemoved, w);
    }
    if (!expecting.isEmpty()) {
      fail("These types don't have any values: " + expecting + w);
    }
  }

  private static void checkValue(Value v, Set<Value> expecting, Set<Value> successfullyRemoved, StringBuilder w) {
    var t = v.getMetaObject();
    if (!expecting.remove(t)) {
      if (!successfullyRemoved.contains(t)) {
        w.append("\nCannot remove type ").append(t).append(" for value ").append(v);
      }
    } else {
      successfullyRemoved.add(t);
    }
  }
}
