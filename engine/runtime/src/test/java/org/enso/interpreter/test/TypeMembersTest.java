package org.enso.interpreter.test;

import java.io.ByteArrayOutputStream;
import java.net.URI;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.Assert;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

public class TypeMembersTest {
  private Context ctx;

  @Before
  public void prepareCtx() {
    Engine eng = Engine.newBuilder()
      .allowExperimentalOptions(true)
      .logHandler(new ByteArrayOutputStream())
      .option(
        RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
        Paths.get("../../distribution/component").toFile().getAbsolutePath()
      ).build();
    this.ctx = Context.newBuilder()
      .engine(eng)
      .allowIO(true)
      .allowAllAccess(true)
      .build();
    final Map<String, Language> langs = ctx.getEngine().getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));
  }


  @Test
  public void checkAtomMembers() throws Exception {
    final URI uri = new URI("memory://how_long.enso");
    final Source src = Source.newBuilder("enso", """
    from Standard.Base.Data.Boolean import True, False

    type IntList
        End
        Head Int IntList

        is_empty self = case self of
            End -> True
            _ -> False

        tail self = case self of
            Head _ t -> t
            _ -> End

        head self = case self of
            Head h _ -> h
            _ -> -1

    list1 = Head 7 <| Head 3 <| End

    """, "compare.enso")
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);


    var list1 = module.invokeMember("eval_expression", "list1");
    var seven = module.invokeMember("eval_expression", "list1.head");
    var three = module.invokeMember("eval_expression", "list1.tail.head");
    var list2 = module.invokeMember("eval_expression", "list1.tail.tail");

    assertTrue("seven is number-like: " + seven, seven.fitsInInt());
    assertTrue("three is number-like: " + three, three.fitsInInt());
    assertFalse("list1 is not number-like: " + list1, list1.fitsInInt());
    assertFalse("list2 is not number-like: " + list2, list2.fitsInInt());

    assertEquals("seven check", 7, seven.asInt());
    assertEquals("three check", 3, three.asInt());

    assertMembers("Keys in list1", list1, "head", "tail", "is_empty");
    assertMembers("Keys in list2", list2, "head", "tail", "is_empty");
  }

  private static void assertMembers(String msg, Value v, String... keys) {
    var realKeys = v.getMemberKeys();
    for (var k : keys) {
      assertTrue(msg + " - found " + k + " in " + realKeys, realKeys.contains(k));
      assertTrue(msg + " - has member " + k, v.hasMember(k));
      assertNotNull(msg + " - can be invoked", v.invokeMember(k));
    }
  }
}
