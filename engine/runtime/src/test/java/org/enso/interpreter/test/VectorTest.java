package org.enso.interpreter.test;

import java.io.ByteArrayOutputStream;
import java.net.URI;
import java.nio.file.Paths;
import java.util.BitSet;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyArray;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

public class VectorTest {
  private Context ctx;

  @Before
  public void prepareCtx() {
    this.ctx = Context.newBuilder()
      .allowExperimentalOptions(true)
      .allowIO(true)
      .allowAllAccess(true)
      .logHandler(new ByteArrayOutputStream())
      .option(
        RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
        Paths.get("../../distribution/component").toFile().getAbsolutePath()
      ).build();
    final Map<String, Language> langs = ctx.getEngine().getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));
  }

  @Test
  public void evaluation() throws Exception {
    final URI facUri = new URI("memory://choose.enso");
    final Source facSrc = Source.newBuilder("enso", """
    import Standard.Base.Data.Vector

    choose x = case x of
        Vector -> "is vector module"
        _ : Vector.Vector -> "is vector type"
        _ -> "nothing"

    check = choose [1, 2, 3]
    """, "choose.enso")
            .uri(facUri)
            .buildLiteral();

    var module = ctx.eval(facSrc);
    var res = module.invokeMember("eval_expression", "check");
    assertEquals("is vector type", res.asString());
  }

  @Test
  public void vectorToString() throws Exception {
    final URI facUri = new URI("memory://vector.enso");
    final Source src = Source.newBuilder("enso", """
    check = [1, 2, 3]
    """, "vector.enso")
            .uri(facUri)
            .buildLiteral();

    var module = ctx.eval(src);
    var res = module.invokeMember("eval_expression", "check");
    assertEquals("[1, 2, 3]", res.toString());
  }

  @Test
  public void arrayToString() throws Exception {
    final URI facUri = new URI("memory://vector.enso");
    final Source src = Source.newBuilder("enso", """
    check = [1, 2, 3].to_array
    """, "vector.enso")
            .uri(facUri)
            .buildLiteral();

    var module = ctx.eval(src);
    var res = module.invokeMember("eval_expression", "check");
    assertEquals("[1, 2, 3]", res.toString());
  }

  @Test
  public void passingVectorDirectlyIntoJava() throws Exception {
    final URI uri = new URI("memory://callback.enso");
    final Source src = Source.newBuilder("enso", """
    import Standard.Base.Data.Vector

    callback f = f.accept ([1, 2, 3].map +5)
    """, "callback.enso")
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);

    class ConsumeList implements Consumer<List<Long>> {
      boolean called;

      ConsumeList() {
      }

      @Override
      public void accept(List<Long> c) {
        assertEquals(3, c.size());
        assertEquals(6L, (long)c.get(0));
        assertEquals(7L, (long)c.get(1));
        assertEquals(8L, (long)c.get(2));
        called = true;
      }
    }
    var consumeList = new ConsumeList();

    var callback = module.invokeMember("eval_expression", "callback");
    var res = callback.execute(consumeList);
    assertTrue("No result", res.isNull());
    assertTrue("Callback called", consumeList.called);
  }

  @Test
  public void passingListOrArrayToEnsoAsArray() throws Exception {
    final URI uri = new URI("memory://how_long.enso");
    final Source src = Source.newBuilder("enso", """
    import Standard.Base.Data.Array.Array

    how_long array = case array of
        arr : Array -> arr.length
        _ -> -1
    """, "how_long.enso")
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);


    var callback = module.invokeMember("eval_expression", "how_long");

    var four = callback.execute(List.of("a", "b", "c", "d"));
    assertEquals("Four elements", 4, four.asInt());

    var hundred = callback.execute((Object) new String[100]);
    assertEquals("Hundred elements", 100, hundred.asInt());
  }

  private static final BitSet QUERIED = new BitSet();
  public static List<String> lazyList() {
    return new java.util.AbstractList<String>() {
      @Override
      public String get(int index) {
        QUERIED.set(index);
        return "at" + index;
      }

      @Override
      public int size() {
        return 10;
      }
    };
  }

  public static ProxyArray lazyProxy() {
    return new ProxyArray() {
      @Override
      public Object get(long index) {
        QUERIED.set((int) index);
        return "at" + index;
      }

      @Override
      public void set(long index, Value value) {
        throw new UnsupportedOperationException();
      }

      @Override
      public long getSize() {
        return 10;
      }
    };
  }

  @Test
  public void noCopyLazyJavaList() throws Exception {
    noCopyTest("lazyList");
  }

  @Test
  public void noCopyLazyProxyArray() throws Exception {
    noCopyTest("lazyProxy");
  }

  private void noCopyTest(String factoryName) throws Exception {
    final URI uri = new URI("memory://how_long.enso");
    final Source src = Source.newBuilder("enso", """
    import Standard.Base.Data.Vector.Vector
    polyglot java import org.enso.interpreter.test.VectorTest

    raw = VectorTest.${call}
    copy = Vector.from_array VectorTest.${call}
    lazy = Vector.from_polyglot_array VectorTest.${call}

    """.replace("${call}", factoryName), "vectors.enso")
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);

    {
      QUERIED.clear();
      var raw = module.invokeMember("eval_expression", "raw");

      assertTrue("We got raw array", raw.hasArrayElements());
      assertEquals("No query yet", 0, QUERIED.cardinality());

      assertEquals("at0", raw.getArrayElement(0).asString());
      assertEquals("One query", 1, QUERIED.cardinality());

      assertEquals("at7", raw.getArrayElement(7).asString());
      assertEquals("Two queries", 2, QUERIED.cardinality());
    }

    {
      QUERIED.clear();
      var raw = module.invokeMember("eval_expression", "copy");

      assertTrue("We got raw array", raw.hasArrayElements());
      assertEquals("All elements queried", 10, QUERIED.cardinality());

      assertEquals("at0", raw.getArrayElement(0).asString());
      assertEquals("at7", raw.getArrayElement(7).asString());
    }



    {
      QUERIED.clear();
      var raw = module.invokeMember("eval_expression", "lazy");

      assertTrue("We got raw array", raw.hasArrayElements());
      assertEquals("No query yet", 0, QUERIED.cardinality());

      assertEquals("at0", raw.getArrayElement(0).asString());
      assertEquals("One query", 1, QUERIED.cardinality());

      assertEquals("at7", raw.getArrayElement(7).asString());
      assertEquals("Two queries", 2, QUERIED.cardinality());
    }
  }
}
