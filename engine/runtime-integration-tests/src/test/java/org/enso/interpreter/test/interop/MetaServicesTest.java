package org.enso.interpreter.test.interop;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.List;
import org.enso.interpreter.node.expression.builtin.meta.LookupServicesNode;
import org.enso.interpreter.runtime.data.Type;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Source;
import org.junit.ClassRule;
import org.junit.Test;

public class MetaServicesTest {
  @ClassRule public static ContextUtils ctx = ContextUtils.createDefault();

  @Test
  public void loadFileSystemServices() throws Exception {
    final URI uri = new URI("memory://services.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    import Standard.Base.System.File.File_System_SPI
    import Standard.Base.Meta
    spis =
        Meta.lookup_services File_System_SPI
    """,
                "services.enso")
            .uri(uri)
            .buildLiteral();

    var arr = ctx.evalModule(src, "spis");

    assertTrue("Got SPIs", arr.hasArrayElements());
    var len = arr.getArraySize();
    for (var i = 0L; i < len; i++) {
      var p = arr.getArrayElement(i);
      System.err.println("found " + p);
      if (p.getMember("protocol").asString().equals("enso")) {
        var type = p.getMember("typ");
        assertTrue("It is a type", type.isMetaObject());
        assertEquals("Enso_File", type.getMetaSimpleName());
        return;
      }
    }
    fail("Not found `enso` file protocol among: " + arr);
  }

  @Test
  public void missingConversionYieldsDataflowError() {
    var arr =
        ctx.evalModule(
            """
    import Standard.Base.System.File.File_System_SPI
    type Broken_Impl

    main = [File_System_SPI, Broken_Impl]
    """);
    var node = new MockLookupServicesNode();
    assertTrue("It is an array", arr.hasArrayElements());
    assertEquals("Two elements", 2, arr.getArraySize());
    var spi = (Type) ctx.unwrapValue(arr.getArrayElement(0));
    var impl = (Type) ctx.unwrapValue(arr.getArrayElement(1));
    node.toReturn = List.of(impl);

    var res = ctx.asValue(node.execute(spi));

    assertTrue("Returned an array", res.hasArrayElements());
    assertEquals("One registration found", 1, res.getArraySize());
    assertTrue("But it is errorneus", res.getArrayElement(0).isException());
  }

  private static final class MockLookupServicesNode extends LookupServicesNode {
    List<Type> toReturn;

    @Override
    protected Iterable<Type> findImplementationsFor(Type type) {
      assertNotNull("The test has to tell us what to return first", toReturn);
      return toReturn;
    }
  }
}
