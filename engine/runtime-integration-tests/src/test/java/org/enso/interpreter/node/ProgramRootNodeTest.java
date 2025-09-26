package org.enso.interpreter.node;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.File;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.data.Type;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.junit.ClassRule;
import org.junit.Test;

public class ProgramRootNodeTest {
  @ClassRule public static final ContextUtils ctx = ContextUtils.createDefault();

  @Test
  public void checkRelativeInPackage() throws Exception {
    var vector =
        ctx.evalModule(
            """
            import Standard.Base.Data.Vector.Vector
            main = Vector
            """);

    var rawVector = ctx.unwrapValue(vector);
    if (rawVector instanceof Type vectorType) {
      var vectorModule = vectorType.getDefinitionScope().getModule();
      var vectorSource = vectorModule.getSource();

      var newModule = ctx.eval(Source.newBuilder("enso", new File(vectorSource.getPath())).build());
      if (ctx.unwrapValue(newModule) instanceof Module secondModule) {
        assertEquals("Name is properly resolved", vectorModule.getName(), secondModule.getName());
      } else {
        fail("Other result should be a module: " + newModule);
      }
    } else {
      fail("" + vector.getMetaQualifiedName());
    }
  }

  @Test
  public void sourceSectionAndName() throws Exception {
    var code =
        """
        main = "Error!
        """;
    var src = Source.newBuilder("enso", code, "error_test.enso").build();
    try {
      var m = ctx.eval(src);
      fail("Expecting an error exception: " + m);
    } catch (PolyglotException ex) {
      var f = ex.getPolyglotStackTrace().iterator().next();
      assertEquals("error_test", f.getRootName());
    }
  }
}
