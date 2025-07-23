package org.enso.interpreter.test.interop;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.net.URI;
import org.enso.interpreter.node.callable.InteropApplicationNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.state.State;
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
    import Standard.Base.Enso_Cloud.Enso_File_System_Impl

    data =
        Meta.lookup_services File_System_SPI
    """,
                "services.enso")
            .uri(uri)
            .buildLiteral();

    var mod = ctx.eval(src);
    var ensoCtx = ctx.ensoContext();

    for (var p : ensoCtx.getPackageRepository().getLoadedPackagesJava()) {
      p.getConfig()
          .services()
          .foreach(
              pw -> {
                var spiType = findType(pw.provides(), ensoCtx);
                var implType = findType(pw.with(), ensoCtx);
                var conversion = UnresolvedConversion.build(implType.getDefinitionScope());
                var state = State.create(ensoCtx);
                var node = InteropApplicationNode.getUncached();
                var fn = conversion.resolveFor(ensoCtx, spiType, implType);
                var conv = node.execute(fn, state, new Object[] {spiType, implType});
                assertNotNull("Some value found", conv);
                var fsImpl = ctx.asValue(conv);
                assertNotNull("Some implementation found", fsImpl);
                assertEquals("Protocol", "enso", fsImpl.getMember("protocol").asString());
                assertEquals(
                    "Type",
                    "Standard.Base.Enso_Cloud.Enso_File",
                    fsImpl.getMember("typ").getMetaQualifiedName());
                return null;
              });
    }
  }

  private Type findType(String name, EnsoContext ensoCtx) {
    var moduleName = name.replaceFirst("\\.[^\\.]*$", "");
    var typeName = name.substring(moduleName.length() + 1);
    var module = ensoCtx.getTopScope().getModule(moduleName).get();
    var implType = module.getScope().getType(typeName, true);
    return implType;
  }
}
