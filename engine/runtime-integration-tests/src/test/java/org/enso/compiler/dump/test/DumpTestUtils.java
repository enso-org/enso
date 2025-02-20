package org.enso.compiler.dump.test;

import static org.enso.test.utils.ContextUtils.compileModule;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Path;
import org.enso.compiler.docs.DocsGenerate;
import org.enso.compiler.docs.DocsVisit;
import org.enso.pkg.QualifiedName;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.graalvm.polyglot.Context;

final class DumpTestUtils {
  private DumpTestUtils() {}

  static void generateDocumentation(Path projDir, String projName, String code, DocsVisit v)
      throws IOException {
    ProjectUtils.createProject(projName, code, projDir);
    ProjectUtils.generateProjectDocs(
        "api",
        ContextUtils.defaultContextBuilder(),
        projDir,
        (context) -> {
          var enso = ContextUtils.leakContext(context);
          var modules = enso.getTopScope().getModules();
          var optMod =
              modules.stream().filter(m -> m.getName().toString().contains(projName)).findFirst();
          assertTrue(
              "Found " + projName + " in " + modules.stream().map(m -> m.getName()).toList(),
              optMod.isPresent());
          var mod = optMod.get();
          assertEquals("local." + projName + ".Main", mod.getName().toString());
          var ir = mod.getIr();
          assertNotNull("Ir for " + mod + " found", ir);

          try {
            DocsGenerate.visitModule(v, mod.getName(), ir, null);
          } catch (IOException e) {
            throw raise(RuntimeException.class, e);
          }
        });
  }

  /**
   * Returns generated signatures as string. Uses {@link org.enso.compiler.docs.DocsEmitSignatures}
   * visitor.
   *
   * @param moduleSrc Source code of the module.
   * @param modName FQN of the module.
   * @return Signature string for the module.
   */
  static String generateSignatures(Context context, String moduleSrc, String modName)
      throws IOException {
    var modIr = compileModule(context, moduleSrc, modName);
    var sigGenerator = DocsVisit.createSignatures();
    var out = new ByteArrayOutputStream();
    var writer = new PrintWriter(out);
    var modFqn = QualifiedName.fromString(modName);
    DocsGenerate.visitModule(sigGenerator, modFqn, modIr, writer);
    writer.flush();
    return out.toString();
  }

  @SuppressWarnings("unchecked")
  private static <E extends Exception> E raise(Class<E> type, Exception t) throws E {
    throw (E) t;
  }
}
