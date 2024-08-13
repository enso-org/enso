package org.enso.compiler.dump.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import org.enso.compiler.dump.IRDumper;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.junit.Test;

public class IRDumpTest {
  @Test
  public void testIrDump() {
    var irDumpsDir = Path.of(IRDumper.DEFAULT_DUMP_DIR);
    var out = new ByteArrayOutputStream();
    System.setProperty(IRDumper.SYSTEM_PROP, "true");
    try (var ctx = ContextUtils.defaultContextBuilder().out(out).build()) {
      // Dumping is done in the compiler, so it is enough just to compile the module
      ContextUtils.compileModule(ctx, """
          main = 42
          """, "MyMainModule");
      assertThat(
          "ir-dumps directory was generated in current working directory",
          irDumpsDir.toFile().exists(),
          is(true));
      var mainModDump = irDumpsDir.resolve("MyMainModule.dot");
      assertThat(
          "MyMainModule.dot file was generated in ir-dumps directory",
          mainModDump.toFile().exists(),
          is(true));
    } finally {
      System.setProperty(IRDumper.SYSTEM_PROP, "false");
      try {
        ProjectUtils.deleteRecursively(irDumpsDir);
      } catch (IOException e) {
        // Ignore. The ir-dumps directory should be deleted eventually.
      }
      out.reset();
    }
  }
}
