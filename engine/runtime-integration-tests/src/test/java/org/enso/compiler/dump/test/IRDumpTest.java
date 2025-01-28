package org.enso.compiler.dump.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.logging.Level;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.dump.service.IRDumpFactoryService;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.IRDumperTestWrapper;
import org.enso.test.utils.ProjectUtils;
import org.junit.After;
import org.junit.Assume;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;
import scala.Option;

/**
 * If run locally, make sure that no IGV instance is running. Otherwise, the IRDumper will try to
 * connect to that instance and the tests will fail.
 */
public class IRDumpTest {
  private static final Path irDumpsDir = Path.of(IRDumpFactoryService.DEFAULT_DUMP_DIR);
  private ByteArrayOutputStream out;

  @Rule public PrintOutRule printOutRule = new PrintOutRule();

  @BeforeClass
  public static void skipOnWindows() {
    Assume.assumeFalse(
        "This test suite should be skipped on Windows",
        System.getProperty("os.name").toLowerCase().contains("win"));
  }

  @Before
  public void before() {
    this.out = new ByteArrayOutputStream();
    cleanIrDumpsDir();
  }

  @After
  public void after() {
    cleanIrDumpsDir();
  }

  private void cleanIrDumpsDir() {
    try {
      ProjectUtils.deleteRecursively(irDumpsDir);
    } catch (IOException e) {
      // Ignore. The ir-dumps directory should be deleted eventually.
    }
  }

  @Test
  public void dumpVectorModule() throws IOException {
    System.setProperty(IRDumpFactoryService.SYSTEM_PROP, "Vector");
    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.toString())
            .out(out)
            .build()) {
      // Dumping is done in the compiler, so it is enough just to compile the module
      ContextUtils.compileModule(
          ctx,
          """
          import Standard.Base.Data.Vector.Vector
          main = 42
          """,
          "MyMainModule");
      assertThat(
          "ir-dumps directory was generated in current working directory",
          irDumpsDir.toFile().exists(),
          is(true));
      var vectorDump =
          Files.list(irDumpsDir)
              .filter(file -> file.getFileName().toString().contains("Vector"))
              .findFirst();
      assertTrue("Vector dump file was generated in ir-dumps directory", vectorDump.isPresent());
    } finally {
      System.setProperty(IRDumpFactoryService.SYSTEM_PROP, "false");
    }
  }

  @Test
  public void dumpExpression() throws Exception {
    var irDumpsDir = Path.of(IRDumpFactoryService.DEFAULT_DUMP_DIR);
    try (var irDumper = new IRDumperTestWrapper()) {
      var lit = new Name.Literal("method", true, null, Option.empty(), new MetadataStorage());
      irDumper.dump(lit, "MyModule", "AfterPass");
    }
    assertTrue("ir-dumps directory was created", irDumpsDir.toFile().exists());
    var dumpsCnt = Files.list(irDumpsDir).count();
    assertThat("ir-dumps directory is not empty", dumpsCnt, is(greaterThan(0L)));
  }

  public final class PrintOutRule implements TestRule {
    @Override
    public Statement apply(Statement base, Description description) {
      return new Statement() {
        @Override
        public void evaluate() {
          try {
            base.evaluate();
          } catch (Throwable e) {
            var errMsg = new StringBuilder();
            errMsg.append("Test failed with output:").append(System.lineSeparator());
            errMsg.append(out.toString());
            throw new AssertionError(errMsg.toString(), e);
          }
        }
      };
    }
  }
}
