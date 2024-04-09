package org.enso.benchmarks;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.util.List;
import org.enso.benchmarks.processor.SpecCollector;
import org.enso.pkg.PackageManager;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames.TopScope;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class TestSpecCollector {

  private Context ctx;
  private File ensoDir;
  private File ensoHomeOverride;
  private Value testCollectorMainModule;

  @Before
  public void setup() {
    ensoDir = locateRootDirectory();
    assertNotNull("Could not find Enso root directory", ensoDir);
    assertTrue(ensoDir.exists());
    assertTrue(ensoDir.isDirectory());
    assertTrue(ensoDir.canRead());

    // Note that ensoHomeOverride does not have to exist, only its parent directory
    ensoHomeOverride = ensoDir.toPath().resolve("distribution").resolve("component").toFile();
    File testCollectorPath =
        new File(getClass().getClassLoader().getResource("Test_Collector").getPath());
    var pkg = PackageManager.Default().fromDirectory(testCollectorPath).get();
    var mainModuleName = pkg.moduleNameForFile(pkg.mainFile());
    ctx =
        Context.newBuilder(LanguageInfo.ID)
            .allowAllAccess(true)
            .allowIO(IOAccess.ALL)
            .option(RuntimeOptions.LOG_LEVEL, "WARNING")
            .option(RuntimeOptions.PROJECT_ROOT, testCollectorPath.getAbsolutePath())
            .option(RuntimeOptions.LANGUAGE_HOME_OVERRIDE, ensoHomeOverride.getAbsolutePath())
            .build();
    testCollectorMainModule =
        ctx.getBindings(LanguageInfo.ID)
            .invokeMember(TopScope.GET_MODULE, mainModuleName.toString());
  }

  @After
  public void tearDown() {
    ctx.close();
  }

  /**
   * Locates the root of the Enso repository. Heuristic: we just keep going up the directory tree
   * until we are in a directory containing ".git" subdirectory. Note that we cannot use the "enso"
   * name, as users are free to name their cloned directories however they like.
   */
  private File locateRootDirectory() {
    File rootDir = null;
    try {
      rootDir =
          new File(
              TestSpecCollector.class.getProtectionDomain().getCodeSource().getLocation().toURI());
    } catch (URISyntaxException e) {
      fail("repository root directory not found: " + e.getMessage());
    }
    for (; rootDir != null; rootDir = rootDir.getParentFile()) {
      // Check if rootDir contains ".git" subdirectory
      if (Files.exists(rootDir.toPath().resolve(".git"))) {
        break;
      }
    }
    return rootDir;
  }

  @Test
  public void testCollectAllSuitesFromMainModule() {
    List<ModuleBenchSuite> moduleBenchSuites =
        SpecCollector.collectBenchSpecsFromModule(testCollectorMainModule, "group_1");
    for (ModuleBenchSuite moduleBenchSuite : moduleBenchSuites) {
      assertEquals(1, moduleBenchSuite.getGroups().size());
      assertEquals("Test_Group", moduleBenchSuite.getGroups().get(0).name());
      List<BenchSpec> specs = moduleBenchSuite.getGroups().get(0).specs();
      assertEquals(1, specs.size());
      assertEquals("Test_Spec", specs.get(0).name());
      Value code = specs.get(0).code();
      Value res = code.execute(Value.asValue(null));
      assertEquals(2, res.asInt());
      var group = moduleBenchSuite.getGroups().get(0);
      assertEquals(11, group.configuration().warmup().iterations());
      assertEquals(12, group.configuration().warmup().seconds());
      assertEquals(13, group.configuration().measure().iterations());
      assertEquals(14, group.configuration().measure().seconds());
    }
    assertFalse(moduleBenchSuites.isEmpty());
  }

  @Test
  public void testCollectSuitesNonExistendVarName() {
    List<ModuleBenchSuite> moduleBenchSuites =
        SpecCollector.collectBenchSpecsFromModule(testCollectorMainModule, "NOOOON_existent_FOO");
    assertTrue(moduleBenchSuites.isEmpty());
  }
}
