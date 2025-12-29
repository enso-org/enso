package org.enso.compiler.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.yaml.snakeyaml.Yaml;

/**
 * Tests that {@code --compile} also creates {@code manifest.yaml} file with correctly inferred
 * dependencies.
 */
public class UpdateManifestTest {
  @Rule public final TemporaryFolder tempFolder = new TemporaryFolder();

  @Test
  public void projectCompilationAlsoCreatesManifest_NoDependencies() throws IOException {
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject(
        "Proj",
        """
        main =
            42
        """,
        projDir);
    compileProject(
        projDir,
        _ -> {
          var manifestFile = projDir.resolve("manifest.yaml");
          assertThat("Manifest file exists", manifestFile.toFile().exists(), is(true));
          assertManifestHasNoDependencies(manifestFile);
        });
  }

  @Test
  public void projectCompilationAlsoCreatesManifest_SingleDependency() throws IOException {
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject(
        "Proj",
        """
        from Standard.Base import all
        main =
            42
        """,
        projDir);
    compileProject(
        projDir,
        _ -> {
          var manifestFile = projDir.resolve("manifest.yaml");
          assertThat("Manifest file exists", manifestFile.toFile().exists(), is(true));
          assertManifestDeclaresDependencies(manifestFile, List.of("Standard.Base"));
        });
  }

  @Test
  public void projectCompilationAlsoCreatesManifest_MultipleDependencies() throws IOException {
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject(
        "Proj",
        """
        from Standard.Base import all
        from Standard.Table import all
        from Standard.Image import all
        main =
            42
        """,
        projDir);
    compileProject(
        projDir,
        _ -> {
          var manifestFile = projDir.resolve("manifest.yaml");
          assertThat("Manifest file exists", manifestFile.toFile().exists(), is(true));
          assertManifestDeclaresDependencies(
              manifestFile, List.of("Standard.Base", "Standard.Table", "Standard.Image"));
        });
  }

  private static void compileProject(Path projDir, Consumer<ContextUtils> whenDone) {
    ProjectUtils.generateProjectDocs(null, ContextUtils.newBuilder(), projDir, whenDone);
  }

  @SuppressWarnings("unchecked")
  private static void assertManifestDeclaresDependencies(
      Path manifest, List<String> expectedDependencies) {
    var yaml = new Yaml();
    try (var is = Files.newInputStream(manifest)) {
      Map<String, Object> content = yaml.load(is);
      List<String> deps = (List<String>) content.get("dependencies");
      assertThat(deps, containsInAnyOrder(expectedDependencies));
    } catch (IOException e) {
      throw new AssertionError(e);
    }
  }

  @SuppressWarnings("unchecked")
  private static void assertManifestHasNoDependencies(Path manifest) {
    var yaml = new Yaml();
    try (var is = Files.newInputStream(manifest)) {
      Map<String, Object> content = yaml.load(is);
      List<String> deps = (List<String>) content.get("dependencies");
      if (deps != null && !deps.isEmpty()) {
        throw new AssertionError("Expected no dependencies, found: " + deps);
      }
    } catch (IOException e) {
      throw new AssertionError(e);
    }
  }
}
