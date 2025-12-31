package org.enso.interpreter.test.libraries;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import org.enso.common.RuntimeOptions;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * This test:
 *
 * <ul>
 *   <li>Creates a ZIP archive from an extension library stored in test resources.
 *   <li>Uses this ZIP archive as a "remote" repository for libraries
 *   <li>Creates a custom edition that declares repository with {@code jar:file} URL
 *   <li>Creates a dummy project that uses this edition and calls a method from the extension
 *       library
 *   <li>Runs the project and verifies that the library was downloaded
 * </ul>
 */
public final class DownloadLibraryTest {
  @ClassRule public static final TemporaryFolder tmpFolder = new TemporaryFolder();
  private static final String EDITION_NAME = "extra_edition";
  private static final String LIB_DIR_RESOURCE = "download_library_test/libraries";
  private static final String LIB_NAMESPACE = "Extension";
  private static final String LIB_NAME = "Lib";
  private static final String LIB_VERSION = "1.0.0";

  private URL zippedLibsURL;
  private Path editionsDir;
  private Path projDir;

  @Before
  public void setup() throws IOException {
    var rootDir = tmpFolder.newFolder().toPath();
    createZippedLibrary();
    createEditions(rootDir);
    createProject(rootDir);
  }

  /**
   * Creates ZIP archive from the extension library in test resources. This ZIP will be used as a
   * "remote" repository for downloading the libraries.
   */
  private void createZippedLibrary() throws IOException {
    var libDirURL = DownloadLibraryTest.class.getClassLoader().getResource(LIB_DIR_RESOURCE);
    Path libDir;
    try {
      libDir = Path.of(libDirURL.toURI());
    } catch (URISyntaxException e) {
      throw new AssertionError("Failed to get library directory", e);
    }
    var zipOut = tmpFolder.newFile("Libs.zip");
    zipDirectory(libDir, zipOut.toPath());
    assertThat(zipOut.exists(), is(true));
    zippedLibsURL = URI.create("jar:" + zipOut.toPath().toAbsolutePath().toUri() + "!/").toURL();
  }

  private void createEditions(Path rootDir) throws IOException {
    editionsDir = rootDir.resolve("editions");
    editionsDir.toFile().mkdirs();
    var editionContent =
        """
        extends: ${parent_edition}
        engine-version: ${engine_version}
        repositories:
          - name: ${repo_name}
            url: ${repo_url}
        libraries:
          - name: ${lib_name}
            version: ${lib_version}
            repository: ${repo_name}
        """
            .replace("${parent_edition}", parentEdition())
            .replace("${engine_version}", engineVersion())
            .replace("${repo_name}", "local_repo")
            .replace("${repo_url}", zippedLibsURL.toString())
            .replace("${lib_name}", LIB_NAMESPACE + "." + LIB_NAME)
            .replace("${lib_version}", LIB_VERSION);
    var editionFile = editionsDir.resolve(EDITION_NAME + ".yaml");
    Files.writeString(editionFile, editionContent);
  }

  private static String parentEdition() {
    var env = System.getenv("ENSO_EDITION");
    if (env != null) {
      return env;
    } else {
      return "0.0.0-dev";
    }
  }

  private static String engineVersion() {
    var env = System.getenv("ENSO_VERSION");
    if (env != null) {
      return env;
    } else {
      return "0.0.0-dev";
    }
  }

  /**
   * Creates a dummy project that will be executed in the test. This project just calls a method
   * from the extension library.
   */
  private void createProject(Path rootDir) throws IOException {
    var projName = "Proj";
    projDir = rootDir.resolve(projName);
    var projSrc =
        """
        from ${lib_name} import all
        main =
            foo 5
        """
            .replace("${lib_name}", LIB_NAMESPACE + "." + LIB_NAME);
    var packageYaml =
        """
        name: ${proj_name}
        namespace: local
        edition: '${edition_name}'
        """
            .replace("${proj_name}", projName)
            .replace("${edition_name}", EDITION_NAME);
    projDir.toFile().mkdirs();
    Files.writeString(projDir.resolve("package.yaml"), packageYaml);
    projDir.resolve("src").toFile().mkdirs();
    Files.writeString(projDir.resolve("src/Main.enso"), projSrc);
  }

  @Test
  public void downloadExtensionLibraryFromCustomEdition() {
    var ctxBldr =
        ContextUtils.newBuilder()
            .withModifiedContext(
                b ->
                    b.option(RuntimeOptions.EDITION_OVERRIDE, EDITION_NAME)
                        .option(
                            RuntimeOptions.EDITIONS_DIRECTORY,
                            editionsDir.toAbsolutePath().toString()));
    ProjectUtils.testProjectRun(
        ctxBldr,
        projDir,
        res -> {
          assertThat(res.isNumber(), is(true));
          assertThat(res.asInt(), is(8));
        });
  }

  private static void zipDirectory(Path dirToZip, Path zipOut) throws IOException {
    try (var outputStream =
        Files.newOutputStream(
            zipOut, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)) {
      try (var zipOutStream = new ZipOutputStream(outputStream)) {
        try (var inputDirStream = Files.walk(dirToZip)) {
          inputDirStream
              .filter(path -> !Files.isDirectory(path))
              .forEach(
                  path -> {
                    var zipEntryName = normalizeZipEntryName(dirToZip.relativize(path).toString());
                    try {
                      zipOutStream.putNextEntry(new ZipEntry(zipEntryName));
                      Files.copy(path, zipOutStream);
                      zipOutStream.closeEntry();
                    } catch (IOException e) {
                      throw new AssertionError(e);
                    }
                  });
        }
      }
    }
  }

  private static String normalizeZipEntryName(String entryName) {
    return entryName.replace('\\', '/');
  }
}
