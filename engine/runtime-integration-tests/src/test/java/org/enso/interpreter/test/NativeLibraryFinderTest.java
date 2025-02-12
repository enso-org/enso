package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.TruffleFile;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import org.enso.cli.OS;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.editions.LibraryName;
import org.enso.interpreter.runtime.util.TruffleFileSystem;
import org.enso.pkg.NativeLibraryFinder;
import org.enso.pkg.Package;
import org.enso.test.utils.ContextUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;

public class NativeLibraryFinderTest {

  @Rule public final TestRule printContextRule = new PrintSystemInfoRule();
  private Package<TruffleFile> stdImgPkg;
  private Package<TruffleFile> stdTableauPkg;

  @Test
  public void standardImageShouldHaveNativeLib() {
    try (var ctx = ContextUtils.createDefaultContext()) {
      // Evaluate dummy sources to force loading Standard.Image
      ContextUtils.evalModule(
          ctx, """
          from Standard.Image import all
          main = 42
          """);
      var ensoCtx = ContextUtils.leakContext(ctx);
      var stdImg =
          ensoCtx
              .getPackageRepository()
              .getPackageForLibraryJava(LibraryName.apply("Standard", "Image"));
      assertThat(stdImg.isPresent(), is(true));
      this.stdImgPkg = stdImg.get();
      var nativeLibs =
          NativeLibraryFinder.listAllNativeLibraries(stdImg.get(), TruffleFileSystem.INSTANCE);
      assertThat(
          "There should be just single native lib in Standard.Image", nativeLibs.size(), is(1));
    }
  }

  @Test
  public void standardTableauShouldHaveNativeLib() {
    try (var ctx = ContextUtils.createDefaultContext()) {
      // Evaluate dummy sources to force loading Standard.Tableau
      ContextUtils.evalModule(
          ctx, """
          from Standard.Tableau import all
          main = 42
          """);
      var ensoCtx = ContextUtils.leakContext(ctx);
      var stdTableau =
          ensoCtx
              .getPackageRepository()
              .getPackageForLibraryJava(LibraryName.apply("Standard", "Tableau"));
      assertThat(stdTableau.isPresent(), is(true));
      this.stdTableauPkg = stdTableau.get();
      var nativeLibs =
          NativeLibraryFinder.listAllNativeLibraries(stdTableau.get(), TruffleFileSystem.INSTANCE);
      // Tableau has Tableau's native lib AND jni
      assertThat("There should be two native libs for Standard.Tableau", nativeLibs.size(), is(2));
    }
  }

  @Test
  public void findTableauNativeLibrary() {
    try (var ctx = ContextUtils.createDefaultContext()) {
      // Evaluate dummy sources to force loading Standard.Tableau
      ContextUtils.evalModule(
          ctx,
          """
          from Standard.Base import all
          from Standard.Tableau import all
          main = 42
          """);
      var nativeLib =
          ctx.getBindings(LanguageInfo.ID)
              .invokeMember(MethodNames.TopScope.FIND_NATIVE_LIBRARY, "tableauhyperapi");
      assertNotNull(nativeLib);
      var expectedLibName = OS.isWindows() ? "tableauhyperapi" : "libtableauhyperapi";
      assertThat(nativeLib.asString(), containsString(expectedLibName));
    }
  }

  @Test
  public void failToFindNativeLibrary() {
    try (var ctx = ContextUtils.createDefaultContext()) {
      // Evaluate dummy sources to force loading of some packages
      ContextUtils.evalModule(
          ctx,
          """
          from Standard.Base import all
          from Standard.Table import all
          main = 42
          """);
      var nativeLib =
          ctx.getBindings(LanguageInfo.ID)
              .invokeMember(MethodNames.TopScope.FIND_NATIVE_LIBRARY, "iDontExist");
      assertTrue(nativeLib.isNull());
    }
  }

  public final class PrintSystemInfoRule implements TestRule {
    @Override
    public Statement apply(Statement base, Description description) {
      return new Statement() {
        @Override
        public void evaluate() {
          try {
            base.evaluate();
          } catch (Throwable e) {
            var sb = new StringBuilder();
            sb.append(System.lineSeparator());
            sb.append("  os.name: ")
                .append(System.getProperty("os.name"))
                .append(System.lineSeparator());
            sb.append("  os.arch: ")
                .append(System.getProperty("os.arch"))
                .append(System.lineSeparator());
            var mappedLibName = System.mapLibraryName("opencv_java470");
            sb.append("  Mapped library name: ")
                .append(System.lineSeparator())
                .append("      " + System.mapLibraryName("opencv_java470"))
                .append(System.lineSeparator())
                .append("      " + System.mapLibraryName("tableauhyperapi"))
                .append(System.lineSeparator());
            if (stdImgPkg != null) {
              sb.append("  Contents of Standard.Image native library dir:")
                  .append(System.lineSeparator());
              var nativeLibDir = stdImgPkg.nativeLibraryDir();
              var nativeLibPath = Path.of(nativeLibDir.getAbsoluteFile().getPath());
              var contents = contentsOfDir(nativeLibPath);
              contents.forEach(
                  path -> sb.append("    ").append(path).append(System.lineSeparator()));
            }
            if (stdTableauPkg != null) {
              sb.append("  Contents of Standard.Tableau native library dir:")
                  .append(System.lineSeparator());
              var nativeLibDir = stdTableauPkg.nativeLibraryDir();
              var nativeLibPath = Path.of(nativeLibDir.getAbsoluteFile().getPath());
              var contents = contentsOfDir(nativeLibPath);
              contents.forEach(
                  path -> sb.append("    ").append(path).append(System.lineSeparator()));
            }
            throw new AssertionError(sb.toString(), e);
          }
        }
      };
    }
  }

  private static List<String> contentsOfDir(Path dir) {
    var contents = new ArrayList<String>();
    var fileVisitor =
        new SimpleFileVisitor<Path>() {
          @Override
          public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
            contents.add(file.toAbsolutePath().toString());
            return FileVisitResult.CONTINUE;
          }
        };
    try {
      Files.walkFileTree(dir, fileVisitor);
    } catch (IOException e) {
      throw new AssertionError(e);
    }
    return contents;
  }
}
