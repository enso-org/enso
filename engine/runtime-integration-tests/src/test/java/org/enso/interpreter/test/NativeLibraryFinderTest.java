package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import com.oracle.truffle.api.TruffleFile;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
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
                .append(mappedLibName)
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
