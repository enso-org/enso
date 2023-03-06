package org.enso.compiler;

import com.oracle.truffle.api.source.Source;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import junit.framework.TestCase;
import static junit.framework.TestCase.fail;
import junit.framework.TestSuite;
import org.junit.runner.RunWith;
import org.junit.runners.AllTests;

@RunWith(AllTests.class)
public final class ParseStdLibTest extends TestCase {
  private static final EnsoCompiler ensoCompiler = new EnsoCompiler();
  private final File where;
  private final Dump dump;

  private ParseStdLibTest(String name, File where, Dump dump) {
    super(name);
    this.where = where;
    this.dump = dump;
  }

  public static TestSuite suite() throws Exception {
    TestSuite s = new TestSuite();
    var os = System.getProperty("os.name");
    if (os != null && os.contains("Window")) {
      s.addTest(new ParseStdLibTest("IgnoringStdLibParsingOnWindows", null, null));
    } else {
      collectDistribution(s, "Base");
    }
    return s;
  }

  private static File file(File dir, String... relative) {
    var f = dir;
    for (var ch : relative) {
      f = new File(f, ch);
    }
    return f;
  }

  private static Path locateDistribution(final String name) throws URISyntaxException {
    var where =
        new File(ParseStdLibTest.class.getProtectionDomain().getCodeSource().getLocation().toURI());
    var dir = where;
    for (; ; ) {
      dir = file(where, "distribution", "lib", "Standard", name, "0.0.0-dev", "src");
      if (dir.exists()) {
        break;
      }
      where = where.getParentFile();
    }
    return dir.toPath();
  }

  private static void collectDistribution(TestSuite s, String name) throws Exception {
    var dir = locateDistribution(name);
    var dump = new Dump();
    class CollectSuites implements FileVisitor<Path> {

      private final TestSuite suite;

      CollectSuites(TestSuite suite) {
        this.suite = suite;
      }

      @Override
      public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs)
          throws IOException {
        return FileVisitResult.CONTINUE;
      }

      @Override
      public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
        if (!file.getFileName().toString().endsWith(".enso")) {
          return FileVisitResult.CONTINUE;
        }
        final String name = file.toFile().getPath().substring(dir.toFile().getPath().length() + 1);
        suite.addTest(new ParseStdLibTest(name, file.toFile(), dump));
        return FileVisitResult.CONTINUE;
      }

      @Override
      public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
        return FileVisitResult.CONTINUE;
      }

      @Override
      public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
        return FileVisitResult.CONTINUE;
      }
    }
    Files.walkFileTree(dir, new CollectSuites(s));
  }

  @SuppressWarnings("unchecked")
  private void parseTest(Source src, boolean generate) throws IOException {
    var ir = ensoCompiler.compile(src);
    assertNotNull("IR was generated", ir);
  }

  @Override
  public void runBare() throws Throwable {
    if (where == null) {
      return;
    }
    var code = Files.readString(where.toPath());
    var src = Source.newBuilder("enso", code, getName()).uri(where.toURI()).build();
    parseTest(src, true);
  }

  private static final class Dump {
    private boolean first = true;

    public void dump(File where, CharSequence old, CharSequence now) throws IOException {
      var name = where.getName();
      var result = where.getParentFile().toPath();
      final Path oldPath = result.resolve(name + ".old");
      Files.writeString(
          oldPath,
          old,
          StandardOpenOption.TRUNCATE_EXISTING,
          StandardOpenOption.CREATE,
          StandardOpenOption.WRITE);
      final Path nowPath = result.resolve(name + ".now");
      Files.writeString(
          nowPath,
          now,
          StandardOpenOption.TRUNCATE_EXISTING,
          StandardOpenOption.CREATE,
          StandardOpenOption.WRITE);
      if (first) {
        first = false;
        fail(
            "IR for "
                + where.getName()
                + " shall be equal:\n$ diff -u '"
                + oldPath
                + "' '"
                + nowPath
                + "'\n ===== Old =====\n"
                + old
                + "\n===== Now =====\n"
                + now);
      }
      fail(
          "IR for "
              + where.getName()
              + " shall be equal:\n$ diff -u '"
              + oldPath
              + "' '"
              + nowPath
              + "'");
    }
  }
}
