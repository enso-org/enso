package org.enso.compiler;

import com.oracle.truffle.api.source.Source;
import java.io.File;
import org.enso.compiler.core.IR;
import org.junit.AfterClass;
import org.junit.BeforeClass;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.function.Function;
import static org.junit.Assert.assertEquals;

import static org.junit.Assert.assertNotNull;

public abstract class CompilerTest {

  protected static EnsoCompiler ensoCompiler;

  @BeforeClass
  public static void initEnsoCompiler() {
    ensoCompiler = new EnsoCompiler();
  }

  @AfterClass
  public static void closeEnsoCompiler() throws Exception {
    ensoCompiler.close();
  }

  protected static IR.Module parse(String code) throws IOException {
    var src =
        Source.newBuilder("enso", code, "test-" + Integer.toHexString(code.hashCode()) + ".enso")
            .build();
    IR.Module ir = ensoCompiler.compile(src);
    assertNotNull("IR was generated", ir);
    return ir;
  }

  static void assertIR(String msg, IR.Module old, IR.Module now) throws IOException {
    Function<IR, String> filter = f -> EnsoCompilerTest.simplifyIR(f, true, true, false);
    String ir1 = filter.apply(old);
    String ir2 = filter.apply(now);
    if (!ir1.equals(ir2)) {
      String name = findTestMethodName();
      Path home = new File(System.getProperty("user.home")).toPath();
      Files.writeString(home.resolve(name + ".1"), ir1, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
      Files.writeString(home.resolve(name + ".2"), ir2, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
      assertEquals(msg, ir1, ir2);
    }
  }

  private static String findTestMethodName() {
    for (var e : new Exception().getStackTrace()) {
      if (e.getMethodName().startsWith("test")) {
        return e.getMethodName();
      }
    }
    throw new IllegalStateException();
  }
}
