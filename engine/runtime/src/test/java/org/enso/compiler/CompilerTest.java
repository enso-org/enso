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
    Function<IR, String> filter = f -> simplifyIR(f, true, true, false);
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

  private static String simplifyIR(IR i, boolean noIds, boolean noLocations, boolean lessDocs) {
    String txt = i.pretty();
    if (noIds) {
      txt = txt.replaceAll("[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\-[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]", "_");
    }
    if (noLocations) {
      for (;;) {
        final String pref = " Location(";
        int at = txt.indexOf(pref);
        if (at == -1) {
          break;
        }
        int to = at + pref.length();
        int depth = 1;
        while (depth > 0) {
          switch (txt.charAt(to)) {
            case '(' -> depth++;
            case ')' -> depth--;
          }
          to++;
        }
        txt = txt.substring(0, at) + "Location[_]" + txt.substring(to);
      }
    }
    if (lessDocs) {
      for (;;) {
        final String pref = "IR.Comment.Documentation(";
        int at = txt.indexOf(pref);
        if (at == -1) {
          break;
        }
        int to = txt.indexOf("location =", at + pref.length());
        txt = txt.substring(0, at) + "IR.Comment.Doc(" + txt.substring(to);
      }
      for (;;) {
        final String pref = "IR.Case.Pattern.Doc(";
        int at = txt.indexOf(pref);
        if (at == -1) {
          break;
        }
        int to = txt.indexOf("location =", at + pref.length());
        txt = txt.substring(0, at) + "IR.Comment.CaseDoc(" + txt.substring(to);
      }
    }
    for (;;) {
      final String pref = "IR.Error.Syntax(";
      int at = txt.indexOf(pref);
      if (at == -1) {
        break;
      }
      int to = txt.indexOf("reason =", at + pref.length());
      txt = txt.substring(0, at) + "IR.Error.Syntax (" + txt.substring(to);
    }
    return txt;
  }
}
