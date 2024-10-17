package org.enso.compiler.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.function.Function;
import org.enso.compiler.core.EnsoParser;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;

public abstract class CompilerTests {
  protected static Module parse(CharSequence code) {
    Module ir = EnsoParser.compile(code);
    assertNotNull("IR was generated", ir);
    return ir;
  }

  protected static Expression.Block parseBlock(CharSequence code) {
    Expression.Block ir = EnsoParser.compileBlock(code);
    assertNotNull("IR was generated", ir);
    return ir;
  }

  public static void assertIR(String msg, IR old, IR now) throws IOException {
    assertEqualsIR(msg, null, old, now);
  }

  public static void assertEqualsIR(String msg, String testName, IR old, IR now)
      throws IOException {
    Function<IR, String> filter = f -> simplifyIR(f, true, true, false);
    String ir1 = filter.apply(old);
    String ir2 = filter.apply(now);
    if (!ir1.equals(ir2)) {
      var home = new File(System.getProperty("java.io.tmpdir")).toPath();
      var fname = testName == null ? findTestMethodName() : testName;
      var file1 = home.resolve(fname + ".1");
      var file2 = home.resolve(fname + ".2");
      Files.writeString(
          file1,
          ir1,
          StandardOpenOption.TRUNCATE_EXISTING,
          StandardOpenOption.CREATE,
          StandardOpenOption.WRITE);
      Files.writeString(
          file2,
          ir2,
          StandardOpenOption.TRUNCATE_EXISTING,
          StandardOpenOption.CREATE,
          StandardOpenOption.WRITE);
      fail("IRs contained in files " + file1 + " and " + file2 + " should equal: " + msg);
    }
  }

  private static int counter = 'A';

  private static String findTestMethodName() {
    for (var e : new Exception().getStackTrace()) {
      if (e.getMethodName().startsWith("test")) {
        return e.getMethodName();
      }
    }
    return "test" + (char) counter++;
  }

  /**
   * Takes an {@link IR} and converts it to text representation suitable for "diffing" while
   * "simplifying" it.
   *
   * @param ir the intermediate representation
   * @param noIds remove all UUIDs or keep them? Multiple runs usually assign random/different UUIDs
   *     to various IR elements. Removing them is a best way to make the converted text comparable
   * @param noLocations locations may slightly differ. Usually off-by-one. Especially when running
   *     old and new parser in parallel - removing them may be useful
   * @param lessDocs documentation often isn't an essential part of the IR one can easily remove it
   *     by specifying {@code false}
   * @return string representation of the IR
   */
  private static String simplifyIR(IR ir, boolean noIds, boolean noLocations, boolean lessDocs) {
    String txt = ir.duplicate(!noLocations, true, true, !noIds).pretty();
    if (noIds) {
      txt =
          txt.replaceAll(
              "[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\-[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]",
              "_");
    }
    if (noLocations) {
      for (; ; ) {
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
      for (; ; ) {
        final String pref = "Comment.Documentation(";
        int at = txt.indexOf(pref);
        if (at == -1) {
          break;
        }
        int to = txt.indexOf("location =", at + pref.length());
        txt = txt.substring(0, at) + "Comment.Doc(" + txt.substring(to);
      }
      for (; ; ) {
        final String pref = "Case.Pattern.Doc(";
        int at = txt.indexOf(pref);
        if (at == -1) {
          break;
        }
        int to = txt.indexOf("location =", at + pref.length());
        txt = txt.substring(0, at) + "Comment.CaseDoc(" + txt.substring(to);
      }
    }
    for (; ; ) {
      final String pref = "errors.Syntax(";
      int at = txt.indexOf(pref);
      if (at == -1) {
        break;
      }
      int to = txt.indexOf("reason =", at + pref.length());
      txt = txt.substring(0, at) + "errors.Syntax (" + txt.substring(to);
    }
    for (; ; ) {
      final String pref = "List(";
      int at = txt.indexOf(pref);
      if (at == -1) {
        break;
      }
      int to = at + pref.length();
      txt = txt.substring(0, at) + "Seq(" + txt.substring(to);
    }
    return txt;
  }
}
