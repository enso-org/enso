package org.enso.compiler.core;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ForkJoinPool;
import org.enso.compiler.core.ir.Module;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class EnsoParserMultiThreadedTest {
  private EnsoParser parser;

  @Before
  public void initParser() throws Exception {
    parser = new EnsoParser();
  }

  @After
  public void closeParser() throws Exception {
    parser.close();
  }

  @Test
  public void stressLoadFromManyThreads() throws Exception {
    List<Callable<Module>> cases = new ArrayList<>();
    final var testCases = 1000;
    for (var i = 0; i < 2 * testCases; i++) {
      var number = i % testCases;
      var code =
          """
            from Standard.Base import all
            main = %n
            """
              .replace("%n", "" + number);
      cases.add(
          () -> {
            return parser.compile(code);
          });
    }

    var results = ForkJoinPool.commonPool().invokeAll(cases);

    for (var i = 0; i < testCases; i++) {
      var r1 = results.get(i).get();
      var r2 = results.get(testCases + i).get();

      EnsoParserTest.assertIR("Run #" + i + " should produce identical IR", r1, r2);
    }
  }
}
