package org.enso.interpreter.caches;

import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.util.logging.Level;
import java.util.stream.Collectors;
import org.enso.common.RuntimeOptions;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Source;
import org.junit.Test;

public class HelloWorldCacheTest {

  @Test
  public void loadingHelloWorldTwiceUsesCaching() throws Exception {
    var root = new File("../..").getAbsoluteFile();
    assertTrue("build.sbt exists at " + root, new File(root, "build.sbt").exists());
    var helloWorld = children(root, "test", "Benchmarks", "src", "Startup", "Hello_World.enso");
    assertTrue("Hello_World.enso found", helloWorld.exists());

    // the first run may or may not use caches
    var firstMsgs = executeOnce(helloWorld);
    assertTrue("Contains hello world:\n" + firstMsgs, firstMsgs.endsWith("Hello World"));
    // after the first run the caches for Hello_World.enso must be generated

    // the second run must read Hello_World from its .ir file!
    var secondMsgs = executeOnce(helloWorld);
    assertTrue("Contains hello world:\n" + secondMsgs, secondMsgs.contains("Hello World"));
    assertTrue(
        "Properly deserialized:\n" + secondMsgs,
        secondMsgs.contains("Deserializing module Hello_World from IR file: true"));
  }

  private static String executeOnce(File src) throws Exception {
    var backLog = new ByteArrayOutputStream();
    var log = new PrintStream(backLog);

    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .out(log)
            .err(log)
            .logHandler(log)
            .option(RuntimeOptions.LOG_LEVEL, Level.FINE.getName())
            .option(RuntimeOptions.DISABLE_IR_CACHES, "false")
            .option(RuntimeOptions.PROJECT_ROOT, findBenchmarks(src).getAbsolutePath())
            .build()) {
      var code = Source.newBuilder("enso", src).build();
      var res = ContextUtils.evalModule(ctx, code, "main");
      assertTrue("Result of IO.println is Nothing", res.isNull());
    }
    return backLog
        .toString()
        .lines()
        .filter(l -> l.toUpperCase().contains("HELLO"))
        .collect(Collectors.joining("\n"));
  }

  private static File children(File f, String... names) {
    for (var n : names) {
      f = new File(f, n);
    }
    return f;
  }

  private static File findBenchmarks(File f) {
    for (; ; ) {
      if (f.getName().equals("Benchmarks")) {
        return f;
      }
      f = f.getParentFile();
    }
  }
}
