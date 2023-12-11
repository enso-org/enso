package org.enso.interpreter.test;

import static org.junit.Assert.*;

import com.oracle.truffle.api.interop.InteropLibrary;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.*;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class VerifyArrowTest {
  private static Context ctx;
  private static Handler handler;

  private static final InteropLibrary interop = InteropLibrary.getUncached();

  @BeforeClass
  public static void initEnsoContext() {
    handler = new MockHandler();
    ctx =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .allowIO(IOAccess.ALL)
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .option(RuntimeOptions.LOG_LEVEL, Level.FINEST.getName())
            .logHandler(handler)
            .out(System.out)
            .err(System.err)
            .allowAllAccess(true)
            .build();
    assertNotNull("Enso language is supported", ctx.getEngine().getLanguages().get("enso"));
    var fourtyTwo =
        ctx.eval("enso", "mul x y = x * y").invokeMember("eval_expression", "mul").execute(6, 7);
    assertEquals(42, fourtyTwo.asInt());
  }

  @AfterClass
  public static void closeEnsoContext() throws Exception {
    if (ctx != null) {
      ctx.close();
    }
  }

  @Test
  public void arrowDate32() {
    var arrow = ctx.getEngine().getLanguages().get("arrow");
    assertNotNull("Arrow is available", arrow);
    var date32Constr = ctx.eval("arrow", "new[Date32]");

    Value date32Array = date32Constr.newInstance((long) 10);
    assertNotNull("allocated value should not be null", date32Array);
    assertTrue("allocated value should be an array", date32Array.hasArrayElements());
    // assertTrue("allocated value should be an array", interop.hasArrayElements()); // false?
    var startDate = LocalDate.now();
    populateArrayWithConsecutiveDays(date32Array, startDate);
    var dayPlus2 = date32Array.getArrayElement(2);
    assertEquals(startDate.plusDays(2), dayPlus2.asDate());
  }

  @Test
  public void arrowDate64() {
    var arrow = ctx.getEngine().getLanguages().get("arrow");
    assertNotNull("Arrow is available", arrow);
    var date64Constr = ctx.eval("arrow", "new[Date64]");

    Value date64Array = date64Constr.newInstance((long) 10);
    assertNotNull("allocated value should not be null", date64Array);
    assertTrue("allocated value should be an array", date64Array.hasArrayElements());
    // assertTrue("allocated value should be an array", interop.hasArrayElements()); // false?
    var startDate = LocalDate.now(ZoneId.of("Europe/Paris"));
    populateArrayWithConsecutiveDays(date64Array, startDate);
    var dayPlus2 = date64Array.getArrayElement(2);
    assertEquals(startDate.plusDays(2), dayPlus2.asDate());
  }

  private void populateArrayWithConsecutiveDays(Value arr, LocalDate startDate) {
    var len = arr.getArraySize();
    for (int i = 0; i < len; i++) {
      arr.setArrayElement(i, startDate.plusDays(i));
    }
  }

  private static class MockHandler extends Handler {
    private final Formatter fmt = new SimpleFormatter();
    private final List<LogRecord> records = new ArrayList<>();
    private String failMsg;
    private Error failure;

    public MockHandler() {}

    public void failOnMessage(String msg) {
      this.failMsg = msg;
    }

    @Override
    public void publish(LogRecord lr) {
      records.add(lr);
      var msg = fmt.formatMessage(lr);
      if (failMsg != null && failMsg.equals(msg)) {
        failure = new AssertionError(this.toString() + "\nGot forbidden message: " + msg);
      }
    }

    @Override
    public void flush() {}

    @Override
    public void close() throws SecurityException {}

    @Override
    public String toString() {
      var sb = new StringBuilder();
      for (var r : records) {
        sb.append("\n").append(fmt.formatMessage(r));
      }
      return sb.toString();
    }
  }
}
