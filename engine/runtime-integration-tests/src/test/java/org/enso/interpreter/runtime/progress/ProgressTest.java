package org.enso.interpreter.runtime.progress;

import static org.junit.Assert.assertEquals;

import java.util.stream.Collectors;
import org.enso.common.MethodNames;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.logger.LoggerMessage;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.LoggerFactory;

public class ProgressTest {
  private static Context ctx;
  private static EnsoContext ensoCtx;

  @BeforeClass
  public static void initCtx() throws Exception {
    ctx = ContextUtils.createDefaultContext();
    ensoCtx = ContextUtils.leakContext(ctx);
  }

  @AfterClass
  public static void closeCtx() throws Exception {
    ctx.close();
    ctx = null;
  }

  public ProgressTest() {}

  @Test
  @SuppressWarnings("unchecked")
  public void advanceMultipleTimes() throws Exception {
    var code =
        """
    from Standard.Base import Integer, Float
    from Standard.Base.Logging import Progress

    geom n:Integer a1:Float q:Float =
        Progress.run "geometric sequence" n progress->
            loop i:Integer v:Float acc:Float =
                if i == n then acc else
                    progress.log "Step #"+i.to_text
                    next = v*q
                    sum = next+acc
                    progress.advance
                    @Tail_Call loop i+1 next sum

            progress.log "About to compute geometric sequence for "+n.to_text
            res = loop 1 a1 a1
            progress.log "We have the result "+res.to_text
            res
    """;
    var log = LoggerFactory.getLogger("Standard.Base.Logging.Progress");

    var geom = ctx.eval("enso", code).invokeMember(MethodNames.Module.EVAL_EXPRESSION, "geom");

    var oneTimeLog =
        LoggerMessage.collect(
            log,
            () -> {
              var r1 = geom.execute(1, 2.0, 0.5);
              assertEquals("Only two", 2.0, r1.asDouble(), 0.001);
            });
    assertEquals("One time: " + oneTimeLog, 4, oneTimeLog.size());
    assertEquals("geometric sequence@1", oneTimeLog.get(0).msg());
    assertEquals(
        "geometric sequence:About to compute geometric sequence for 1", oneTimeLog.get(1).msg());
    assertEquals("geometric sequence:We have the result 2.0", oneTimeLog.get(2).msg());
    assertEquals("geometric sequence+1", oneTimeLog.get(3).msg());

    var r2 = geom.execute(2, 2.0, 0.5);
    assertEquals("Three", 3.0, r2.asDouble(), 0.001);
    var r3 = geom.execute(3, 2.0, 0.5);
    assertEquals("Three and half", 3.5, r3.asDouble(), 0.001);

    var fiftyTimes =
        LoggerMessage.collect(
            log,
            () -> {
              var r4 = geom.execute(50, 2.0, 0.5);
              assertEquals("Got almost four", 4.0, r4.asDouble(), 0.001);
            });
    assertEquals("50*2 + 2: " + fiftyTimes, 102, fiftyTimes.size());
  }

  @Test
  public void useExistingProgressFromJava() throws Exception {
    performExistingProgressFromJavaWith(new Accumulator(1));
  }

  @Test
  public void useExistingProgressFromJavaViaProgressInterface() throws Exception {
    performExistingProgressFromJavaWith(new AccumulatorWithProgress(1));
  }

  /**
   * Expecting {@code acc} to have methods {@code accumulate} (two arguments) and {@code result} (no
   * argument).
   */
  private void performExistingProgressFromJavaWith(Object acc) {
    var code =
        """
    from Standard.Base import Integer, Float
    from Standard.Base.Logging import Progress

    up_to n combine =
        Progress.run "from 0 to "+n.to_text n progress->
            loop count_down =
                if count_down <= 0 then combine.result else
                    combine.accumulate count_down progress
                    @Tail_Call loop count_down-1

            loop n
    """;
    var upTo = ctx.eval("enso", code).invokeMember(MethodNames.Module.EVAL_EXPRESSION, "up_to");

    var log = LoggerFactory.getLogger("Standard.Base.Logging.Progress");

    var msgs =
        LoggerMessage.collect(
            log,
            () -> {
              var fac5 = upTo.execute(5, acc);
              assertEquals(120, fac5.asInt());
            });

    assertEquals("Seven messsages " + msgs, 7, msgs.size());
    var txt = msgs.stream().map(LoggerMessage::msg).collect(Collectors.joining("\n"));
    assertEquals(
        "Initialize five steps. Then five `advance` calls and finally advance to finish.",
        """
        from 0 to 5@5
        from 0 to 5+1
        from 0 to 5+1
        from 0 to 5+1
        from 0 to 5+1
        from 0 to 5+1
        from 0 to 5+5""",
        txt);
  }

  public static interface Progress {
    public void advance(int steps);

    public void log(String detail);
  }

  public static final class Accumulator {
    private long mul;

    private Accumulator(long mul) {
      this.mul = mul;
    }

    public void accumulate(Long t, Value progress) {
      mul *= t;
      progress.invokeMember("advance", 1);
    }

    public Long result() {
      return mul;
    }
  }

  public static final class AccumulatorWithProgress {
    // implements BiConsumer<Long, Progress>, Supplier<Long> {
    private long mul;

    private AccumulatorWithProgress(long mul) {
      this.mul = mul;
    }

    public void accumulate(Long t, Progress progress) {
      mul *= t;
      progress.advance(1);
    }

    public Long result() {
      return mul;
    }
  }
}
