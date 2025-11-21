package org.enso.interpreter.runtime.progress;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.stream.Collectors;
import org.enso.common.MethodNames;
import org.enso.logger.ObservedMessage;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.junit.ClassRule;
import org.junit.Test;
import org.slf4j.LoggerFactory;

public class ProgressTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

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

    var geom = ctxRule.eval("enso", code).invokeMember(MethodNames.Module.EVAL_EXPRESSION, "geom");

    var oneTimeLog =
        ObservedMessage.collect(
            log,
            () -> {
              var r1 = geom.execute(1, 2.0, 0.5);
              assertEquals("Only two", 2.0, r1.asDouble(), 0.001);
            });
    assertEquals("One time: " + oneTimeLog, 4, oneTimeLog.size());
    assertEquals("INIT {}:{}@{}", oneTimeLog.get(0).getMessage());
    var progressHandle = oneTimeLog.get(0).getArguments().get(0);
    assertEquals("geometric sequence", oneTimeLog.get(0).getArguments().get(1));
    assertEquals(1L, oneTimeLog.get(0).getArguments().get(2));
    assertEquals("LOG {}:{}", oneTimeLog.get(1).getMessage());
    assertEquals(progressHandle, oneTimeLog.get(0).getArguments().get(0));
    assertEquals(
        "About to compute geometric sequence for 1", oneTimeLog.get(1).getArguments().get(1));
    assertEquals("LOG {}:{}", oneTimeLog.get(2).getMessage());
    assertEquals(progressHandle, oneTimeLog.get(2).getArguments().get(0));
    assertEquals("We have the result 2.0", oneTimeLog.get(2).getArguments().get(1));
    assertEquals("ADVANCE {}+{}", oneTimeLog.get(3).getMessage());
    assertEquals(progressHandle, oneTimeLog.get(0).getArguments().get(0));
    assertEquals(1L, oneTimeLog.get(3).getArguments().get(1));

    var r2 = geom.execute(2, 2.0, 0.5);
    assertEquals("Three", 3.0, r2.asDouble(), 0.001);
    var r3 = geom.execute(3, 2.0, 0.5);
    assertEquals("Three and half", 3.5, r3.asDouble(), 0.001);

    var fiftyTimes =
        ObservedMessage.collect(
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
    var upTo = ctxRule.eval("enso", code).invokeMember(MethodNames.Module.EVAL_EXPRESSION, "up_to");

    var log = LoggerFactory.getLogger("Standard.Base.Logging.Progress");

    var msgs =
        ObservedMessage.collect(
            log,
            () -> {
              var fac5 = upTo.execute(5, acc);
              assertEquals(120, fac5.asInt());
            });

    assertEquals("Seven messsages " + msgs, 7, msgs.size());
    var txt =
        msgs.stream().map(ObservedMessage::getFormattedMessage).collect(Collectors.joining("\n"));

    assertTrue("Initialization first", msgs.get(0).getMessage().startsWith("INIT "));

    assertEquals(
        "Initialize five steps. Then five `advance` calls and finally advance to finish.",
        """
        INIT Progress:from 0 to 5@5
        ADVANCE Progress+1
        ADVANCE Progress+1
        ADVANCE Progress+1
        ADVANCE Progress+1
        ADVANCE Progress+1
        ADVANCE Progress+5\
        """,
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
    // ideally this class would
    // implement BiConsumer<Long, Progress>, Supplier<Long>
    // but that isn't working well with Truffle builtin host interop
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

  /**
   * Demonstrates logging fully managed from Java code.
   *
   * <p>Because the underlaying <em>progress reporting</em> infrastructure relies on logging, it is
   * possible to perform whole progress manipulation just from Java. All that is needed is to
   * simulate the same messages and send them to {@code Standard.Base.Logging.Progress} logger as
   * this method shows.
   */
  private static int showHowToLogDirectlyFromJavaWhileComputingFactorial(int n) {
    var log = LoggerFactory.getLogger("Standard.Base.Logging.Progress");
    var progressHandle =
        new Object() {
          @Override
          public String toString() {
            return "JavaProgress";
          }
        };
    log.trace("INIT {}:{}@{}", progressHandle, "Logging progress fully from Java", 5);

    var mul = 1;
    for (var i = 1; i <= n; i++) {
      log.trace("ADVANCE {}+{}", progressHandle, 1);
      mul *= i;
    }

    log.trace("ADVANCE {}+{}", progressHandle, 5);
    return mul;
  }

  @Test
  public void createNewProgressInJava() {
    var code =
        """
        from Standard.Base import Integer, Float
        from Standard.Base.Logging import Progress

        up_to n host =
            host n
        """;
    var upTo = ctxRule.eval("enso", code).invokeMember(MethodNames.Module.EVAL_EXPRESSION, "up_to");

    var log = LoggerFactory.getLogger("Standard.Base.Logging.Progress");

    var javaMethod =
        (ProxyExecutable)
            (Value... arguments) -> {
              return showHowToLogDirectlyFromJavaWhileComputingFactorial(arguments[0].asInt());
            };

    var msgs =
        ObservedMessage.collect(
            log,
            () -> {
              var fac5 = upTo.execute(5, javaMethod);
              assertEquals(120, fac5.asInt());
            });

    assertEquals("Seven messsages " + msgs, 7, msgs.size());
    var txt =
        msgs.stream().map(ObservedMessage::getFormattedMessage).collect(Collectors.joining("\n"));

    assertTrue("Initialization first", msgs.get(0).getMessage().startsWith("INIT "));

    assertEquals(
        "Initialize five steps. Then five `advance` calls and finally advance to finish.",
        """
        INIT JavaProgress:Logging progress fully from Java@5
        ADVANCE JavaProgress+1
        ADVANCE JavaProgress+1
        ADVANCE JavaProgress+1
        ADVANCE JavaProgress+1
        ADVANCE JavaProgress+1
        ADVANCE JavaProgress+5\
        """,
        txt);
  }
}
