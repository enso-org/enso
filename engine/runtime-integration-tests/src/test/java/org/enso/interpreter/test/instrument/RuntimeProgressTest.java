package org.enso.interpreter.test.instrument;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.nio.file.Paths;
import java.util.UUID;
import org.enso.common.LanguageInfo;
import org.enso.common.RuntimeOptions;
import org.enso.interpreter.runtime.type.ConstantsGen;
import org.enso.interpreter.test.Metadata;
import org.enso.polyglot.RuntimeServerInfo;
import org.enso.polyglot.runtime.Runtime$Api$CreateContextRequest;
import org.enso.polyglot.runtime.Runtime$Api$CreateContextResponse;
import org.enso.polyglot.runtime.Runtime$Api$ExpressionUpdate;
import org.enso.polyglot.runtime.Runtime$Api$ExpressionUpdate$Payload$Pending;
import org.enso.polyglot.runtime.Runtime$Api$ExpressionUpdates;
import org.enso.polyglot.runtime.Runtime$Api$InitializedNotification;
import org.enso.polyglot.runtime.Runtime$Api$MethodCall;
import org.enso.polyglot.runtime.Runtime$Api$MethodPointer;
import org.enso.polyglot.runtime.Runtime$Api$OpenFileRequest;
import org.enso.polyglot.runtime.Runtime$Api$OpenFileResponse$;
import org.enso.polyglot.runtime.Runtime$Api$PushContextRequest;
import org.enso.polyglot.runtime.Runtime$Api$PushContextResponse;
import org.enso.polyglot.runtime.Runtime$Api$Request;
import org.enso.polyglot.runtime.Runtime$Api$Response;
import org.enso.polyglot.runtime.Runtime$Api$StackItem$ExplicitCall;
import org.graalvm.polyglot.Context;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import scala.Option;
import scala.collection.immutable.List;
import scala.collection.immutable.Set;
import scala.collection.immutable.Set$;
import scala.collection.immutable.Vector$;
import scala.collection.immutable.Vector1;

public class RuntimeProgressTest {
  private static final String MODULE_NAME = "Enso_Test.Test.Main";
  private TestContext context;
  private File mainFile;

  @Before
  public void initializeContext() {
    context = new TestContext("Test");
    context.init();
    var initResponse = context.receive().get();
    assertEquals(Response(new Runtime$Api$InitializedNotification()), initResponse);
  }

  @After
  public void teardownContext() {
    context.close();
    context = null;
    mainFile = null;
  }

  @Test
  public void reportAProgress() {

    var contextId = UUID.randomUUID();
    var requestId = UUID.randomUUID();
    var metadata = new Metadata("");
    var mainRes = metadata.addItem(56, 5, "eeee");

    var code =
        """
        import Standard.Base.Logging.Progress

        main =
            res = steps
            res

        steps = Progress.run "Six steps" 6 progress->
            progress.advance 1
            progress.advance 2
            progress.advance 3
            10
        """;

    metadata.assertInCode(mainRes, code, "steps");
    var contents = metadata.appendToCode(code);
    this.mainFile = context.writeMain(contents);

    // create context
    var request = Request(requestId, new Runtime$Api$CreateContextRequest(contextId));
    context.send(request);
    var response = context.receive().get();
    assertEquals(response, Response(requestId, new Runtime$Api$CreateContextResponse(contextId)));
    // Open the new file
    context.send(Request(requestId, new Runtime$Api$OpenFileRequest(mainFile, contents)));
    response = context.receive().get();
    assertEquals(response, Response(requestId, Runtime$Api$OpenFileResponse$.MODULE$));

    context.send(
        Request(
            requestId,
            new Runtime$Api$PushContextRequest(
                contextId,
                new Runtime$Api$StackItem$ExplicitCall(
                    new Runtime$Api$MethodPointer(MODULE_NAME, "Enso_Test.Test.Main", "main"),
                    Option.empty(),
                    new Vector1<>(new String[] {"0"})),
                true)));

    var reply1 = context.receiveNIgnoreStdLib(9, 60);
    assertSameElements(
        reply1,
        Response(requestId, new Runtime$Api$PushContextResponse(contextId)),
        progressPayload(contextId, mainRes, -1.0, null),
        progressPayload(contextId, mainRes, -1.0, null),
        progressPayload(contextId, mainRes, 0.0, "Six steps"),
        progressPayload(contextId, mainRes, 1.0 / 6.0, "Six steps"), // one sixth of work
        progressPayload(contextId, mainRes, 0.5, "Six steps"), // half of work
        progressPayload(contextId, mainRes, 1.0, "Six steps"), // all of work
        TestMessages.update(
            contextId,
            mainRes,
            ConstantsGen.INTEGER,
            new Runtime$Api$MethodCall(
                new Runtime$Api$MethodPointer(
                    "Enso_Test.Test.Main", "Enso_Test.Test.Main", "steps"),
                Vector$.MODULE$.empty())),
        context.executionComplete(contextId));
  }

  @Test
  public void reportAbitOfProgressAndMessage() {

    var contextId = UUID.randomUUID();
    var requestId = UUID.randomUUID();
    var metadata = new Metadata("");
    var mainRes = metadata.addItem(56, 5, "eeee");

    var code =
        """
        import Standard.Base.Logging.Progress

        main =
            res = steps
            res

        steps = Progress.run "Few steps" 5 progress->
            progress.advance
            progress.log "I've just finished 1st step"
            progress.advance 3
            progress.log "Returning a value"
            42
        """;

    metadata.assertInCode(mainRes, code, "steps");
    var contents = metadata.appendToCode(code);
    this.mainFile = context.writeMain(contents);

    // create context
    var request = Request(requestId, new Runtime$Api$CreateContextRequest(contextId));
    context.send(request);
    var response = context.receive().get();
    assertEquals(response, Response(requestId, new Runtime$Api$CreateContextResponse(contextId)));
    // Open the new file
    context.send(Request(requestId, new Runtime$Api$OpenFileRequest(mainFile, contents)));
    response = context.receive().get();
    assertEquals(response, Response(requestId, Runtime$Api$OpenFileResponse$.MODULE$));

    context.send(
        Request(
            requestId,
            new Runtime$Api$PushContextRequest(
                contextId,
                new Runtime$Api$StackItem$ExplicitCall(
                    new Runtime$Api$MethodPointer(MODULE_NAME, "Enso_Test.Test.Main", "main"),
                    Option.empty(),
                    new Vector1<>(new String[] {"0"})),
                true)));

    var reply1 = context.receiveNIgnoreStdLib(11, 60);
    assertSameElements(
        reply1,
        Response(requestId, new Runtime$Api$PushContextResponse(contextId)),
        progressPayload(contextId, mainRes, -1.0, null),
        progressPayload(contextId, mainRes, -1.0, null),
        progressPayload(contextId, mainRes, 0.0, "Few steps"),
        progressPayload(contextId, mainRes, 0.2, "Few steps"), // 20% of work
        progressPayload(contextId, mainRes, 0.2, "I've just finished 1st step"), // new message
        progressPayload(contextId, mainRes, 0.8, "I've just finished 1st step"), // 80 % of work
        progressPayload(contextId, mainRes, 0.8, "Returning a value"), // another message
        progressPayload(contextId, mainRes, 1.0, "Returning a value"), // all of work at the end
        TestMessages.update(
            contextId,
            mainRes,
            ConstantsGen.INTEGER,
            new Runtime$Api$MethodCall(
                new Runtime$Api$MethodPointer(
                    "Enso_Test.Test.Main", "Enso_Test.Test.Main", "steps"),
                Vector$.MODULE$.empty())),
        context.executionComplete(contextId));
  }

  private static void assertSameElements(
      List<Runtime$Api$Response> actual, Runtime$Api$Response... seq) {
    assertEquals("Same size: " + actual, seq.length, actual.size());
    for (int i = 0; i < seq.length; i++) {
      var real = actual.drop(i).head();
      assertEquals("Check on #" + i, seq[i], real);
    }
  }

  private static Runtime$Api$Response progressPayload(
      UUID contextId, UUID id, double amount, String msg) {
    var pending =
        new Runtime$Api$ExpressionUpdate$Payload$Pending(
            Option.apply(msg), Option.apply(amount), false);
    var up =
        new Runtime$Api$ExpressionUpdate(
            id, Option.empty(), Option.empty(), Vector$.MODULE$.empty(), false, false, pending);
    var updates = new Runtime$Api$ExpressionUpdates(contextId, singletonSet(up));
    return new Runtime$Api$Response(Option.empty(), updates);
  }

  @SuppressWarnings("unchecked")
  private static <T> Set<T> singletonSet(T one) {
    var b = Set$.MODULE$.newBuilder();
    b.addOne(one);
    return (Set<T>) b.result();
  }

  private static Runtime$Api$Request Request(
      UUID id, org.enso.polyglot.runtime.Runtime.ApiRequest request) {
    return org.enso.polyglot.runtime.Runtime$Api$Request$.MODULE$.apply(id, request);
  }

  private static Runtime$Api$Response Response(
      org.enso.polyglot.runtime.Runtime.ApiResponse request) {
    return org.enso.polyglot.runtime.Runtime$Api$Response$.MODULE$.apply(request);
  }

  private static Runtime$Api$Response Response(
      UUID id, org.enso.polyglot.runtime.Runtime.ApiResponse request) {
    return org.enso.polyglot.runtime.Runtime$Api$Response$.MODULE$.apply(id, request);
  }

  private static final class TestContext extends InstrumentTestContext {

    private Context _context;

    TestContext(String packageName) {
      super(packageName);
    }

    @Override
    public Context context() {
      if (_context == null) {
        _context =
            Context.newBuilder(LanguageInfo.ID)
                .allowExperimentalOptions(true)
                .allowAllAccess(true)
                .option(RuntimeOptions.PROJECT_ROOT, pkg().root().getAbsolutePath())
                .option(RuntimeOptions.LOG_LEVEL, java.util.logging.Level.WARNING.getName())
                .option(RuntimeOptions.CHECK_CWD, "false")
                .option(RuntimeOptions.INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION, "true")
                .option(RuntimeOptions.ENABLE_PROJECT_SUGGESTIONS, "false")
                .option(RuntimeOptions.ENABLE_PROGRESS_REPORT, "true")
                .option(RuntimeOptions.ENABLE_GLOBAL_SUGGESTIONS, "false")
                .option(RuntimeOptions.ENABLE_EXECUTION_TIMER, "false")
                .option(RuntimeOptions.STRICT_ERRORS, "false")
                .option(RuntimeOptions.DISABLE_IR_CACHES, "true")
                .option(RuntimeServerInfo.ENABLE_OPTION, "true")
                .option(RuntimeOptions.INTERACTIVE_MODE, "true")
                .option(
                    RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                    Paths.get("../../distribution/component").toFile().getAbsolutePath())
                .option(RuntimeOptions.EDITION_OVERRIDE, "0.0.0-dev")
                .serverTransport(runtimeServerEmulator().makeServerTransport())
                .build();
      }
      return _context;
    }

    @Override
    public void close() {
      super.close();
      if (_context != null) {
        _context.close();
        _context = null;
      }
    }
  }
}
