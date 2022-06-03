package org.enso.interpreter.test.instrument;

import java.util.UUID;
import org.enso.interpreter.runtime.type.ConstantsGen;
import org.enso.interpreter.test.Metadata;
import org.enso.interpreter.test.NodeCountingTestInstrument;
import org.enso.interpreter.test.instrument.RuntimeServerTest.TestContext;
import org.enso.polyglot.runtime.Runtime$Api$CreateContextRequest;
import org.enso.polyglot.runtime.Runtime$Api$CreateContextResponse;
import org.enso.polyglot.runtime.Runtime$Api$EditFileNotification;
import org.enso.polyglot.runtime.Runtime$Api$InitializedNotification;
import org.enso.polyglot.runtime.Runtime$Api$MethodPointer;
import org.enso.polyglot.runtime.Runtime$Api$OpenFileNotification;
import org.enso.polyglot.runtime.Runtime$Api$PushContextRequest;
import org.enso.polyglot.runtime.Runtime$Api$PushContextResponse;
import org.enso.polyglot.runtime.Runtime$Api$Request;
import org.enso.polyglot.runtime.Runtime$Api$Response;
import org.enso.polyglot.runtime.Runtime$Api$StackItem$ExplicitCall;
import org.enso.polyglot.runtime.Runtime$Api$StackItem$LocalCall;
import org.enso.text.editing.model;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;
import scala.Option;
import scala.collection.immutable.List;
import scala.collection.immutable.Seq;
import scala.collection.immutable.Vector1;

public class IncrementalUpdatesTest {
  private TestContext context;
  private NodeCountingTestInstrument nodeCountingInstrument;

  @Before
  public void initializeContext() {
    RuntimeServerTest t = new RuntimeServerTest();
    context = t.new TestContext("Test");
    var initResponse = context.receive().get();
    assertEquals(Response(new Runtime$Api$InitializedNotification()), initResponse);
    var engine = context.executionContext().context().getEngine();
    nodeCountingInstrument = engine.getInstruments().get(NodeCountingTestInstrument.INSTRUMENT_ID).lookup(NodeCountingTestInstrument.class);
  }

  @Test
  public void sendUpdatesWhenFunctionBodyIsChanged() {
    var contextId = UUID.randomUUID();
    var requestId = UUID.randomUUID();
    var moduleName = "Enso_Test.Test.Main";
    var metadata = new Metadata();

    // foo definition
    metadata.addItem(25, 22);
    // foo name
    metadata.addItem(25, 3);
    var fooX = metadata.addItem(39, 1);
    var fooRes = metadata.addItem(45, 1);
    var mainFoo = metadata.addItem(63, 8);
    var mainRes = metadata.addItem(76, 12);

    var code = """
      import Standard.Base.IO

      foo =
          x = 4
          x

      main =
          y = here.foo
          IO.println y
        """;
    var contents = metadata.appendToCode(code);
    var mainFile = context.writeMain(contents);

    nodeCountingInstrument.enable();

    // create context
    var request = Request(requestId, new Runtime$Api$CreateContextRequest(contextId));
    context.send(request);
    var response = context.receive().get();
    assertEquals(response,
            Response(requestId, new Runtime$Api$CreateContextResponse(contextId))
    );
    // Open the new file
    context.send(
            Request(new Runtime$Api$OpenFileNotification(mainFile, contents))
    );
    assertTrue("No reply", context.receiveNone().isEmpty());

    nodeCountingInstrument.assertNewNodes("No execution, no nodes yet", 0, 0);

    context.send(
            Request(
                    requestId,
                    new Runtime$Api$PushContextRequest(
                            contextId,
                            new Runtime$Api$StackItem$ExplicitCall(
                                    new Runtime$Api$MethodPointer(moduleName, "Enso_Test.Test.Main", "main"),
                                    None(),
                                    new Vector1<>(new String[] { "0" })
                            )
                    )
            )
    );

    assertSameElements(context.receiveNIgnoreStdLib(4),
            Response(requestId, new Runtime$Api$PushContextResponse(contextId)),
            TestMessages.update(contextId, mainFoo, ConstantsGen.INTEGER, new Runtime$Api$MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main", "foo")),
            TestMessages.update(contextId, mainRes, ConstantsGen.NOTHING),
            context.executionComplete(contextId)
    );
    assertEquals(List.newBuilder().addOne("4"), context.consumeOut());

    nodeCountingInstrument.assertNewNodes("Execution creates some nodes", 20, 35);

    // push foo call
    context.send(
      Request(
        requestId,
        new Runtime$Api$PushContextRequest(contextId, new Runtime$Api$StackItem$LocalCall(mainFoo))
      )
    );
    assertSameElements(context.receiveN(4, 10),
      Response(requestId, new Runtime$Api$PushContextResponse(contextId)),
      TestMessages.update(contextId, fooX, ConstantsGen.INTEGER),
      TestMessages.update(contextId, fooRes, ConstantsGen.INTEGER),
      context.executionComplete(contextId)
    );
    assertEquals(List.newBuilder().addOne("4"), context.consumeOut());

    nodeCountingInstrument.assertNewNodes("There shall be more nodes after execution", 25, Integer.MAX_VALUE);
    context.send(
      Request(
        new Runtime$Api$EditFileNotification(
          mainFile,
          makeSeq(
              new model.TextEdit(
              new model.Range(new model.Position(3, 8), new model.Position(3, 9)),
              "5"
            )
          )
        )
      )
    );
    assertSameElements(context.receiveN(1, 10000),
      context.executionComplete(contextId)
    );
    assertEquals(List.newBuilder().addOne("5"), context.consumeOut());
    nodeCountingInstrument.assertNewNodes("No new nodes created", 0, 0);
  }

  private static void assertSameElements(List<Runtime$Api$Response> actual, Runtime$Api$Response... seq) {
    assertEquals("Same size: " + actual, seq.length, actual.size());
    for (int i = 0; i < seq.length; i++) {
      var real = actual.drop(i).head();
      assertEquals("Check on #" + i, real, seq[i]);
    }
  }

  @SafeVarargs
  private static <T> Seq<T> makeSeq(T... items) {
    var b = Seq.<T>newBuilder();
    for (var o : items) {
     b.addOne(o);
    }
    return b.result();
  }

  @SuppressWarnings("unchecked")
  private static <T> Option<T> None() {
    return (Option<T>) scala.None$.MODULE$;
  }

  private static Runtime$Api$Request Request(UUID id, org.enso.polyglot.runtime.Runtime.ApiRequest request) {
    return org.enso.polyglot.runtime.Runtime$Api$Request$.MODULE$.apply(id, request);
  }
  private static Runtime$Api$Request Request(org.enso.polyglot.runtime.Runtime.ApiRequest request) {
    return org.enso.polyglot.runtime.Runtime$Api$Request$.MODULE$.apply(request);
  }
  private static Runtime$Api$Response Response(org.enso.polyglot.runtime.Runtime.ApiResponse request) {
    return org.enso.polyglot.runtime.Runtime$Api$Response$.MODULE$.apply(request);
  }
  private static Runtime$Api$Response Response(UUID id, org.enso.polyglot.runtime.Runtime.ApiResponse request) {
    return org.enso.polyglot.runtime.Runtime$Api$Response$.MODULE$.apply(id, request);
  }
}
