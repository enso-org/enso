package org.enso.interpreter.test.instrument;

import com.oracle.truffle.api.nodes.Node;
import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;
import org.enso.compiler.core.IR$Literal$Number;
import org.enso.interpreter.node.expression.literal.LiteralNode;
import org.enso.interpreter.runtime.type.ConstantsGen;
import org.enso.interpreter.test.Metadata;
import org.enso.interpreter.test.NodeCountingTestInstrument;
import org.enso.interpreter.test.instrument.RuntimeServerTest.TestContext;
import org.enso.polyglot.runtime.Runtime$Api$CreateContextRequest;
import org.enso.polyglot.runtime.Runtime$Api$CreateContextResponse;
import org.enso.polyglot.runtime.Runtime$Api$EditFileNotification;
import org.enso.polyglot.runtime.Runtime$Api$ExecutionFailed;
import org.enso.polyglot.runtime.Runtime$Api$ExpressionUpdates;
import org.enso.polyglot.runtime.Runtime$Api$InitializedNotification;
import org.enso.polyglot.runtime.Runtime$Api$MethodPointer;
import org.enso.polyglot.runtime.Runtime$Api$PushContextRequest;
import org.enso.polyglot.runtime.Runtime$Api$PushContextResponse;
import org.enso.polyglot.runtime.Runtime$Api$Request;
import org.enso.polyglot.runtime.Runtime$Api$Response;
import org.enso.polyglot.runtime.Runtime$Api$SetExpressionValueNotification;
import org.enso.polyglot.runtime.Runtime$Api$OpenFileNotification;
import org.enso.polyglot.runtime.Runtime$Api$StackItem$ExplicitCall;
import org.enso.polyglot.runtime.Runtime$Api$StackItem$LocalCall;
import org.enso.text.editing.model;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;
import scala.Option;
import scala.collection.immutable.List;
import scala.collection.immutable.Seq;
import scala.collection.immutable.Set;
import scala.collection.immutable.Set$;
import scala.collection.immutable.Vector1;

public class IncrementalUpdatesTest {

  private static final String MODULE_NAME = "Enso_Test.Test.Main";
  private TestContext context;
  private NodeCountingTestInstrument nodeCountingInstrument;
  private File mainFile;

  @Before
  public void initializeContext() {
    RuntimeServerTest t = new RuntimeServerTest();
    context = t.new TestContext("Test");
    var initResponse = context.receive().get();
    assertEquals(Response(new Runtime$Api$InitializedNotification()), initResponse);
    var engine = context.executionContext().context().getEngine();
    var instr = engine.getInstruments().get(NodeCountingTestInstrument.INSTRUMENT_ID);
    assertNotNull("NodeCountingTestInstrument found", instr);
    nodeCountingInstrument = instr.lookup(NodeCountingTestInstrument.class);
  }

  @Test
  public void sendUpdatesWhenFunctionBodyIsChanged() {
    sendUpdatesWhenFunctionBodyIsChangedBySettingValue("4", ConstantsGen.INTEGER, "4", "5", "5", LiteralNode.class);
    var m = context.languageContext().findModule(MODULE_NAME).orElse(null);
    assertNotNull("Module found", m);
    var numbers = m.getIr().preorder().filter((v1) -> {
      return v1 instanceof IR$Literal$Number;
    });
    assertEquals("One number found: " + numbers, 1, numbers.size());
    if (numbers.head() instanceof IR$Literal$Number n) {
      assertEquals("updated to 5", "5", n.value());
    }
  }

  @Test
  public void sendUpdatesWhenWhenLineIsChangedBySettingValue() {
    sendUpdatesWhenFunctionBodyIsChangedBySettingValue("4", ConstantsGen.INTEGER, "4", "1000", "1000", LiteralNode.class);
  }

  @Test
  public void sendUpdatesWhenWhenLineIsChangedByTextEdit() {
    sendUpdatesWhenFunctionBodyIsChangedByTextEdit("4", ConstantsGen.INTEGER, "4", "1000", "1000", LiteralNode.class);
  }

  @Test
  public void sendMultipleUpdates() {
    sendUpdatesWhenFunctionBodyIsChangedBySettingValue("4", ConstantsGen.INTEGER, "4", "1000", "1000", LiteralNode.class);
    sendExpressionValue("1000", "333");
    assertEquals(List.newBuilder().addOne("333"), context.consumeOut());
    nodeCountingInstrument.assertNewNodes("No execution on 333, no nodes yet", 0, 0);
    sendExpressionValue("333", "22");
    assertEquals(List.newBuilder().addOne("22"), context.consumeOut());
    nodeCountingInstrument.assertNewNodes("No execution on 22, no nodes yet", 0, 0);
  }

  @Test
  public void sendUpdatesWhenTextIsChangedByTextEdit() {
    sendUpdatesWhenFunctionBodyIsChangedByTextEdit("\"hi\"", ConstantsGen.TEXT, "hi", "\"text\"", "text", LiteralNode.class);
  }

  @Test
  public void sendUpdatesWhenTextIsChangedBySettingValue() {
    sendUpdatesWhenFunctionBodyIsChangedBySettingValue("\"hi\"", ConstantsGen.TEXT, "hi", "\"text\"", "text", LiteralNode.class);
  }

  @Test
  public void sendNotANumberChange() {
    var failed = sendUpdatesWhenFunctionBodyIsChangedBySettingValue("4", ConstantsGen.INTEGER, "4", "x", null, LiteralNode.class);
    assertTrue("Execution failed: " + failed, failed.head().payload() instanceof Runtime$Api$ExecutionFailed);
  }

  private static String extractPositions(String code, String chars, Map<Character, int[]> beginAndLength) {
    for (int at = 0; at < code.length();) {
      char ch = code.charAt(at);
      if (chars.indexOf(ch) >= 0) {
        int[] prev = beginAndLength.get(ch);
        if (prev == null) {
          beginAndLength.put(ch, new int[]{at, -1});
        } else if (prev[1] == -1) {
          prev[1] = at - prev[0];
        } else {
          throw new IllegalStateException();
        }
        var before = code.substring(0, at);
        var after = code.substring(at + 1);
        code = before + after;
      } else {
        at++;
      }
    }
    return code;
  }

  private List<Runtime$Api$Response> sendUpdatesWhenFunctionBodyIsChangedByTextEdit(
      String originalText, String exprType, String originalOutput,
      String newText, String executionOutput, Class<? extends Node> truffleNodeType
  ) {
    return sendUpdatesWhenFunctionBodyIsChanged(originalText, exprType, originalOutput, newText, executionOutput, truffleNodeType, this::sendEditFile);
  }

  private List<Runtime$Api$Response> sendUpdatesWhenFunctionBodyIsChangedBySettingValue(
      String originalText, String exprType, String originalOutput,
      String newText, String executionOutput, Class<? extends Node> truffleNodeType
  ) {
    return sendUpdatesWhenFunctionBodyIsChanged(originalText, exprType, originalOutput, newText, executionOutput, truffleNodeType, this::sendExpressionValue);
  }

  private List<Runtime$Api$Response> sendUpdatesWhenFunctionBodyIsChanged(
          String originalText, String exprType, String originalOutput,
          String newText, String executionOutput, Class<? extends Node> truffleNodeType,
          java.util.function.BiFunction<String, String, List<Runtime$Api$Response>> sendEdit
  ) {
    var contextId = UUID.randomUUID();
    var requestId = UUID.randomUUID();
    var metadata = new Metadata();

    var pos = new HashMap<Character, int[]>();
    var code = extractPositions("""
      import Standard.Base.IO

      &$foo$ =
          x = #{originalText}#
          *x*
      &
      main =
          y = @foo@
          %IO.println y%
        """.replace("{originalText}", originalText),
            "&$#*@%", pos);

    Function<Character, UUID> registerRegion = (ch) -> {
      int[] beginAndLength = pos.get(ch);
      return metadata.addItem(beginAndLength[0], beginAndLength[1], null);
    };
    // foo definition
    registerRegion.apply('&');
    // foo name
    registerRegion.apply('$');
    var fooX = registerRegion.apply('#');
    var fooRes = registerRegion.apply('*');
    var mainFoo = registerRegion.apply('@');
    var mainRes = registerRegion.apply('%');

    var contents = metadata.appendToCode(code);
    this.mainFile = context.writeMain(contents);

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
            new Runtime$Api$MethodPointer(MODULE_NAME, "Enso_Test.Test.Main", "main"),
            None(),
            new Vector1<>(new String[]{"0"})
          )
        )
      )
    );

    assertSameElements(context.receiveNIgnorePendingExpressionUpdates(4, 10, emptySet()),
      Response(requestId, new Runtime$Api$PushContextResponse(contextId)),
      TestMessages.update(contextId, mainFoo, exprType, new Runtime$Api$MethodPointer("Enso_Test.Test.Main", "Enso_Test.Test.Main", "foo")),
      TestMessages.update(contextId, mainRes, ConstantsGen.NOTHING),
      context.executionComplete(contextId)
    );
    assertEquals(List.newBuilder().addOne(originalOutput), context.consumeOut());

    nodeCountingInstrument.assertNewNodes("Execution creates some nodes", 20, 35);

    // push foo call
    context.send(
      Request(
        requestId,
        new Runtime$Api$PushContextRequest(contextId, new Runtime$Api$StackItem$LocalCall(mainFoo))
      )
    );
    assertSameElements(context.receiveNIgnorePendingExpressionUpdates(4, 10, emptySet()),
      Response(requestId, new Runtime$Api$PushContextResponse(contextId)),
      TestMessages.update(contextId, fooX, exprType),
      TestMessages.update(contextId, fooRes, exprType),
      context.executionComplete(contextId)
    );
    assertEquals(List.newBuilder().addOne(originalOutput), context.consumeOut());

    var allNodesAfterException = nodeCountingInstrument.assertNewNodes("There shall be more nodes after execution", 23, Integer.MAX_VALUE);
    var literalNode = findLiteralNode(truffleNodeType, allNodesAfterException);
    assertEquals("Check Literal node text in the source", originalText, literalNode.getSourceSection().getCharacters().toString());

    var executionCompleteEvents = sendEdit.apply(originalText, newText);
    if (executionOutput != null) {
      assertSameElements(executionCompleteEvents, context.executionComplete(contextId));
      assertEquals(List.newBuilder().addOne(executionOutput), context.consumeOut());
      nodeCountingInstrument.assertNewNodes("No new nodes created", 0, 0);

      assertEquals("Literal node has been updated in the source", newText, literalNode.getSourceSection().getCharacters().toString());
    }
    return executionCompleteEvents;
  }

  private List<Runtime$Api$Response> sendEditFile(String originalText, String newText) {
    assertNotNull("Main file must be defined before", mainFile);
    context.send(Request(new Runtime$Api$EditFileNotification(
        mainFile,
        makeSeq(
            new model.TextEdit(
                new model.Range(new model.Position(3, 8), new model.Position(3, 8 + originalText.length())),
                newText
            )
        ),
        true
    )));
    return context.receiveNIgnorePendingExpressionUpdates(1, 10, emptySet());
  }

  private List<Runtime$Api$Response> sendExpressionValue(String originalText, String newText) {
    assertNotNull("Main file must be defined before", mainFile);
    context.send(Request(new Runtime$Api$SetExpressionValueNotification(
      mainFile,
      makeSeq(
        new model.TextEdit(
          new model.Range(new model.Position(3, 8), new model.Position(3, 8 + originalText.length())),
          newText
        )
      ),
      UUID.randomUUID(),
      newText
    )));
    return context.receiveNIgnoreExpressionUpdates(1, 10);
  }

  private <T extends Node> T findLiteralNode(Class<T> type, Map<Class, java.util.List<Node>> nodes) {
    var intNodes = nodes.get(type);
    assertNotNull("Found LiteralNode in " + nodes, intNodes);
    assertEquals("Expecting one node: " + intNodes, 1, intNodes.size());
    return type.cast(intNodes.get(0));
  }

  private static void assertSameElements(List<Runtime$Api$Response> actual, Runtime$Api$Response... seq) {
    assertEquals("Same size: " + actual, seq.length, actual.size());
    for (int i = 0; i < seq.length; i++) {
      var real = actual.drop(i).head();
      if (real instanceof Runtime$Api$Response response) {
        if (response.payload() instanceof Runtime$Api$ExpressionUpdates) {
          continue;
        }
      }
      assertEquals("Check on #" + i, seq[i], real);
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

  private static <T> Set<T> emptySet() {
    return Set$.MODULE$.empty();
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
