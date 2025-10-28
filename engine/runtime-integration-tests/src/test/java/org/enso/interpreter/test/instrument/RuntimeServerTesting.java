package org.enso.interpreter.test.instrument;

import static org.junit.Assert.assertEquals;

import java.util.List;
import java.util.UUID;
import org.enso.interpreter.test.Metadata;
import org.enso.interpreter.test.instrument.RuntimeServerTest.TestContext;
import org.enso.polyglot.runtime.Runtime$Api$CreateContextRequest;
import org.enso.polyglot.runtime.Runtime$Api$CreateContextResponse;
import org.enso.polyglot.runtime.Runtime$Api$MethodPointer;
import org.enso.polyglot.runtime.Runtime$Api$OpenFileRequest;
import org.enso.polyglot.runtime.Runtime$Api$OpenFileResponse$;
import org.enso.polyglot.runtime.Runtime$Api$PushContextRequest;
import org.enso.polyglot.runtime.Runtime$Api$Request;
import org.enso.polyglot.runtime.Runtime$Api$Response;
import org.enso.polyglot.runtime.Runtime$Api$StackItem$ExplicitCall;
import org.enso.scala.wrapper.ScalaConversions;
import scala.Option;

/**
 * Invoked from {@link RuntimeServerTest}. Reuses the same infrastructure while it allows writing
 * new Java instead of in Scala.
 */
final class RuntimeServerTesting {
  static void accessRuntimeCache(TestContext context) {

    var contextId = UUID.randomUUID();
    var requestId = UUID.randomUUID();
    var reqOptId = Option.apply(requestId);
    var moduleName = "Enso_Test.Test.Main";

    var metadata = new Metadata("");
    var id_x_0 = metadata.addItem(166, 1, "aa");
    var id_x_1 = metadata.addItem(178, 1, "ab");

    var code =
        """
        from Standard.Base.Runtime import value_for_uuid
        from Standard.Base.Data.Numbers import all
        import Standard.Base.IO

        private v n = value_for_uuid n

        main =
            x_0 = 6
            x_1 = 7
            x_2 = (v "${aa}")*(v "${bb}")
            IO.println x_2
            IO.println x_0*x_1
            IO.println x_2==x_0*x_1
        """
            .replace("${aa}", id_x_0.toString())
            .replace("${bb}", id_x_1.toString());
    var contents = metadata.appendToCode(code);
    var mainFile = context.writeMain(contents);

    metadata.assertInCode(id_x_0, code, "6");
    metadata.assertInCode(id_x_1, code, "7");

    // create context
    context.send(
        new Runtime$Api$Request(reqOptId, new Runtime$Api$CreateContextRequest(contextId)));
    assertEquals(
        context.receive(),
        Option.apply(
            new Runtime$Api$Response(reqOptId, new Runtime$Api$CreateContextResponse(contextId))));

    // open file
    context.send(
        new Runtime$Api$Request(reqOptId, new Runtime$Api$OpenFileRequest(mainFile, contents)));
    assertEquals(
        context.receive(),
        Option.apply(new Runtime$Api$Response(reqOptId, Runtime$Api$OpenFileResponse$.MODULE$)));

    // push main
    context.send(
        new Runtime$Api$Request(
            reqOptId,
            new Runtime$Api$PushContextRequest(
                contextId,
                new Runtime$Api$StackItem$ExplicitCall(
                    new Runtime$Api$MethodPointer(moduleName, moduleName, "main"),
                    Option.empty(),
                    ScalaConversions.<String>nil().toVector()),
                true)));
    var reply = context.receiveNIgnoreStdLib(4, 60);
    assertEquals("Five messages", 4, reply.size());
    assertEquals(
        "Output should be correct for " + reply + " messages",
        List.of("42", "42", "True"),
        ScalaConversions.asJava(context.consumeOut()));
  }
}
