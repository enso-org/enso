open module org.enso.runtime {
  requires java.base;
  requires java.net.http;
  // Because of akka.util.Unsafe
  requires jdk.unsupported;
  requires org.enso.syntax;
  // org.enso.profiling and org.enso.doc are not needed for runtime. It is needed for runner.jar,
  // but we have to include it here so that the forwarding in IsolatedClassLoader
  // works.
  requires org.enso.profiling;
  requires org.enso.ydoc;
  requires org.graalvm.polyglot;
  requires org.graalvm.truffle;
  requires static org.slf4j;
  // ydoc-server
  requires io.helidon.webclient;
  requires io.helidon.webclient.websocket;
  requires io.helidon.webserver;
  requires io.helidon.webserver.websocket;

  uses org.slf4j.spi.SLF4JServiceProvider;
  uses org.enso.interpreter.instrument.HandlerFactory;

  provides com.oracle.truffle.api.provider.TruffleLanguageProvider with
      org.enso.interpreter.EnsoLanguageProvider,
      org.enso.interpreter.epb.EpbLanguageProvider;

  provides com.oracle.truffle.api.instrumentation.provider.TruffleInstrumentProvider with
    org.enso.interpreter.instrument.repl.debugger.ReplDebuggerInstrumentProvider,
    org.enso.interpreter.instrument.runtime.server.RuntimeServerInstrumentProvider,
    org.enso.interpreter.instrument.id.execution.IdExecutionInstrumentProvider;


  // java.beans.Transient needed by Jackson jackson.databind.ext.Java7SupportImpl
  requires java.desktop;
  // also needed by Jackson to avoid
  // com.fasterxml.jackson.databind.exc.InvalidTypeIdException:
  //   Could not resolve type id 'org.enso.polyglot.data.Tree$Node' as a subtype of
  //   `org.enso.polyglot.data.Tree$Node<org.enso.polyglot.runtime.Runtime$Api$SuggestionUpdate>`: Not a subtype
  requires java.se;
}
