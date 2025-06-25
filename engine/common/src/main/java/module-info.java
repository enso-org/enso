import org.enso.common.ContextLoggingConfigurator;
import org.enso.common.PolyglotSymbolResolver;

module org.enso.engine.common {
  requires org.graalvm.nativeimage;
  requires org.graalvm.polyglot;

  exports org.enso.common;

  uses PolyglotSymbolResolver;
  uses ContextLoggingConfigurator;
}
