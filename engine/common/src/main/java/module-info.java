import org.enso.common.ContextLoggingConfigurator;

module org.enso.engine.common {
  requires org.graalvm.nativeimage;
  requires org.graalvm.polyglot;

  exports org.enso.common;

  uses ContextLoggingConfigurator;
}
