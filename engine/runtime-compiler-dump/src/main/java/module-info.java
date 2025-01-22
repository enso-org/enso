module org.enso.runtime.compiler.dump {
  requires org.enso.runtime.parser;

  exports org.enso.compiler.dump.service;

  uses org.enso.compiler.dump.service.IRDumpFactoryService;
}
