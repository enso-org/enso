module org.enso.runtime.compiler.dump.igv {
  requires jdk.internal.vm.compiler;
  requires org.enso.runtime.parser;
  requires org.enso.runtime.compiler.dump;
  requires org.slf4j;
  requires scala.library;

  provides org.enso.compiler.dump.service.IRDumpFactoryService with
      org.enso.compiler.dump.igv.IGVDumperFactory;
}
