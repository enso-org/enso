module org.enso.runtime.compiler.dump.igv {
  requires jdk.internal.vm.compiler;
  requires org.enso.runtime.parser;
  requires org.enso.runtime.compiler.dump;

  provides org.enso.compiler.dump.service.IRDumpService with
      org.enso.compiler.dump.igv.IGVDumper;
}
