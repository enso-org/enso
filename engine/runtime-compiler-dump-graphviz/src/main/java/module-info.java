module org.enso.runtime.compiler.dump.graphviz {
  requires scala.library;
  requires org.enso.runtime.parser;
  requires org.enso.runtime.compiler;
  requires org.enso.runtime.compiler.dump;
  requires org.enso.pkg;
  requires org.slf4j;

  provides org.enso.compiler.dump.service.IRDumpService with
      org.enso.compiler.dump.graphviz.GraphVizDumper;
}
