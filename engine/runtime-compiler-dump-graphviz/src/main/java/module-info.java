import org.enso.compiler.dump.service.IRDumper;

module org.enso.runtime.compiler.dump.graphviz {
  requires scala.library;
  requires org.enso.runtime.parser;
  requires org.enso.runtime.compiler;
  requires org.enso.runtime.compiler.dump;
  requires org.enso.pkg;
  requires org.slf4j;

  provides IRDumper with
      org.enso.compiler.dump.graphviz.GraphVizDumper;
}
