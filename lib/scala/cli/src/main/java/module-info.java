module org.enso.cli {
  requires scala.library;
  requires org.enso.scala.yaml;
  requires org.enso.scala.wrapper;

  exports org.enso.cli;
  exports org.enso.cli.arguments;
  exports org.enso.cli.task;
}
