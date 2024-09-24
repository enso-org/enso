module org.enso.task.progress.notifications {
  requires scala.library;
  requires org.enso.scala.wrapper;
  requires org.enso.akka.wrapper;
  requires org.enso.cli;
  requires org.enso.json.rpc.server;

  exports org.enso.cli.task.notifications;
}
