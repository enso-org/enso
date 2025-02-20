module org.enso.downloader {
  requires java.net.http;
  requires scala.library;

  requires org.apache.commons.compress;
  requires org.apache.commons.io;

  requires org.enso.cli;
  // For com.typesafe.scalalogging.Logger
  requires org.enso.scala.wrapper;

  requires org.slf4j;

  exports org.enso.downloader.http;
  exports org.enso.downloader.archive;

}
