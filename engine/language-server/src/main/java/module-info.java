module org.enso.language.server {
  requires java.logging;
  requires scala.library;

  requires commons.cli;
  requires flatbuffers.java;
  requires org.apache.commons.io;
  requires org.graalvm.polyglot;
  requires org.eclipse.jgit;
  requires org.openide.util.lookup.RELEASE180;
  requires org.slf4j;

  requires org.enso.akka.wrapper;
  requires org.enso.zio.wrapper;
  requires org.enso.runner.common;
  requires org.enso.engine.common;
  requires org.enso.json.rpc.server;
  requires org.enso.logging.utils;
  requires org.enso.logging.utils.akka;
  requires org.enso.librarymanager;
  requires org.enso.polyglot.api;
  requires org.enso.profiling;
  requires org.enso.scala.wrapper;
  requires org.enso.language.server.deps.wrapper;
  requires org.enso.searcher;
  requires org.enso.version.output;
  requires org.enso.text.buffer;
  requires org.enso.ydoc;
}
