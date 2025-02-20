module org.enso.directory.watcher.wrapper {
  requires scala.library;
  requires org.enso.jna.wrapper;
  requires jdk.unsupported;
  requires org.slf4j;

  exports io.methvin.watcher;
  exports io.methvin.watchservice;

  opens io.methvin.watchservice.jna;
}
