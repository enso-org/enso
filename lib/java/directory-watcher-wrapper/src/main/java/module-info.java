module org.enso.directory.watcher.wrapper {
  requires scala.library;
  requires com.sun.jna;
  requires jdk.unsupported;
  requires org.slf4j;

  exports io.methvin.watcher;
  exports io.methvin.watchservice;
  opens io.methvin.watchservice.jna;
}
