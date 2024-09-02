module org.enso.jpms.wrapper.methvin.directory.watcher {
  requires scala.library;
  requires com.sun.jna;
  requires org.slf4j;

  exports io.methvin.watcher;
  exports io.methvin.watchservice;
}
