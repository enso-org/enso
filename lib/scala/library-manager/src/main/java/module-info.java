module org.enso.librarymanager {
  requires scala.library;

  requires org.enso.distribution;
  requires org.enso.cli;
  requires org.enso.editions;

  exports org.enso.librarymanager;
  exports org.enso.librarymanager.dependencies;
  exports org.enso.librarymanager.published;
  exports org.enso.libraryupload;
  exports org.enso.libraryupload.auth;
}
