module org.enso.pkg {
  requires scala.library;
  requires org.apache.commons.compress;
  requires org.enso.editions;
  requires org.enso.semver;
  requires org.enso.scala.yaml;
  // For io.circe
  requires org.enso.scala.wrapper;
  requires org.yaml.snakeyaml;

  exports org.enso.pkg;
  exports org.enso.filesystem;
}
