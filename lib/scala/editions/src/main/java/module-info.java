module org.enso.editions {
  requires scala.library;

  requires org.enso.semver;
  requires org.enso.scala.yaml;
  requires org.enso.scala.wrapper;
  requires org.enso.version.output;

  requires org.yaml.snakeyaml;

  exports org.enso.editions;
  exports org.enso.editions.provider;
  exports org.enso.editions.repository;
  exports org.enso.yaml;
}
