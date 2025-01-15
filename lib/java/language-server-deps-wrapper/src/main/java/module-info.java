module org.enso.language.server.deps.wrapper {
  requires scala.library;
  // For shapeless
  requires org.enso.scala.wrapper;

  // pureconfig-core_2.13-0.17.4.jar
  exports pureconfig;
  exports pureconfig.generic;
}
