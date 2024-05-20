module org.enso.profiling {
  requires org.netbeans.modules.sampler.RELEASE180;
  requires jdk.management;
  requires org.slf4j;

  exports org.enso.profiling.sampler;
  exports org.enso.profiling.events;
}
