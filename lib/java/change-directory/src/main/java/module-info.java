/**
 * Calls native code that is responsible for changing the working directory. Only works in Native
 * Image.
 */
module org.enso.change.directory {
  requires org.enso.engine.common;
  requires org.graalvm.nativeimage;
  requires org.slf4j;

  exports org.enso.change.directory;
}
