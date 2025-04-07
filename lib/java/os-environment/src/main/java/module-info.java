/**
 * Calls native code that is responsible for changing the working directory. Only works in Native
 * Image.
 */
module org.enso.os.environment {
  requires org.enso.engine.common;
  requires org.graalvm.nativeimage;
  requires org.slf4j;
  requires org.apache.commons.io;

  exports org.enso.os.environment;
  exports org.enso.os.environment.chdir;
  exports org.enso.os.environment.trash;
  exports org.enso.os.environment.directories;
}
