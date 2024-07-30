module org.enso.std.database {
  requires java.logging;
  requires java.sql;
  requires org.enso.std.base;
  requires org.enso.polyglot.common_utils;
  requires org.graalvm.collections;
  requires org.graalvm.polyglot;
  requires com.fasterxml.jackson.databind;
  requires org.openide.util.lookup.RELEASE180;

  exports org.enso.database;
}
