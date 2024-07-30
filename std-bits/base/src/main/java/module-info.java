import org.enso.base.enso_cloud.EnsoPathFileSystemSPI;
import org.enso.base.file_system.FileSystemSPI;

module org.enso.std.base {
  requires java.logging;
  requires java.xml;
  requires java.net.http;
  requires java.sql;
  requires org.graalvm.collections;
  requires com.ibm.icu;
  requires org.graalvm.polyglot;
  requires com.fasterxml.jackson.databind;
  requires org.openide.util.lookup.RELEASE180;
  requires org.enso.polyglot.common_utils;

  uses FileSystemSPI;

  exports org.enso.base.file_system;
  exports org.enso.base.file_format;
  exports org.enso.base.enso_cloud;
  exports org.enso.base.enso_cloud.audit;

  provides FileSystemSPI with
      EnsoPathFileSystemSPI;
}
