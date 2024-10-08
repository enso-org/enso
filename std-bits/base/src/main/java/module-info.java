import org.enso.base.enso_cloud.DataLinkSPI;
import org.enso.base.enso_cloud.EnsoFileDataLinkSPI;
import org.enso.base.enso_cloud.EnsoPathFileSystemSPI;
import org.enso.base.file_format.ByteFormatSPI;
import org.enso.base.file_format.FileFormatSPI;
import org.enso.base.file_format.JSONFormatSPI;
import org.enso.base.file_format.TextFormatSPI;
import org.enso.base.file_format.XMLFormatSPI;
import org.enso.base.file_system.FileSystemSPI;
import org.enso.base.net.http.HTTPFetchDataLinkSPI;

module org.enso.std.base {
  requires java.logging;
  requires java.xml;
  requires java.net.http;
  requires java.sql;
  requires org.graalvm.collections;
  requires com.ibm.icu;
  requires org.graalvm.polyglot;
  requires com.fasterxml.jackson.databind;
  requires org.enso.common.polyglot.core.utils;

  uses FileSystemSPI;
  uses FileFormatSPI;
  uses DataLinkSPI;

  // following packages are accessed by Java code in other Enso modules
  exports org.enso.base;
  exports org.enso.base.file_system;
  exports org.enso.base.file_format;
  exports org.enso.base.enso_cloud;
  exports org.enso.base.enso_cloud.audit;
  exports org.enso.base.lookup;
  exports org.enso.base.numeric;
  exports org.enso.base.polyglot;
  exports org.enso.base.time;
  exports org.enso.base.text;
  exports org.enso.base.arrays;
  exports org.enso.base.statistics;

  // following packages are accessed by Enso via polyglot java import
  opens org.enso.base;
  opens org.enso.base.arrays;

  exports org.enso.base.encoding;

  opens org.enso.base.numeric;
  opens org.enso.base.net;
  opens org.enso.base.net.http;
  opens org.enso.base.polyglot;
  opens org.enso.base.random;
  opens org.enso.base.statistics;
  opens org.enso.base.text;

  provides FileSystemSPI with
      EnsoPathFileSystemSPI;
  provides FileFormatSPI with
      ByteFormatSPI,
      JSONFormatSPI,
      TextFormatSPI,
      XMLFormatSPI;
  provides DataLinkSPI with
      EnsoFileDataLinkSPI,
      HTTPFetchDataLinkSPI;
}
