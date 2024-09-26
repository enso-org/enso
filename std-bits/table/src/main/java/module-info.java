import org.enso.base.file_format.FileFormatSPI;
import org.enso.table.read.DelimitedFormatSPI;
import org.enso.table.read.ExcelFormatSPI;

module org.enso.std.table {
  requires java.xml;
  requires jdk.xml.dom;
  requires com.ibm.icu;
  requires org.antlr.antlr4.runtime;
  requires org.apache.poi.ooxml;
  requires org.apache.poi.ooxml.schemas;
  requires org.apache.poi.poi;
  requires org.apache.xmlbeans;
  requires org.graalvm.collections;
  requires org.graalvm.polyglot;
  requires org.enso.common.polyglot.core.utils;
  requires org.enso.std.base;
  requires univocity.parsers;

  provides FileFormatSPI with
      ExcelFormatSPI,
      DelimitedFormatSPI;
}
