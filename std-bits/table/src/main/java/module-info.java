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

  //
  // opening up for reflection from Enso code
  //
  opens org.enso.table.aggregations;
  opens org.enso.table.expressions;
  opens org.enso.table.formatting;
  opens org.enso.table.excel;
  opens org.enso.table.problems;
  opens org.enso.table.parsing;
  opens org.enso.table.parsing.problems;

  exports org.enso.table.data.column.storage;

  opens org.enso.table.data.column.storage.numeric;
  opens org.enso.table.data.column.builder;
  opens org.enso.table.data.column.operation;
  opens org.enso.table.data.column.operation.cast;

  exports org.enso.table.data.column.operation.map;

  opens org.enso.table.data.column.operation.unary;

  exports org.enso.table.data.column.storage.type;
  exports org.enso.table.data.mask;
  exports org.enso.table.data.index;
  exports org.enso.table.data.table;

  opens org.enso.table.data.table.problems;
  opens org.enso.table.data.table.join;

  exports org.enso.table.data.table.join.between;

  opens org.enso.table.data.table.join.conditions;
  opens org.enso.table.data.table.join.lookup;
  opens org.enso.table.error;
  opens org.enso.table.read;
  opens org.enso.table.operations;
  opens org.enso.table.util;
  opens org.enso.table.util.problems;
  opens org.enso.table.write;
}
