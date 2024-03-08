package org.enso.table.data.table;

import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.apache.xmlbeans.XmlException;
import org.graalvm.polyglot.Context;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class TableToXml {

  public static Document to_xml(
      int rowCount,
      Column[] element_columns,
      Column[] attribute_columns,
      Column value_Column,
      String root_name,
      String row_name)
      throws XmlException, ParserConfigurationException {
    var docFactory = DocumentBuilderFactory.newInstance();
    var docBuilder = docFactory.newDocumentBuilder();
    var doc = docBuilder.newDocument();
    var rootElement = doc.createElement(makeXmlTagNameLegal(root_name));
    doc.appendChild(rootElement);

    Map<String, String> legal_column_names =
        Stream.concat(Stream.of(element_columns), Stream.of(attribute_columns))
            .collect(
                Collectors.toMap(
                    e -> e.getName(), e -> makeXmlTagNameLegal(e.getName()), (e1, e2) -> e1));

    var legal_row_name = makeXmlTagNameLegal(row_name);
    var context = Context.getCurrent();
    for (int row = 0; row < rowCount; row++) {
      var rowElement = doc.createElement(legal_row_name);
      if (value_Column != null) {
        get_set_value(value_Column, row, rowElement);
      }
      for (var element_column : element_columns) {
        var legal_name = legal_column_names.get(element_column.getName());
        get_append_element(element_column, legal_name, row, doc, rowElement);
        context.safepoint();
      }
      for (var attribute_column : attribute_columns) {
        var legal_name = legal_column_names.get(attribute_column.getName());
        get_set_attribute(attribute_column, legal_name, row, rowElement);
        context.safepoint();
      }
      rootElement.appendChild(rowElement);
      context.safepoint();
    }

    return doc;
  }

  private static void get_set_attribute(
      Column attribute_column, String legal_name, int row, Element rowElement) throws DOMException {
    var item = attribute_column.getStorage().getItemBoxed(row);
    if (item != null) {
      rowElement.setAttribute(legal_name, item.toString());
    }
  }

  private static void get_append_element(
      Column element_column, String legal_name, int row, Document doc, Element rowElement)
      throws DOMException {
    var item = element_column.getStorage().getItemBoxed(row);
    if (item != null) {
      var columnElement = doc.createElement(legal_name);
      columnElement.setTextContent(item.toString());
      rowElement.appendChild(columnElement);
    }
  }

  private static void get_set_value(Column value_Column, int row, Element rowElement)
      throws DOMException {
    var item = value_Column.getStorage().getItemBoxed(row);
    if (item != null) {
      rowElement.setTextContent(item.toString());
    }
  }

  public static String makeXmlTagNameLegal(String input) {
    String nameStartChar =
        "[^:A-Z_a-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\ud800\udc00-\udbff\udfff]";
    String nameChar =
        "[^-.0-9:A-Z_a-z\u00B7\u0300-\u036F\u203F-\u2040\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\ud800\udc00-\udbff\udfff]";
    // XML tag names cannot start with a number or punctuation character, and cannot contain spaces
    String cleaned = input.replaceAll(nameChar, "_");
    // If the cleaned name is empty or doesn't start with a valid character, prefix it with an
    // underscore
    if (cleaned.isEmpty() || cleaned.substring(0, 1).matches(nameStartChar)) {
      cleaned = "_" + cleaned;
    }
    return cleaned;
  }
}
