package org.enso.table.data.table;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.apache.xmlbeans.XmlException;
import org.apache.xmlbeans.XmlObject;
import org.apache.xmlbeans.XmlOptions;
import org.graalvm.polyglot.Context;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class TableToXml {

  public static String to_xml(
      int rowCount,
      Column[] element_columns,
      Column[] attribute_columns,
      Column value_Column,
      String root_name,
      String row_name)
      throws XmlException, ParserConfigurationException {
    DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
    DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
    Document doc = docBuilder.newDocument();
    Element rootElement =
        doc.createElement(root_name.isEmpty() ? "root" : makeXmlTagNameLegal(root_name));
    doc.appendChild(rootElement);

    Context context = Context.getCurrent();
    for (int row = 0; row < rowCount; row++) {
      Element rowElement =
          doc.createElement(row_name.isEmpty() ? "row" : makeXmlTagNameLegal(row_name));
      if (value_Column != null) {
        var item = value_Column.getStorage().getItemBoxed(row);
        if (item != null) {
          rowElement.setTextContent(item.toString());
        }
      }
      for (Column element_column : element_columns) {
        var item = element_column.getStorage().getItemBoxed(row);
        if (item != null) {
          Element columnElement = doc.createElement(makeXmlTagNameLegal(element_column.getName()));
          columnElement.setTextContent(item.toString());
          rowElement.appendChild(columnElement);
        }
        context.safepoint();
      }
      for (Column attribute_column : attribute_columns) {
        var item = attribute_column.getStorage().getItemBoxed(row);
        if (item != null) {
          rowElement.setAttribute(makeXmlTagNameLegal(attribute_column.getName()), item.toString());
        }
        context.safepoint();
      }
      rootElement.appendChild(rowElement);
      context.safepoint();
    }

    XmlObject xmlObject = XmlObject.Factory.parse(doc);

    XmlOptions options = new XmlOptions();
    options.setSavePrettyPrint();
    options.setUseDefaultNamespace();
    options.setSaveAggressiveNamespaces();

    String xmlString = xmlObject.xmlText(options);

    return xmlString;
  }

  public static String makeXmlTagNameLegal(String input) {
    // XML tag names cannot start with a number or punctuation character, and cannot contain spaces
    String cleaned = input.replaceAll("^[^a-zA-Z0-9_]+|[^a-zA-Z0-9-_:.]", "_");
    // If the cleaned name is empty or doesn't start with a valid character, prefix it with an
    // underscore
    if (cleaned.isEmpty() || !cleaned.substring(0, 1).matches("[a-zA-Z_]")) {
      cleaned = "_" + cleaned;
    }
    return cleaned;
  }
}
