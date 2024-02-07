package org.enso.table.data.table;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.xmlbeans.XmlException;
import org.apache.xmlbeans.XmlObject;
import org.apache.xmlbeans.XmlOptions;
import org.graalvm.polyglot.Context;
import org.w3c.dom.DOMException;
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
        var docFactory = DocumentBuilderFactory.newInstance();
        var docBuilder = docFactory.newDocumentBuilder();
        var doc = docBuilder.newDocument();
        var rootElement
                = doc.createElement(root_name.isEmpty() ? "root" : makeXmlTagNameLegal(root_name));
        doc.appendChild(rootElement);

        var context = Context.getCurrent();
        for (int row = 0; row < rowCount; row++) {
            var rowElement
                    = doc.createElement(row_name.isEmpty() ? "row" : makeXmlTagNameLegal(row_name));
            if (value_Column != null) {
                get_set_value(value_Column, row, rowElement);
            }
            for (var element_column : element_columns) {
                get_append_element(element_column, row, doc, rowElement);
                context.safepoint();
            }
            for (var attribute_column : attribute_columns) {
                get_set_attribute(attribute_column, row, rowElement);
                context.safepoint();
            }
            rootElement.appendChild(rowElement);
            context.safepoint();
        }

        return convert_to_string(doc);
    }

    private static String convert_to_string(Document doc) throws XmlException {
        var xmlObject = XmlObject.Factory.parse(doc);
        var options = new XmlOptions();
        options.setSavePrettyPrint();

        String xmlString = xmlObject.xmlText(options);

        return xmlString;
    }

    private static void get_set_attribute(Column attribute_column, int row, Element rowElement)
            throws DOMException {
        var item = attribute_column.getStorage().getItemBoxed(row);
        if (item != null) {
            rowElement.setAttribute(makeXmlTagNameLegal(attribute_column.getName()), item.toString());
        }
    }

    private static void get_append_element(
            Column element_column, int row, Document doc, Element rowElement) throws DOMException {
        var item = element_column.getStorage().getItemBoxed(row);
        if (item != null) {
            var columnElement = doc.createElement(makeXmlTagNameLegal(element_column.getName()));
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
