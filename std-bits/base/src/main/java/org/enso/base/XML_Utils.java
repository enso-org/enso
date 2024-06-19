package org.enso.base;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.DOMConfiguration;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.bootstrap.DOMImplementationRegistry;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSOutput;
import org.w3c.dom.ls.LSSerializer;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

public class XML_Utils {
  /**
   * Return the string representation of an XML element, including its tag and all its contents.
   *
   * @param element the element to convert to a string
   * @return the string representation of the element
   * @throws ClassNotFoundException if the DOM implementation class cannot be found.
   * @throws IllegalAccessException if the DOM implementation class cannot be accessed.
   * @throws InstantiationException if the DOM implementation class cannot be instantiated.
   */
  public static String outerXML(Node element, boolean prettyPrint)
      throws ClassNotFoundException, IllegalAccessException, InstantiationException {
    DOMImplementationLS dom =
        (DOMImplementationLS) DOMImplementationRegistry.newInstance().getDOMImplementation("LS");
    LSSerializer serializer = dom.createLSSerializer();
    DOMConfiguration config = serializer.getDomConfig();
    config.setParameter("xml-declaration", false);
    config.setParameter("format-pretty-print", prettyPrint);
    serializer.setNewLine("\n");
    return serializer.writeToString(element);
  }

  /**
   * Return the string representation of the contents of an XML element, not including its tag.
   *
   * @param element the element to convert to a string
   * @return the string representation of the element's contents
   * @throws ClassNotFoundException if the DOM implementation class cannot be found.
   * @throws IllegalAccessException if the DOM implementation class cannot be accessed.
   * @throws InstantiationException if the DOM implementation class cannot be instantiated.
   */
  public static String innerXML(Node element)
      throws ClassNotFoundException, IllegalAccessException, InstantiationException {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    DOMImplementationLS dom =
        (DOMImplementationLS) DOMImplementationRegistry.newInstance().getDOMImplementation("LS");
    LSSerializer serializer = dom.createLSSerializer();
    DOMConfiguration config = serializer.getDomConfig();
    config.setParameter("xml-declaration", false);
    serializer.setNewLine("\n");
    NodeList childNodes = element.getChildNodes();
    LSOutput output = dom.createLSOutput();
    output.setByteStream(out);
    for (int i = 0; i < childNodes.getLength(); ++i) {
      serializer.write(childNodes.item(i), output);
    }
    return out.toString();
  }

  public static Document parseStream(InputStream is)
      throws ParserConfigurationException, SAXException, IOException {
    return doParse(new InputSource(is));
  }

  public static Document parseString(String text)
      throws ParserConfigurationException, SAXException, IOException {
    return doParse(new InputSource(new StringReader(text)));
  }

  private static Document doParse(InputSource is)
      throws ParserConfigurationException, SAXException, IOException {
    var factory = DocumentBuilderFactory.newInstance();
    var builder = factory.newDocumentBuilder();
    configureErrorHandler(builder);
    return builder.parse(is);
  }

  private static void configureErrorHandler(DocumentBuilder documentBuilder) {
    documentBuilder.setErrorHandler(
        new ErrorHandler() {
          @Override
          public void warning(SAXParseException e) {}

          @Override
          public void fatalError(SAXParseException e) throws SAXException {
            throw e;
          }

          @Override
          public void error(SAXParseException e) throws SAXException {
            throw e;
          }
        });
  }
}
