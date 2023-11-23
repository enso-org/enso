package org.enso.base;

import java.io.ByteArrayOutputStream;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.transform.TransformerException;
import org.w3c.dom.DOMConfiguration;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.bootstrap.DOMImplementationRegistry;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSOutput;
import org.w3c.dom.ls.LSSerializer;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

public class XML_Utils {
  /**
   * Return the string representation of an XML element, including its tag and all its contents.
   *
   * @param element the element to convert to a string
   * @return the string representation of the element
   * @throws TransformerException
   */
  public static String outerXML(Element element)
      throws ClassNotFoundException, IllegalAccessException, InstantiationException {
    DOMImplementationLS dom =
        (DOMImplementationLS) DOMImplementationRegistry.newInstance().getDOMImplementation("LS");
    LSSerializer serializer = dom.createLSSerializer();
    DOMConfiguration config = serializer.getDomConfig();
    config.setParameter("xml-declaration", false);
    serializer.setNewLine("\n");
    return serializer.writeToString(element);
  }

  /**
   * Return the string representation of the contents of an XML element, not including its tag.
   *
   * @param element the element to convert to a string
   * @return the string representation of the element's contents
   * @throws TransformerException
   */
  public static String innerXML(Element element)
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

  public static void setCustomErrorHandler(DocumentBuilder documentBuilder) {
    documentBuilder.setErrorHandler(
        new ErrorHandler() {
          @Override
          public void warning(SAXParseException e) throws SAXException {
            ;
          }

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
