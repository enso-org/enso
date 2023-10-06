package org.enso.base;

import java.io.ByteArrayOutputStream;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
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
  public static String outerXML(Element element) throws TransformerException {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    Transformer transformer = TransformerFactory.newInstance().newTransformer();
    transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
    Source source = new DOMSource(element);
    Result target = new StreamResult(out);
    transformer.transform(source, target);
    return out.toString();
  }

  /**
   * Return the string representation of the contents of an XML element, not including its tag.
   *
   * @param element the element to convert to a string
   * @return the string representation of the element's contents
   * @throws TransformerException
   */
  public static String innerXML(Element element) throws TransformerException {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    Transformer transformer = TransformerFactory.newInstance().newTransformer();
    transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
    Result target = new StreamResult(out);
    NodeList childNodes = element.getChildNodes();
    for (int i = 0; i < childNodes.getLength(); ++i) {
      Source source = new DOMSource(childNodes.item(i));
      transformer.transform(source, target);
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
