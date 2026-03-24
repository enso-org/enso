package org.enso.base;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPathFactory;
import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;
import org.w3c.dom.*;
import org.w3c.dom.bootstrap.DOMImplementationRegistry;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSOutput;
import org.w3c.dom.ls.LSSerializer;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**
 * Wrapper for XML functions in Enso. Public functions should handle exceptions and return as Enso
 * Errors.
 */
public class XML_Utils {
  private XML_Utils() {}

  /**
   * Gets the Document Element of an XML Document.
   *
   * @param document the document to get the element from.
   * @return the root XML element.
   */
  public static Element getDocumentElement(Document document) {
    return document.getDocumentElement();
  }

  /**
   * Gets the name of an XML element, which is the tag name for element nodes.
   *
   * @param element the XML element to get the name of.
   * @return the tag name.
   */
  public static String getNodeName(Element element) {
    return element.getNodeName();
  }

  private static Value handleExceptions(Exception exception) {
    if (exception instanceof SAXParseException parseException) {
      var parseError =
          EnsoMeta.makeInstance(
              "Standard.Base.Data.XML",
              "XML_Error",
              "Parse_Error",
              parseException.getLineNumber(),
              parseException.getColumnNumber());
      return EnsoMeta.asDataflowError(parseError);
    }

    var ensoError =
        EnsoMeta.makeInstance(
            "Standard.Base.Data.XML",
            "XML_Error",
            "Other",
            "An Exception has occurred: " + exception);
    return EnsoMeta.asDataflowError(ensoError);
  }

  /**
   * Return the string representation of an XML element, including its tag and all its contents.
   *
   * @param element the element to convert to a string
   * @param prettyPrint whether to format the output with indentation and new lines
   * @return the string representation of the element
   */
  public static Value outerXML(Node element, boolean prettyPrint) {
    try {
      var value = outerXMLImplementation(element, prettyPrint);
      return Value.asValue(value);
    } catch (Exception e) {
      return handleExceptions(e);
    }
  }

  /**
   * Return the string representation of an XML element, including its tag and all its contents.
   *
   * @param element the element to convert to a string
   * @param prettyPrint whether to format the output with indentation and new lines
   * @return the string representation of the element
   * @throws ClassNotFoundException if the DOM implementation class cannot be found.
   * @throws IllegalAccessException if the DOM implementation class cannot be accessed.
   * @throws InstantiationException if the DOM implementation class cannot be instantiated.
   */
  private static String outerXMLImplementation(Node element, boolean prettyPrint)
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
   * Return the string representation of an XML element, not including its tag.
   *
   * @param element the element to convert to a string
   * @return the string representation of the element
   */
  public static Value innerXML(Node element) {
    try {
      var value = innerXMLImplementation(element);
      return Value.asValue(value);
    } catch (Exception e) {
      return handleExceptions(e);
    }
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
  private static String innerXMLImplementation(Node element)
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

  /**
   * Read an XML document from an input stream and return it as an Enso XML_Document.
   *
   * @param is the input stream to read the XML document from.
   * @param makeXMLDocument a function that takes a Document and returns an Enso XML_Document
   *     instance.
   * @return the parsed XML document or an Enso error if parsing failed.
   * @throws IOException if an IO exception occurs reading the stream.
   */
  public static Value parseStream(InputStream is, Function<Document, Value> makeXMLDocument)
      throws IOException {
    try {
      var document = doParse(new InputSource(is));
      var value = makeXMLDocument.apply(document);
      return Value.asValue(value);
    } catch (IOException e) {
      // Pass through any IO exception to be handled in Enso
      throw e;
    } catch (Exception e) {
      return handleExceptions(e);
    }
  }

  /**
   * Parses a String value into an XML Document, returning an Enso XML_Document object.
   *
   * @param text the String value to parse.
   * @param makeXMLDocument a function that takes a Document and returns an Enso XML_Document
   *     instance.
   * @return the parsed XML document or an Enso error if parsing failed.
   */
  public static Value parseString(String text, Function<Document, Value> makeXMLDocument) {
    try {
      var document = doParse(new InputSource(new StringReader(text)));
      var value = makeXMLDocument.apply(document);
      return Value.asValue(value);
    } catch (Exception e) {
      return handleExceptions(e);
    }
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

  private static Function<ArrayList<Value>, Value> vectorConstructor;

  /**
   * Given an XML Element or Document, an XPath query, get the matching set.
   *
   * @param source the element or document to query.
   * @param xpath an XPath query to execute on the source.
   * @param makeXMLElement factory function to make Enso XML_Element.
   * @param firstOnly if true then stop after the first value.
   * @return an Enso Vector of matching results.
   */
  public static Value getXPath(
      Object source, String xpath, Function<Element, Value> makeXMLElement, boolean firstOnly) {
    try {
      var factory = XPathFactory.newInstance().newXPath();

      var nodeSet = factory.evaluate(xpath, source, javax.xml.xpath.XPathConstants.NODESET);
      if (!(nodeSet instanceof NodeList nodeList)) {
        throw new IllegalStateException(
            "Unexpected result from XPath evaluation: expected a NodeList");
      }

      return convertNodeListToVector(makeXMLElement, firstOnly, nodeList);
    } catch (Exception e) {
      return handleExceptions(e);
    }
  }

  /**
   * Given an XML Element, make an Enso vector of all the child nodes.
   *
   * @param element the element to get the children of.
   * @param makeXMLElement factory function to make Enso XML_Element.
   * @return an Enso Vector of child nodes.
   */
  public static Value getChildren(Element element, Function<Element, Value> makeXMLElement) {
    try {
      var children = element.getChildNodes();
      return convertNodeListToVector(makeXMLElement, false, children);
    } catch (Exception e) {
      return handleExceptions(e);
    }
  }

  /**
   * Given an XML Element, make an Enso vector of all the matching descendant tag nodes.
   *
   * @param element the element to get the children of.
   * @param tagName the name of the child nodes to match.
   * @param makeXMLElement factory function to make Enso XML_Element.
   * @return an Enso Vector of child nodes.
   */
  public static Value getElementsByTagName(
      Element element, String tagName, Function<Element, Value> makeXMLElement) {
    try {
      var children = element.getElementsByTagName(tagName);
      return convertNodeListToVector(makeXMLElement, false, children);
    } catch (Exception e) {
      return handleExceptions(e);
    }
  }

  private static Value convertNodeListToVector(
      Function<Element, Value> makeXMLElement, boolean firstOnly, NodeList nodeList) {
    var result = new ArrayList<Value>();
    for (int i = 0; i < nodeList.getLength(); i++) {
      var node = nodeList.item(i);
      if (isWantedNode(node)) {
        result.add(convert(node, makeXMLElement));
        if (firstOnly) {
          break;
        }
      }
    }

    if (vectorConstructor == null) {
      var vectorType = EnsoMeta.getType("Standard.Base.Data.Vector", "Vector");
      var method = vectorType.getMember("from_polyglot_array");
      vectorConstructor = arr -> method.execute(vectorType, arr);
    }

    return vectorConstructor.apply(result);
  }

  private static boolean isWantedNode(Node node) {
    return switch (node.getNodeType()) {
      case Node.ELEMENT_NODE, Node.ATTRIBUTE_NODE -> true;
      case Node.TEXT_NODE -> !node.getNodeValue().trim().isEmpty();
      default -> false;
    };
  }

  private static Value convert(Node node, Function<Element, Value> makeXMLElement) {
    return switch (node) {
      case Attr attr -> Value.asValue(node.getNodeValue());
      case Text textNode -> Value.asValue(node.getTextContent());
      case Element elt -> makeXMLElement.apply(elt);
      default -> throw new IllegalStateException("Unexpected value: " + node.getNodeValue());
    };
  }

  /**
   * Gets the text content of an XML element, which is the concatenation of the text content of all
   * its child text nodes. If the element has no text content, returns an empty string.
   *
   * @param element the XML element to get the text content of.
   * @return the text of the XML element, or an Enso error if an exception occurs.
   */
  public static Value getText(Element element) {
    try {
      var text = element.getTextContent();
      return Value.asValue(text);
    } catch (Exception e) {
      return handleExceptions(e);
    }
  }

  /**
   * Gets the value of an attribute of an XML element. If the attribute does not exist, returns
   * Nothing.
   *
   * @param elt the element to query
   * @param attrName the name of the attribute to match
   * @return the value of the attribute, or Nothing if the attribute does not exist
   */
  public static Value getAttribute(Element elt, String attrName) {
    var attrNode = elt.getAttributeNode(attrName);
    var attrValue = attrNode == null ? null : attrNode.getNodeValue();
    return Value.asValue(attrValue);
  }

  /**
   * Gets a list of attribute names
   *
   * @param elt the element to query
   * @return a list of attribute names
   */
  public static List<String> getAttributeNames(Element elt) {
    var namedNodeMap = elt.getAttributes();
    var results = new ArrayList<String>(namedNodeMap.getLength());
    for (int i = 0; i < namedNodeMap.getLength(); i++) {
      results.add(namedNodeMap.item(i).getNodeName());
    }
    return results;
  }

  /**
   * Gets a Map attribute names and values for an element.
   *
   * @param elt the element to query
   * @return a Map of attribute names and values
   */
  public static Map<String, String> getAttributes(Element elt) {
    var namedNodeMap = elt.getAttributes();
    var results = new HashMap<String, String>(namedNodeMap.getLength());
    for (int i = 0; i < namedNodeMap.getLength(); i++) {
      results.put(namedNodeMap.item(i).getNodeName(), namedNodeMap.item(i).getNodeValue());
    }
    return results;
  }
}
