package org.enso.base;

import java.io.ByteArrayOutputStream;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import org.w3c.dom.Element;

public class XML_Utils {
    // This is outerXML
    public static String outerXML(Element element) throws TransformerException {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        Transformer transformer = TransformerFactory.newInstance().newTransformer();
        transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
        Source source = new DOMSource(element);
        Result target = new StreamResult(out);
        transformer.transform(source, target);
        return out.toString();
    }
}