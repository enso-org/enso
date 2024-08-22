package org.enso.table.excel.xssfreader;

import org.apache.poi.ooxml.util.DocumentHelper;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.openxml4j.opc.PackageAccess;
import org.apache.poi.xssf.eventusermodel.XSSFReader;
import org.apache.poi.xssf.model.SharedStrings;
import org.apache.poi.xssf.model.Styles;
import org.apache.poi.xssf.usermodel.XSSFRelation;
import org.enso.table.excel.ExcelSheet;
import org.enso.table.excel.ExcelWorkbook;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

/** Implementation of ExcelWorkbook backed by a XSSFReader */
public class XSSFReaderWorkbook implements ExcelWorkbook {
  private static final XPathFactory xpathFactory = XPathFactory.newInstance();
  private static final NamespaceContext namespaceContext = new SpreadsheetContext();
  private static final Map<String, XPathExpression> xpathCache = new HashMap<>();

  private static XPathExpression compileXPathWithNamespace(String xpath) throws XPathExpressionException {
    if (!xpathCache.containsKey(xpath)) {
      var newXPath = xpathFactory.newXPath();
      newXPath.setNamespaceContext(namespaceContext);
      var compiled = newXPath.compile(xpath);
      xpathCache.put(xpath, compiled);
    }
    return xpathCache.get(xpath);
  }

  private static class SpreadsheetContext implements NamespaceContext {
    @Override
    public String getNamespaceURI(String prefix) {
      if (prefix == null) {
        throw new IllegalArgumentException("prefix cannot be null");
      }
      return prefix.equals("ss")
          ? XSSFRelation.NS_SPREADSHEETML
          : XMLConstants.NULL_NS_URI;
    }

    @Override
    public String getPrefix(String namespaceURI) {
      if (namespaceURI == null) {
        throw new IllegalArgumentException("namespaceURI cannot be null");
      }
      return namespaceURI.equals(XSSFRelation.NS_SPREADSHEETML)
          ? "ss"
          : null;
    }

    @Override
    public Iterator<String> getPrefixes(String namespaceURI) {
      if (namespaceURI == null) {
        throw new IllegalArgumentException("namespaceURI cannot be null");
      }

      return namespaceURI.equals(XSSFRelation.NS_SPREADSHEETML)
          ? Collections.singleton("ss").iterator()
          : Collections.emptyIterator();
    }
  }

  private static final String SHEET_NAME_XPATH = "/ss:workbook/ss:sheets/ss:sheet";
  private static final String NAMED_RANGE_XPATH = "/ss:workbook/ss:definedNames/ss:definedName";

  private final String path;

  private boolean readWorkbookData = false;
  private List<SheetInfo> sheetInfos;
  private Map<String, SheetInfo> sheetInfoMap;
  private Map<String, NamedRange> namedRangeMap;

  private boolean readShared = false;
  private SharedStrings sharedStrings;
  private Styles styles;

  public XSSFReaderWorkbook(String path) {
    this.path = path;
  }

  public String getPath() {
    return path;
  }

  void withReader(Consumer<XSSFReader> action) {
    try (var pkg = OPCPackage.open(path, PackageAccess.READ)) {
      var reader = new XSSFReader(pkg);
      action.accept(reader);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  private record SheetInfo(int index, String name, String relID, boolean visible) {
  }

  private record NamedRange(String name, String formula) {
  }

  private synchronized void readWorkbookData() {
    if (readWorkbookData) {
      return;
    }

    withReader(rdr -> {
      try {
        var workbookData = rdr.getWorkbookData();
        var workbookDoc = DocumentHelper.readDocument(workbookData);

        // Read the Sheets
        var sheetXPath = compileXPathWithNamespace(SHEET_NAME_XPATH);
        var sheetNodes = (NodeList) sheetXPath.evaluate(workbookDoc, XPathConstants.NODESET);
        sheetInfos = new ArrayList<>(sheetNodes.getLength());
        sheetInfoMap = new HashMap<>();
        for (int i = 0; i < sheetNodes.getLength(); i++) {
          var node = sheetNodes.item(i);
          var sheetName = node.getAttributes().getNamedItem("name").getNodeValue();
          var sheetId = Integer.parseInt(node.getAttributes().getNamedItem("sheetId").getNodeValue());
          var relId = node.getAttributes().getNamedItem("r:id").getNodeValue();
          var visible = node.getAttributes().getNamedItem("state") == null;
          var sheetInfo = new SheetInfo(sheetId, sheetName, relId, visible);
          sheetInfos.add(sheetInfo);
          sheetInfoMap.put(sheetName, sheetInfo);
        }

        // Read the Named Ranges
        var namesXPath = compileXPathWithNamespace(NAMED_RANGE_XPATH);
        var nameNodes = (NodeList) namesXPath.evaluate(workbookDoc, XPathConstants.NODESET);
        namedRangeMap = new HashMap<>();
        for (int i = 0; i < nameNodes.getLength(); i++) {
          var node = nameNodes.item(i);
          var name = node.getAttributes().getNamedItem("name").getNodeValue();
          var formula = node.getTextContent();
          namedRangeMap.put(name, new NamedRange(name, formula));
        }

        // Mark as read
        readWorkbookData = true;
      } catch (SAXException | IOException | InvalidFormatException | XPathExpressionException e) {
        throw new RuntimeException(e);
      }
    });
  }

  private synchronized void readShared() {
    if (readShared) {
      return;
    }

    withReader(rdr -> {
      try {
        rdr.setUseReadOnlySharedStringsTable(true);
        sharedStrings = rdr.getSharedStringsTable();
        styles = rdr.getStylesTable();
        readShared = true;
      } catch (InvalidFormatException | IOException e) {
        throw new RuntimeException(e);
      }
    });
  }

  private Map<String, SheetInfo> getSheetInfoMap() {
    if (!readWorkbookData) {
      readWorkbookData();
    }
    return sheetInfoMap;
  }

  private List<SheetInfo> getSheetInfos() {
    if (!readWorkbookData) {
      readWorkbookData();
    }
    return sheetInfos;
  }

  private Map<String, NamedRange> getNamesMap() {
    if (!readWorkbookData) {
      readWorkbookData();
    }
    return namedRangeMap;
  }

  @Override
  public int getNumberOfSheets() {
    return getSheetInfoMap().size();
  }

  @Override
  public int getSheetIndex(String name) {
    var map = getSheetInfoMap();
    if (!map.containsKey(name)) {
      throw new IllegalArgumentException("Sheet not found: " + name);
    }
    return map.get(name).index;
  }

  @Override
  public String getSheetName(int sheet) {
    var list = getSheetInfos();
    if (sheet < 0 || sheet >= list.size()) {
      throw new IllegalArgumentException("Sheet index out of range: " + sheet);
    }
    return list.get(sheet).name;
  }

  @Override
  public int getNumberOfNames() {
    return getNamesMap().size();
  }

  @Override
  public String[] getRangeNames() {
    return getNamesMap().keySet().toArray(String[]::new);
  }

  @Override
  public String getNameFormula(String name) {
    var map = getNamesMap();
    var namedRange = map.get(name);
    return namedRange == null ? null : namedRange.formula;
  }

  SharedStrings getSharedStrings() {
    if (!readShared) {
      readShared();
    }
    return sharedStrings;
  }

  Styles getStyles() {
    if (!readShared) {
      readShared();
    }
    return styles;
  }

  @Override
  public ExcelSheet getSheetAt(int sheetIndex) {
    var sheetInfos = getSheetInfos();
    if (sheetIndex < 0 || sheetIndex >= sheetInfos.size()) {
      throw new IllegalArgumentException("Sheet index out of range: " + sheetIndex);
    }
    var sheetInfo = sheetInfos.get(sheetIndex);
    return new XSSFReaderSheet(sheetIndex, sheetInfo.name, sheetInfo.relID, this);
  }
}

