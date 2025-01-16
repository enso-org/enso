package org.enso.table.excel.xssfreader;

import java.io.IOException;
import java.nio.channels.ClosedByInterruptException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.apache.poi.ooxml.util.DocumentHelper;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.exceptions.OpenXML4JException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.openxml4j.opc.PackageAccess;
import org.apache.poi.ss.usermodel.RichTextString;
import org.apache.poi.xssf.eventusermodel.XSSFReader;
import org.apache.poi.xssf.model.SharedStrings;
import org.apache.poi.xssf.usermodel.XSSFRelation;
import org.enso.table.excel.ExcelSheet;
import org.enso.table.excel.ExcelWorkbook;
import org.enso.table.util.ConsumerWithException;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public class XSSFReaderWorkbook implements ExcelWorkbook {
  private static final XPathFactory xpathFactory = XPathFactory.newInstance();
  private static final NamespaceContext namespaceContext = new SpreadsheetContext();
  private static final Map<String, XPathExpression> xpathCache = new HashMap<>();

  private static XPathExpression compileXPathWithNamespace(String xpath)
      throws XPathExpressionException {
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
      return prefix.equals("ss") ? XSSFRelation.NS_SPREADSHEETML : XMLConstants.NULL_NS_URI;
    }

    @Override
    public String getPrefix(String namespaceURI) {
      if (namespaceURI == null) {
        throw new IllegalArgumentException("namespaceURI cannot be null");
      }
      return namespaceURI.equals(XSSFRelation.NS_SPREADSHEETML) ? "ss" : null;
    }

    @Override
    public Iterator<String> getPrefixes(String namespaceURI) {
      if (namespaceURI == null) {
        throw new IllegalArgumentException("namespaceURI cannot be null");
      }
      return namespaceURI.equals(XSSFRelation.NS_SPREADSHEETML)
          ? Collections.singleton("ss").iterator()
          : Arrays.stream(new String[0]).iterator();
    }
  }

  public static final String WORKBOOK_CONFIG_XPATH = "/ss:workbook/ss:workbookPr";
  public static final String SHEET_NAME_XPATH = "/ss:workbook/ss:sheets/ss:sheet";
  public static final String NAMED_RANGE_XPATH = "/ss:workbook/ss:definedNames/ss:definedName";

  private final String path;

  private boolean use1904DateSystemFlag = false;
  private List<SheetInfo> sheetInfos;
  private Map<String, SheetInfo> sheetInfoMap;
  private Map<String, NamedRange> namedRangeMap;

  private boolean hasReadShared = false;
  private SharedStrings sharedStrings;
  private XSSFReaderFormats styles;

  public XSSFReaderWorkbook(String path) throws IOException, InterruptedException {
    this.path = path;

    // Read the workbook data
    this.readWorkbookData();
  }

  public String getPath() {
    return path;
  }

  void withReader(ConsumerWithException<XSSFReader, InterruptedException> action)
      throws IOException, InterruptedException {
    try (var pkg = OPCPackage.open(path, PackageAccess.READ)) {
      var reader = new XSSFReader(pkg);
      action.accept(reader);
    } catch (OpenXML4JException e) {
      throw new IOException(
          "Invalid format encountered when opening the file " + path + " as XLSX.", e);
    }
  }

  private record SheetInfo(int index, int sheetId, String name, String relID, boolean visible) {}

  private record NamedRange(String name, String formula) {}

  private void readWorkbookData() throws IOException, InterruptedException {
    withReader(
        reader -> {
          try {
            var workbookData = reader.getWorkbookData();
            var workbookDoc = DocumentHelper.readDocument(workbookData);
            read1904DateSetting(workbookDoc);
            readSheetInfo(workbookDoc);
            readNamedRanges(workbookDoc);
          } catch (ClosedByInterruptException e) {
            throw new InterruptedException(e.getMessage());
          } catch (SAXException
              | IOException
              | InvalidFormatException
              | XPathExpressionException e) {
            throw new RuntimeException(e);
          }
        });
  }

  private void readNamedRanges(Document workbookDoc) throws XPathExpressionException {
    var namesXPath = compileXPathWithNamespace(NAMED_RANGE_XPATH);
    var nameNodes = (NodeList) namesXPath.evaluate(workbookDoc, XPathConstants.NODESET);
    namedRangeMap = new HashMap<>();
    for (int i = 0; i < nameNodes.getLength(); i++) {
      var node = nameNodes.item(i);
      var name = node.getAttributes().getNamedItem("name").getNodeValue();
      var formula = node.getTextContent();
      namedRangeMap.put(name, new NamedRange(name, formula));
    }
  }

  private void readSheetInfo(Document workbookDoc) throws XPathExpressionException {
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
      var sheetInfo = new SheetInfo(i, sheetId, sheetName, relId, visible);
      sheetInfos.add(sheetInfo);
      sheetInfoMap.put(sheetName, sheetInfo);
    }
  }

  private void read1904DateSetting(Document workbookDoc) throws XPathExpressionException {
    var workbookXPath = compileXPathWithNamespace(WORKBOOK_CONFIG_XPATH);
    var workbookNode = (Node) workbookXPath.evaluate(workbookDoc, XPathConstants.NODE);
    if (workbookNode != null) {
      var date1904 = workbookNode.getAttributes().getNamedItem("date1904");
      use1904DateSystemFlag = date1904 != null && "1".equals(date1904.getNodeValue());
    }
  }

  private synchronized void ensureReadShared() throws InterruptedException {
    if (hasReadShared) {
      return;
    }

    try {
      withReader(
          reader -> {
            try {
              reader.setUseReadOnlySharedStringsTable(true);
              sharedStrings = reader.getSharedStringsTable();
              if (sharedStrings == null) {
                sharedStrings =
                    new SharedStrings() {
                      @Override
                      public RichTextString getItemAt(int idx) {
                        return null;
                      }

                      @Override
                      public int getCount() {
                        return 0;
                      }

                      @Override
                      public int getUniqueCount() {
                        return 0;
                      }
                    };
              }

              // Read the styles table and attach the format data
              var stylesTable = reader.getStylesTable();
              styles = new XSSFReaderFormats(stylesTable);

              hasReadShared = true;
            } catch (ClosedByInterruptException e) {
              throw new InterruptedException(e.getMessage());
            } catch (InvalidFormatException | IOException e) {
              throw new RuntimeException(e);
            }
          });
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  /** Flag that workbook is in 1904 format. */
  boolean use1904Format() {
    return use1904DateSystemFlag;
  }

  @Override
  public int getNumberOfSheets() {
    return sheetInfoMap.size();
  }

  @Override
  public int getSheetIndex(String name) {
    if (!sheetInfoMap.containsKey(name)) {
      return -1;
    }
    return sheetInfoMap.get(name).index;
  }

  @Override
  public String getSheetName(int sheet) {
    if (sheet < 0 || sheet >= sheetInfos.size()) {
      throw new IllegalArgumentException("Sheet index out of range: " + sheet);
    }
    return sheetInfos.get(sheet).name;
  }

  @Override
  public int getNumberOfNames() {
    return namedRangeMap.size();
  }

  @Override
  public String[] getRangeNames() {
    return namedRangeMap.keySet().toArray(String[]::new);
  }

  @Override
  public String getNameFormula(String name) {
    var namedRange = namedRangeMap.get(name);
    return namedRange == null ? null : namedRange.formula;
  }

  public SharedStrings getSharedStrings() throws InterruptedException {
    ensureReadShared();
    return sharedStrings;
  }

  public XSSFReaderFormats getStyles() throws InterruptedException {
    ensureReadShared();
    return styles;
  }

  @Override
  public ExcelSheet getSheetAt(int sheetIndex) {
    if (sheetIndex < 0 || sheetIndex >= sheetInfos.size()) {
      throw new IllegalArgumentException("Sheet index out of range: " + sheetIndex);
    }
    var sheetInfo = sheetInfos.get(sheetIndex);
    return new XSSFReaderSheet(sheetIndex, sheetInfo.name, sheetInfo.relID, this);
  }

  @Override
  public void close() throws IOException {
    // Nothing to do
  }
}
