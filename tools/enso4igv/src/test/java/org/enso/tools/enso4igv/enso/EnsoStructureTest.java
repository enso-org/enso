package org.enso.tools.enso4igv.enso;

import java.util.List;
import javax.swing.text.PlainDocument;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.netbeans.api.lsp.StructureElement;
import org.netbeans.api.lsp.StructureElement.Kind;

public class EnsoStructureTest {

  public EnsoStructureTest() {
  }

  @Test
  public void processASimpleDocument() throws Exception {
    var doc = new PlainDocument();
    doc.insertString(0, """
        type B
            T
            F
        """, null);

    var s = new EnsoStructure();
    var root = s.getStructure(doc);
    assertEquals("One root element: " + root, 1, root.size());
    assertEquals("Type is class", StructureElement.Kind.Class, root.get(0).getKind());
    final List<StructureElement> chldrn = root.get(0).getChildren();
    assertEquals("Has two children", 2, chldrn.size());
    assertEquals("Constructor is it at 0", StructureElement.Kind.Constructor, chldrn.get(0).getKind());
    assertEquals("Constructor is it at 1", StructureElement.Kind.Constructor, chldrn.get(1).getKind());
  }

  @Test
  public void collectMethods() throws Exception {
    var doc = new PlainDocument();
    doc.insertString(0, """
        main = 6 * 7
        """, null);

    var s = new EnsoStructure();
    var root = s.getStructure(doc);
    assertEquals("One root element: " + root, 1, root.size());
    assertEquals("It is a method", StructureElement.Kind.Method, root.get(0).getKind());
    assertEquals("It is a method", "main", root.get(0).getName());
  }

  @Test
  public void collectMethodInType() throws Exception {
    var doc = new PlainDocument();
    doc.insertString(0, """
        type My_Type
            my_method self = 42
        """, null);
    var s = new EnsoStructure();
    var root = s.getStructure(doc);
    assertEquals(1, root.size());
    assertEquals("My_Type is class", Kind.Class, root.get(0).getKind());
    var children = root.get(0).getChildren();
    assertEquals("Has 1 child", 1, children.size());
    assertEquals("my_method is method", Kind.Method, children.get(0).getKind());
  }

  @Test
  public void collectExtensionMethod() throws Exception {
    var doc = new PlainDocument();
    doc.insertString(0, """
        type My_Type
        My_Type.extension_method self = 42
        """, null);
    var s = new EnsoStructure();
    var root = s.getStructure(doc);
    assertEquals(2, root.size());
    assertEquals("My_Type is class", Kind.Class, root.get(0).getKind());
    assertEquals("My_Type.extension_method is method", Kind.Method, root.get(1).getKind());
  }
}
