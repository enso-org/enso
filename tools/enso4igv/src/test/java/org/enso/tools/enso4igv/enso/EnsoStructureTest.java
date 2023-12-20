package org.enso.tools.enso4igv.enso;

import java.util.List;
import javax.swing.text.PlainDocument;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.netbeans.api.lsp.StructureElement;

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
}
