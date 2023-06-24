package org.enso.tools.enso4igv.enso;

import javax.swing.text.PlainDocument;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

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
        var result = s.getStructure(doc);
        assertEquals("Three elements: " + result, 3, result.size());
    }
}
