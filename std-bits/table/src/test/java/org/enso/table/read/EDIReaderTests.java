package org.enso.table.read;

import static org.junit.Assert.*;

import java.util.HashMap;
import org.junit.Test;

public class EDIReaderTests {
  @Test
  public void testStructureSingle() {
    var simpleField = "N1";
    var structure = EDIReader.EDIStructure.parse(simpleField);
    assertEquals(simpleField, structure.name());
    assertFalse(structure.isArray());
    assertFalse(structure.isObject());
    assertEquals(2, structure.charLength());
    assertEquals(simpleField, structure.toString());
  }

  @Test
  public void testStructureArray() {
    var simpleField = "[N1]";
    var structure = EDIReader.EDIStructure.parse(simpleField);
    assertEquals("N1", structure.name());
    assertTrue(structure.isArray());
    assertFalse(structure.isObject());
    assertEquals(4, structure.charLength());
    assertEquals(simpleField, structure.toString());

    var child = structure.child("N1");
    assertNotNull(child);
    assertEquals("N1", child.name());
    assertFalse(child.isArray());
    assertFalse(child.isObject());
    assertEquals(2, child.charLength());
    assertEquals("N1", child.toString());
  }

  @Test
  public void testStructureObject() {
    var simpleField = "{N1}";
    var structure = EDIReader.EDIStructure.parse(simpleField);
    assertEquals("N1", structure.name());
    assertFalse(structure.isArray());
    assertTrue(structure.isObject());
    assertEquals(4, structure.charLength());
    assertEquals(simpleField, structure.toString());

    var child = structure.child("N1");
    assertNotNull(child);
    assertEquals("N1", child.name());
    assertFalse(child.isArray());
    assertFalse(child.isObject());
    assertEquals(2, child.charLength());
    assertEquals("N1", child.toString());
  }

  @Test
  public void testStructureArrayWithChildren() {
    var simpleField = "[N1,N3,PER,DEL]";
    var structure = EDIReader.EDIStructure.parse(simpleField);
    assertEquals("N1", structure.name());
    assertTrue(structure.isArray());
    assertFalse(structure.isObject());
    assertEquals(simpleField.length(), structure.charLength());
    assertEquals(simpleField, structure.toString());

    var child = structure.child("N1");
    assertNotNull(child);
    assertEquals("N1", child.name());
    assertFalse(child.isArray());
    assertFalse(child.isObject());
    assertEquals(2, child.charLength());
    assertEquals("N1", child.toString());

    var child2 = structure.child("PER");
    assertNotNull(child2);
    assertEquals("PER", child2.name());
    assertFalse(child2.isArray());
    assertFalse(child2.isObject());
    assertEquals(3, child2.charLength());
    assertEquals("PER", child2.toString());
  }

  @Test
  public void testStructureObjectWithChildren() {
    var simpleField = "{N1,N3,PER,DEL}";
    var structure = EDIReader.EDIStructure.parse(simpleField);
    assertEquals("N1", structure.name());
    assertFalse(structure.isArray());
    assertTrue(structure.isObject());
    assertEquals(simpleField.length(), structure.charLength());
    assertEquals(simpleField, structure.toString());

    var child = structure.child("N1");
    assertNotNull(child);
    assertEquals("N1", child.name());
    assertFalse(child.isArray());
    assertFalse(child.isObject());
    assertEquals(2, child.charLength());
    assertEquals("N1", child.toString());

    var child2 = structure.child("PER");
    assertNotNull(child2);
    assertEquals("PER", child2.name());
    assertFalse(child2.isArray());
    assertFalse(child2.isObject());
    assertEquals(3, child2.charLength());
    assertEquals("PER", child2.toString());
  }

  @Test
  public void testStructureNestedArray() {
    var simpleField = "[N1,[N3,PER],DEL]";
    var structure = EDIReader.EDIStructure.parse(simpleField);
    assertEquals("N1", structure.name());
    assertTrue(structure.isArray());
    assertFalse(structure.isObject());
    assertEquals(simpleField.length(), structure.charLength());
    assertEquals(simpleField, structure.toString());
    assertEquals("[N3,PER]", structure.child("N3").toString());

    var child = structure.child("N3");
    assertNotNull(child);
    assertEquals("N3", child.name());
    assertTrue(child.isArray());
    assertFalse(child.isObject());
    assertEquals(8, child.charLength());
    assertEquals("[N3,PER]", child.toString());
  }

  @Test
  public void testStructureNestedObject() {
    var simpleField = "[N1,{N2,N3},N4]";
    var structure = EDIReader.EDIStructure.parse(simpleField);
    assertEquals("N1", structure.name());
    assertTrue(structure.isArray());
    assertFalse(structure.isObject());
    assertEquals(simpleField.length(), structure.charLength());
    assertEquals(simpleField, structure.toString());

    var child = structure.child("N2");
    assertNotNull(child);
    assertEquals("N2", child.name());
    assertFalse(child.isArray());
    assertTrue(child.isObject());
    assertEquals(7, child.charLength());
    assertEquals("{N2,N3}", child.toString());
  }

  @Test
  public void testFull822Structure() {
    var complexField =
        "[ISA,GS,ST,BGN,[DTM],CUR,{N1,[N2],[N3],N4,[PER]},[RTE,DTM],[ENT,[N1,[N2],[N3],N4,[PER]],[ACT,CUR,[ADJ],[RTE,DTM],[LX,[BLN],[DTM]],[SER,[CTP],DTM]]],SE,GE,IEA]";
    var structure = EDIReader.EDIStructure.parse(complexField);

    assertEquals("ISA", structure.name());
    assertTrue(structure.isArray());
    assertFalse(structure.isObject());

    // Check DTM
    var dtm = structure.child("DTM");
    assertNotNull(dtm);
    assertEquals("DTM", dtm.name());
    assertTrue(dtm.isArray());
    assertFalse(dtm.isObject());
    assertEquals("[DTM]", dtm.toString());

    // Check N1
    var n1 = structure.child("N1");
    assertNotNull(n1);
    assertEquals("N1", n1.name());
    assertFalse(n1.isArray());
    assertTrue(n1.isObject());
    assertEquals("{N1,[N2],[N3],N4,[PER]}", n1.toString());

    // Check ENT
    var ent = structure.child("ENT");
    assertNotNull(ent);
    assertEquals("ENT", ent.name());
    assertTrue(ent.isArray());
    assertFalse(ent.isObject());

    // Check N1 inside ENT
    var entN1 = ent.child("N1");
    assertNotNull(entN1);
    assertEquals("N1", entN1.name());
    assertTrue(entN1.isArray());
    assertFalse(entN1.isObject());
    assertEquals("[N1,[N2],[N3],N4,[PER]]", entN1.toString());
  }

  @Test
  public void testSegments() {
    var data =
        "ISA*00*          *00*          *12*BANKOFEXAMPLE  *12*CUSTOMERCO  "
            + " *20251009*0013*U*00401*000000001*0*P*>~\n"
            + "GS*AN*BANKOFEXAMPLE*CUSTOMERCO*20251009*0013*1*X*004010~\n"
            + "ST*822*0001~\n"
            + "BTA*20251009*100000*CC*AB~\n"
            + "N1*FI*Bank of Example*1*999999999~\n"
            + "N1*CN*Customer Account 1*92*CUST001~\n"
            + "N3*5668 Main Street~\n"
            + "N4*Newport Beach*CA*92660~\n";

    var segments = EDIReader.parseSegments(data, "");
    assertEquals(8, segments.size());
    assertEquals(17, segments.get(0).length);
    assertEquals("ISA", segments.get(0)[0]);
    assertEquals("CUSTOMERCO", segments.get(0)[8]);
    assertEquals(">", segments.get(0)[16]);
    assertEquals(5, segments.get(4).length);
    assertEquals("N4", segments.get(7)[0]);

    var segmentsNL = EDIReader.parseSegments(data, "\n");
    assertEquals(8, segmentsNL.size());
    assertEquals(17, segmentsNL.get(0).length);
    assertEquals("ISA", segmentsNL.get(0)[0]);
    assertEquals("CUSTOMERCO", segmentsNL.get(0)[8]);
    assertEquals(">~", segmentsNL.get(0)[16]);
    assertEquals(5, segmentsNL.get(4).length);
    assertEquals("N4", segmentsNL.get(7)[0]);
  }

  @Test
  public void testEDI822Read() {
    var data =
        "ISA*00*          *00*          *12*BANKOFEXAMPLE  *12*CUSTOMERCO  "
            + " *20251009*0013*U*00401*000000001*0*P*>~\n"
            + "GS*AN*BANKOFEXAMPLE*CUSTOMERCO*20251009*0013*1*X*004010~\n"
            + "ST*822*0001~\n"
            + "BTA*20251009*100000*CC*AB~";
    var segments = EDIReader.parse(data, "\n", "[ISA,GS,ST,BTA]", new HashMap<>());
    assertNotNull(segments);
  }
}
