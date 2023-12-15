package org.enso.interpreter.epb;

import java.util.Collections;

import org.junit.Test;
import static org.junit.Assert.*;

import com.oracle.truffle.api.source.Source;

public class ForeignEvalNodeTest {

  public ForeignEvalNodeTest() {
  }

  @Test
  public void sourceWithoutHash() throws Exception {
    var src = Source.newBuilder("epb", """
    nonsensecontent
    """, "simple.test").build();

    var node = ForeignEvalNode.parse(null, src, Collections.emptyList());
    try {
      var res = node.execute(null);
      fail("Unexpected result: " + res);
    } catch (ForeignParsingException e) {
      assertEquals("No # found", e.getMessage());
    }
  }
}
