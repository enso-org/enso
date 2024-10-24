package org.enso.interpreter.epb;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import com.oracle.truffle.api.source.Source;
import java.util.Collections;
import org.junit.Test;

public class ForeignEvalNodeTest {

  public ForeignEvalNodeTest() {}

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
      assertEquals("No `#` found. Expecting `lang:lineno#code` format.", e.getMessage());
    }
  }
}
