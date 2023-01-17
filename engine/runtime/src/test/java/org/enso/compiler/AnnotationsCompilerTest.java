package org.enso.compiler;

import org.enso.compiler.core.IR;
import org.enso.compiler.core.IR$Name$Annotation;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class AnnotationsCompilerTest extends CompilerTest {

  @Test
  public void testMethodAnnotations() throws Exception {
    var ir = parse("""
    @a expr
    @b (x y)
    foo a b = a b      
    """);

    var annotation1 = (IR$Name$Annotation) ir.bindings().apply(0);
    var annotation2 = (IR$Name$Annotation) ir.bindings().apply(1);
    assertEquals(annotation1.name(), "a");
    assertEquals(annotation2.name(), "b");
  }


}
