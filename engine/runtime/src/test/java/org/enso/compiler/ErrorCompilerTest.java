package org.enso.compiler;

import com.oracle.truffle.api.source.Source;

import java.io.IOException;

import org.enso.compiler.core.IR;
import org.enso.compiler.core.IR$Error$Syntax;
import org.enso.compiler.core.IR$Error$Syntax$UnexpectedExpression$;
import org.enso.syntax.text.Location;

import org.junit.AfterClass;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import org.junit.BeforeClass;
import org.junit.Test;
import scala.collection.immutable.List;

public class ErrorCompilerTest {
  private static EnsoCompiler ensoCompiler;

  @BeforeClass
  public static void initEnsoCompiler() {
    ensoCompiler = new EnsoCompiler();
  }

  @AfterClass
  public static void closeEnsoCompiler() throws Exception {
    ensoCompiler.close();
  }

  @Test
  public void spaceRequired() throws Exception {
    var ir = parseTest("foo = if cond.x else.y");
    assertSingleSyntaxError(ir, IR$Error$Syntax$UnexpectedExpression$.MODULE$, "Unexpected expression.", 6, 8);
  }

  @Test
  public void incompleteTypeDefinition() throws Exception {
    var ir = parseTest("type");
    assertSingleSyntaxError(ir, IR$Error$Syntax$UnexpectedExpression$.MODULE$, "Unexpected expression.", 0, 4);
  }

  @Test
  public void badCase1() throws Exception {
    var ir = parseTest("""
    foo = case x of
     4
    """);
    assertSingleSyntaxError(ir, IR$Error$Syntax$UnexpectedExpression$.MODULE$, "Unexpected expression.", 6, 18);
  }

  @Test
  public void badCase2() throws Exception {
    var ir = parseTest("""
    foo = case x of
     4 ->
    """);
    assertSingleSyntaxError(ir, IR$Error$Syntax$UnexpectedExpression$.MODULE$, "Unexpected expression.", 6, 21);
  }

  @Test
  public void badCase3() throws Exception {
    var ir = parseTest("""
    foo = case x of
     4->
    """);
    assertSingleSyntaxError(ir, IR$Error$Syntax$UnexpectedExpression$.MODULE$, "Unexpected expression.", 6, 20);
  }

  private void assertSingleSyntaxError(
      IR.Module ir, IR$Error$Syntax$UnexpectedExpression$ type,
      String msg, int start, int end
  ) {
    var errors = assertIR(ir, IR$Error$Syntax.class, 1);
    assertEquals(type, errors.head().reason());
    assertEquals(msg, errors.head().message());
    assertEquals(new Location(start, end), errors.head().location().get().location());
  }

  private List<IR$Error$Syntax> assertIR(IR.Module ir, Class<IR$Error$Syntax> type, int count) {
    var errors = ir.preorder().filter(type::isInstance).map(type::cast);
    assertEquals("Expecting errors: " + errors, count, errors.size());
    return errors;
  }

  private static IR.Module parseTest(String code) throws IOException {
    var src =
        Source.newBuilder("enso", code, "test-" + Integer.toHexString(code.hashCode()) + ".enso")
            .build();
    var ir = ensoCompiler.compile(src);
    assertNotNull("IR was generated", ir);
    return ir;
  }
}
