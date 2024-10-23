package org.enso.compiler.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Empty;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Location;
import org.enso.compiler.core.ir.expression.errors.Syntax;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.junit.Test;
import scala.collection.immutable.List;

public class ErrorCompilerTest extends CompilerTests {

  @Test
  public void unfinishedLiteral1() throws Exception {
    var ir = parse("""
    foo = "unfinished literal...
    """);

    assertSingleSyntaxError(
        ir, Syntax.UnclosedTextLiteral$.MODULE$, "Unclosed text literal", 6, 28);
  }

  @Test
  public void brokenAnnotationMissingArgument() throws Exception {
    var ir = parse("""
    @anno
    fn = 10
    """);

    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 5);
  }

  @Test
  public void dotUnderscore() throws Exception {
    var ir = parse("""
    run op =
      op._
    """);

    assertSingleSyntaxError(ir, Syntax.InvalidUnderscore$.MODULE$, "Invalid use of _", 14, 15);
  }

  @Test
  public void spaceDotUnderscore() throws Exception {
    var ir = parse("""
    run op =
      op ._
    """);

    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 14, 16);
  }

  @Test
  public void dotUnderscore2() throws Exception {
    var ir = parse("""
    run op =
      op._.something
    """);

    assertSingleSyntaxError(ir, Syntax.InvalidUnderscore$.MODULE$, "Invalid use of _", 14, 15);
  }

  @Test
  public void unfinishedLiteral2() throws Exception {
    var ir = parse("""
    foo = 'unfinished literal...
    """);
    assertSingleSyntaxError(
        ir, Syntax.UnclosedTextLiteral$.MODULE$, "Unclosed text literal", 6, 28);
  }

  @Test
  public void unpairedLiteral1() throws Exception {
    var ir = parse("""
    foo = "unpaired literal'
    """);
    assertSingleSyntaxError(
        ir, Syntax.UnclosedTextLiteral$.MODULE$, "Unclosed text literal", 6, 24);
  }

  @Test
  public void unpairedLiteral2() throws Exception {
    var ir = parse("""
    foo = 'unpaired literal"
    """);
    assertSingleSyntaxError(
        ir, Syntax.UnclosedTextLiteral$.MODULE$, "Unclosed text literal", 6, 24);
  }

  @Test
  public void spaceRequired() throws Exception {
    var ir = parse("foo = if cond.x else.y");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 6, 8);
  }

  @Test
  public void incompleteTypeDefinition() throws Exception {
    var ir = parse("type");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 4);
  }

  @Test
  public void lessThanTwoArgumentsToAnOperator() throws Exception {
    var ir = parse("""
    type T
       %& self = 0
    """);
    assertSingleSyntaxError(
        ir, Syntax.InvalidOperator$.MODULE$, "Operator must have two arguments", 10, 21);
  }

  @Test
  public void moreThanTwoArgumentsToAnOperator() throws Exception {
    var ir = parse("""
    type X
       &% self one two = one+two
    """);
    assertSingleSyntaxError(
        ir, Syntax.InvalidOperator$.MODULE$, "Operator must have two arguments", 10, 35);
  }

  @Test
  public void badCase1() throws Exception {
    var ir = parse("""
    foo = case x of
     4
    """);
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 6, 18);
  }

  @Test
  public void badCase2() throws Exception {
    var ir = parse("""
    foo = case x of
     4 ->
    """);
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 6, 21);
  }

  @Test
  public void badCase3() throws Exception {
    var ir = parse("""
    foo = case x of
     4->
    """);
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 6, 20);
  }

  @Test
  public void badCase4() throws Exception {
    var ir = parse("""
    main =
        case value of
        -1 ->"minus one"
    """);
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 32, 45);
  }

  @Test
  public void malformedSequence1() throws Exception {
    var ir = parse("(1, )");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 5);
  }

  @Test
  public void malformedSequence2() throws Exception {
    var ir = parse("foo = (1, )");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 7, 9);
  }

  @Test
  public void unmatchedDemiliter1() throws Exception {
    var ir = parse("(");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 1);
  }

  @Test
  public void unmatchedDemiliter2() throws Exception {
    var ir = parse(")");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 1);
  }

  @Test
  public void unmatchedDemiliter3() throws Exception {
    var ir = parse("[");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 1);
  }

  @Test
  public void unmatchedDemiliter4() throws Exception {
    var ir = parse("[");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 1);
  }

  @Test
  public void unmatchedDemiliter5() throws Exception {
    var ir = parse("foo = (");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 6, 7);
  }

  @Test
  public void unmatchedDemiliter6() throws Exception {
    var ir = parse("foo = )");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 6, 7);
  }

  @Test
  public void unmatchedDemiliter7() throws Exception {
    var ir = parse("foo = [");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 6, 7);
  }

  @Test
  public void unmatchedDemiliter8() throws Exception {
    var ir = parse("foo = ]");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 6, 7);
  }

  @Test
  public void unexpectedSpecialOperator() throws Exception {
    var ir = parse("foo = 1, 2");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 6, 10);
  }

  @Test
  public void malformedImport1() throws Exception {
    var ir = parse("import");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 6);
  }

  @Test
  public void malformedImport2() throws Exception {
    var ir = parse("import as Foo");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 13);
  }

  private Syntax.InvalidImport invalidImport(String msg) {
    return new Syntax.InvalidImport(msg);
  }

  private Syntax.InvalidExport invalidExport(String msg) {
    return new Syntax.InvalidExport(msg);
  }

  @Test
  public void malformedImport3() throws Exception {
    var ir = parse("import Foo as Foo, Bar");
    assertSingleSyntaxError(ir, invalidImport("Expected identifier."), null, 14, 22);
  }

  @Test
  public void malformedImport4() throws Exception {
    var ir = parse("import Foo as Foo.Bar");
    assertSingleSyntaxError(ir, invalidImport("Expected identifier."), null, 14, 21);
  }

  @Test
  public void malformedImport5() throws Exception {
    var ir = parse("import Foo as");
    assertSingleSyntaxError(ir, invalidImport("Expected tokens."), null, 13, 13);
  }

  @Test
  public void malformedImport6() throws Exception {
    var ir = parse("import Foo as Bar.Baz");
    assertSingleSyntaxError(ir, invalidImport("Expected identifier."), null, 14, 21);
  }

  @Test
  public void malformedImport7() throws Exception {
    var ir = parse("import Foo hiding");
    assertSingleSyntaxError(ir, invalidImport("Expected qualified name."), null, 7, 17);
  }

  @Test
  public void malformedImport8() throws Exception {
    var ir = parse("import Foo hiding X,");
    assertSingleSyntaxError(ir, invalidImport("Malformed comma-delimited sequence."), null, 7, 20);
  }

  @Test
  public void malformedImport9() throws Exception {
    var ir = parse("polyglot import Foo");
    assertSingleSyntaxError(ir, Syntax.UnrecognizedToken$.MODULE$, "Unrecognized token", 0, 19);
  }

  @Test
  public void malformedImport10() throws Exception {
    var ir = parse("polyglot java import");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 20);
  }

  @Test
  public void malformedTypeException() throws Exception {
    var ir =
        parse(
            """
    fan_out_to_columns : Table -> Text | Integer -> (Any -> Vector Any) -> | Nothing -> Problem_Behavior -> Table | Nothing
    fan_out_to_columns table text_or_integer any_to_vector_any wat problem_behavior = Nothing
    """);
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 48, 119);
  }

  @Test
  public void malformedImport11() throws Exception {
    var ir = parse("from import all");
    assertSingleSyntaxError(ir, invalidImport("Expected tokens."), null, 4, 4);
  }

  @Test
  public void malformedImport12() throws Exception {
    var ir = parse("from Foo import all hiding");
    assertSingleSyntaxError(ir, invalidImport("Expected tokens."), null, 26, 26);
  }

  @Test
  public void malformedImport13() throws Exception {
    var ir = parse("from Foo import all hiding X.Y");
    assertSingleSyntaxError(ir, invalidImport("Expected identifier."), null, 27, 30);
  }

  @Test
  public void malformedImport14() throws Exception {
    var ir = parse("from Foo import Some.Nested.Module.Path");
    assertSingleSyntaxError(ir, invalidImport("Expected identifier."), null, 16, 39);
  }

  @Test
  public void malformedExport1() throws Exception {
    var ir = parse("export");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 6);
  }

  @Test
  public void malformedExport2() throws Exception {
    var ir = parse("export as Foo");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 13);
  }

  @Test
  public void malformedExport3() throws Exception {
    var ir = parse("export Foo as Foo, Bar");
    assertSingleSyntaxError(ir, invalidExport("Expected identifier."), null, 14, 22);
  }

  @Test
  public void malformedExport4() throws Exception {
    var ir = parse("export Foo as Foo.Bar");
    assertSingleSyntaxError(ir, invalidExport("Expected identifier."), null, 14, 21);
  }

  @Test
  public void malformedExport5() throws Exception {
    var ir = parse("export Foo as");
    assertSingleSyntaxError(ir, invalidExport("Expected tokens."), null, 13, 13);
  }

  @Test
  public void malformedExport6() throws Exception {
    var ir = parse("export Foo as Bar.Baz");
    assertSingleSyntaxError(ir, invalidExport("Expected identifier."), null, 14, 21);
  }

  @Test
  public void malformedExport7() throws Exception {
    var ir = parse("export Foo hiding");
    assertSingleSyntaxError(ir, invalidExport("Expected qualified name."), null, 7, 17);
  }

  @Test
  public void malformedExport8() throws Exception {
    var ir = parse("export Foo hiding X,");
    assertSingleSyntaxError(ir, invalidExport("Malformed comma-delimited sequence."), null, 7, 20);
  }

  @Test
  public void malformedExport9() throws Exception {
    var ir = parse("from export all");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 15);
  }

  @Test
  public void malformedExport10() throws Exception {
    var ir = parse("from Foo export all hiding");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 26);
  }

  @Test
  public void malformedExport11() throws Exception {
    var ir = parse("from Foo export all hiding X.Y");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 30);
  }

  @Test
  public void invalidToken1() throws Exception {
    var ir = parse("`");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 1);
  }

  @Test
  public void invalidToken2() throws Exception {
    var ir = parse("splice_outside_text = `");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 22, 23);
  }

  @Test
  public void illegalForeignBody1() throws Exception {
    var ir = parse("foreign 4");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 9);
  }

  @Test
  public void illegalForeignBody2() throws Exception {
    var ir = parse("foreign 4 * 4");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 13);
  }

  @Test
  public void illegalForeignBody3() throws Exception {
    var ir = parse("foreign foo = \"4\"");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 17);
  }

  @Test
  public void illegalForeignBody4() throws Exception {
    var ir = parse("foreign js foo = 4");
    assertSingleSyntaxError(
        ir, new Syntax.InvalidForeignDefinition("Expected text literal as body"), null, 0, 18);
  }

  @Test
  public void illegalPrivateVariableDeclaration() throws Exception {
    var ir = parseBlock("private var = 42");
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 7);
  }

  @Test
  public void illegalPrivateKeywordUseInType() throws Exception {
    var ir = parse("""
        type T
            private
        """);
    assertSingleSyntaxError(
        ir,
        Syntax.UnexpectedDeclarationInType$.MODULE$,
        "Unexpected declaration in the body of a type",
        11,
        18);
  }

  @Test
  public void illegalPrivateKeywordRepeatedDeclarations() throws Exception {
    var ir = parse("""
        private
        private
        """);
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 8, 15);
  }

  @Test
  public void illegalPrivateKeywordUseInMethodBody() throws Exception {
    var ir = parse("""
        method =
            private priv_nested_method x = x
        """);
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 13, 20);
  }

  @Test
  public void illegalPrivateTypeDeclaration() throws Exception {
    var ir = parse("""
        private type T
        """);
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 14);
  }

  @Test
  public void illegalEscapeSequence() throws Exception {
    var ir = parse("""
    escape = 'wrong \\c sequence'
    """);
    assertSingleSyntaxError(
        ir,
        new Syntax.InvalidEscapeSequence("wrong  sequence"),
        "Invalid escape sequence wrong  sequence",
        9,
        28);
  }

  @Test
  public void testNPE183814303() throws Exception {
    var ir =
        parse(
            """
    from Standard.Base import all

    main =
        x = "foo"
        z = x. length
        IO.println z
    """);
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 60, 62);
  }

  @Test
  public void testNPE183863754() throws Exception {
    var ir = parse("""
    main =
    #    meh
         42
    """);
    var errors = ir.preorder().filter(Syntax.class::isInstance).map(Syntax.class::cast);
    assertEquals("One error", 1, errors.size());
    assertEquals(Syntax.UnexpectedExpression$.MODULE$, errors.head().reason());
    assertEquals(7, errors.head().location().get().start());
    assertEquals(16, errors.head().location().get().length());
  }

  @Test
  public void testMissingEqualsInMethodDefinition() throws Exception {
    var ir = parse("""
    type T
      method self
        42
    """);
    assertSingleSyntaxError(
        ir,
        Syntax.UnexpectedDeclarationInType$.MODULE$,
        "Unexpected declaration in the body of a type",
        9,
        27);
  }

  @Test
  public void testAnnotation1() throws Exception {
    var ir = parse("""
    @x `
    id x = x
    """);
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 3, 4);
  }

  @Test
  public void testAnnotation2() throws Exception {
    var ir = parse("""
    @` foo
    id x = x
    """);
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 6);
  }

  @Test
  public void testEmptyBody() throws Exception {
    var ir = parse("""
    main =
    """);

    var method = (Method) ir.bindings().apply(0);
    assertTrue(method.body() instanceof Empty);
  }

  @Test
  public void testBodyWithComment() throws Exception {
    var ir = parse("""
    main =
        # comment
    """);

    var method = (Method) ir.bindings().apply(0);
    var body = (Expression.Block) method.body();
    assertTrue(body.expressions().isEmpty());
    assertTrue(body.returnValue() instanceof Empty);
  }

  @Test
  public void exportAllIsNotAllowed() {
    var ir = parse("""
        from project.Module export all
        """);
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 30);
  }

  @Test
  public void exportHidingIsNotAllowed() {
    var ir = parse("""
        from project.Module export all hiding Foo
        """);
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 0, 41);
  }

  @Test
  public void inlineDocCommentIsNotAllowed_1() {
    var ir = parse("""
        main args =
            v = 42 ## meh
            v
        """);
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 23, 29);
  }

  @Test
  public void inlineDocCommentIsNotAllowed_2() {
    var ir = parse("""
        main args =
            v = 42
            v ## meh
        """);
    assertSingleSyntaxError(
        ir, Syntax.UnexpectedExpression$.MODULE$, "Unexpected expression", 29, 35);
  }

  private void assertSingleSyntaxError(IR ir, Syntax.Reason type, String msg, int start, int end) {
    var errors = assertIR(ir, Syntax.class, 1);
    assertEquals(type, errors.head().reason());
    if (msg != null) {
      assertEquals(msg, errors.head().message(null));
    }
    assertEquals(new Location(start, end), errors.head().location().get().location());
  }

  private List<Syntax> assertIR(IR ir, Class<Syntax> type, int count) {
    var errors = ir.preorder().filter(type::isInstance).map(type::cast);
    if (ir.diagnostics() != null)
      errors =
          errors.prependedAll(ir.diagnostics().toList().filter(type::isInstance).map(type::cast));
    assertEquals("Expecting errors: " + errors, count, errors.size());
    return errors;
  }
}
