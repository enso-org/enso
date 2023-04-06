package org.enso.compiler;

import org.enso.compiler.core.IR$Error$Syntax;
import org.enso.compiler.core.IR$Error$Syntax$UnexpectedDeclarationInType$;
import org.enso.compiler.core.IR$Function$Binding;
import org.enso.compiler.core.IR$Module$Scope$Definition$Data;
import org.enso.compiler.core.IR$Module$Scope$Definition$SugaredType;
import org.enso.compiler.core.IR$Name$Annotation;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class AnnotationsCompilerTest extends CompilerTest {

  @Test
  public void testModuleMethod() throws Exception {
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

  @Test
  public void testExtensionMethod() throws Exception {
    var ir = parse("""
    @a expr
    @b (x y)
    Foo.foo a b = a b
    """);

    var annotation1 = (IR$Name$Annotation) ir.bindings().apply(0);
    var annotation2 = (IR$Name$Annotation) ir.bindings().apply(1);

    assertEquals(annotation1.name(), "a");
    assertEquals(annotation2.name(), "b");
  }

  @Test
  public void testTypeMethod() throws Exception {
    var ir = parse("""
    type Foo
        @a foo
        @b bar
        method a b = a b
    """);

    var typeDefinition = (IR$Module$Scope$Definition$SugaredType) ir.bindings().apply(0);

    var annotation1 = (IR$Name$Annotation) typeDefinition.body().apply(0);
    var annotation2 = (IR$Name$Annotation) typeDefinition.body().apply(1);
    var function = (IR$Function$Binding) typeDefinition.body().apply(2);

    assertEquals(annotation1.name(), "a");
    assertEquals(annotation2.name(), "b");
    assertEquals(function.name().name(), "method");
  }

  @Test
  public void testConstructor() throws Exception {
    var ir = parse("""
    type Foo
        @a foo
        @b bar
        Cons a b = a b
    """);

    var typeDefinition = (IR$Module$Scope$Definition$SugaredType) ir.bindings().apply(0);

    var annotation1 = (IR$Name$Annotation) typeDefinition.body().apply(0);
    var annotation2 = (IR$Name$Annotation) typeDefinition.body().apply(1);
    var constructor = (IR$Module$Scope$Definition$Data) typeDefinition.body().apply(2);

    assertEquals(annotation1.name(), "a");
    assertEquals(annotation2.name(), "b");
    assertEquals(constructor.name().name(), "Cons");
  }

  @Test
  public void testInvalidComplexType() throws Exception {
    var ir = parse("""
    type Foo
        bar a =
    """);

    var typeDefinition = (IR$Module$Scope$Definition$SugaredType) ir.bindings().apply(0);
    var methodOrError = typeDefinition.body().apply(0);

    if (methodOrError instanceof IR$Error$Syntax error) {
        assertEquals(error.reason(), IR$Error$Syntax$UnexpectedDeclarationInType$.MODULE$);
    } else {
        fail("Expecting error instead of bar function: " + methodOrError);
    }
  }
}
