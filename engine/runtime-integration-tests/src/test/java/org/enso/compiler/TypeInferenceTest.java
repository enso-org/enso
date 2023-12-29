package org.enso.compiler;

import org.enso.compiler.context.FreshNameSupply;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.ProcessingPass;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.data.CompilerConfig;
import org.enso.compiler.pass.PassConfiguration;
import org.enso.compiler.pass.PassManager;
import org.enso.compiler.pass.analyse.types.InferredType;
import org.enso.compiler.pass.analyse.types.TypeInference;
import org.enso.compiler.pass.analyse.types.TypeRepresentation;
import org.enso.compiler.test.CompilerRunner;
import org.enso.pkg.QualifiedName;
import org.graalvm.polyglot.Source;
import org.junit.Ignore;
import org.junit.Test;
import scala.Option;
import scala.collection.immutable.Seq;
import scala.collection.immutable.Seq$;

import java.net.URI;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class TypeInferenceTest extends CompilerTest {
  @Ignore
  @Test
  public void zeroAryCheck() throws Exception {
    final URI uri = new URI("memory://zeroAryCheck.enso");
    final Source src =
        Source.newBuilder("enso", """
                type My_Type
                    Value x
                    
                const -> My_Type = My_Type.Value 42
                    
                foo =
                    x = const
                    y = My_Type.Value 23
                    _ = y
                    x
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    Module module = compile(src);
    Method foo = findStaticMethod(module, "foo");
    var x = findAssignment(foo.body(), "x");
    TypeRepresentation xType = getInferredType(x.expression());
    TypeRepresentation.AtomType asAtom = (TypeRepresentation.AtomType) xType;
    assertTrue("The type of `x` should be `My_Type`.", asAtom.fqn().contains("My_Type"));
  }

  @Ignore("TODO resolution of local function application")
  @Test
  public void functionReturnCheck() throws Exception {
    final URI uri = new URI("memory://functionReturnCheck.enso");
    final Source src =
        Source.newBuilder("enso", """
                type My_Type
                    Value x
                    
                add x y -> My_Type = My_Type.Value (x.x+y.x)
                    
                foo z =
                    a = My_Type.Value 42
                    b = add a z
                    b
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = findStaticMethod(module, "foo");
    var b = findAssignment(foo.body(), "b");
    TypeRepresentation myType = new TypeRepresentation.AtomType("functionReturnCheck.My_Type");

    // The result of `add a z` should be `My_Type` as guaranteed by the return type check of `add`.
    assertEquals(myType, getInferredType(b.expression()));
  }

  @Ignore("TODO resolving local bindings/arguments")
  @Test
  public void argChecks() throws Exception {
    final URI uri = new URI("memory://argChecks.enso");
    final Source src =
        Source.newBuilder("enso", """
                type My_Type
                    Value v
                    
                f1 (x1 : My_Type) =
                  y1 = x1
                  My_Type.Value (y2.v + y2.v)
                                
                f2 : My_Type -> My_Type
                f2 x2 =
                  y2 = x2
                  My_Type.Value (y2.v + y2.v)
                                
                f3 (x3 : My_Type) -> My_Type = My_Type.Value (x3.v + x3.v)
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);

    var myType = new TypeRepresentation.AtomType("argChecks.My_Type");
    assertEquals(myType, getInferredType(findAssignment(findStaticMethod(module, "f1"), "y1").expression()));
    assertNoInferredType(findAssignment(findStaticMethod(module, "f2"), "y2").expression());

    // TODO also checks for types of whole functions
  }

  @Test
  public void ascribedExpressions() throws Exception {
    final URI uri = new URI("memory://ascribedExpressions.enso");
    final Source src =
        Source.newBuilder("enso", """
                type My_Type
                    Value x
                    
                f x =
                    y = (x : My_Type)
                    My_Type.Value (y.x + y.x)
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    Module module = compile(src);
    Method f = findStaticMethod(module, "f");

    TypeRepresentation myType = new TypeRepresentation.AtomType("ascribedExpressions.My_Type");
    TypeRepresentation yType = getInferredType(findAssignment(f.body(), "y").expression());
    assertEquals(myType, yType);
  }


  @Test
  public void advancedAscribedExpressions() throws Exception {
    final URI uri = new URI("memory://advancedAscribedExpressions.enso");
    final Source src =
        Source.newBuilder("enso", """
                type My_Type
                    Value x
                type Other_Type
                    Value y
                f z =
                    y1 = (z : My_Type | Other_Type)
                    y2 = (z : My_Type & Other_Type)
                    My_Type.Value (y1.x + y2.x)
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    Module module = compile(src);
    Method f = findStaticMethod(module, "f");

    TypeRepresentation myType = new TypeRepresentation.AtomType("advancedAscribedExpressions.My_Type");
    TypeRepresentation otherType = new TypeRepresentation.AtomType("advancedAscribedExpressions.Other_Type");
    TypeRepresentation sum = new TypeRepresentation.SumType(List.of(myType, otherType));
    assertEquals(sum, getInferredType(findAssignment(f.body(), "y1").expression()));

    TypeRepresentation intersection = new TypeRepresentation.IntersectionType(List.of(myType, otherType));
    assertEquals(intersection, getInferredType(findAssignment(f.body(), "y2").expression()));
  }

  @Test
  public void ascribedFunctionType() throws Exception {
    final URI uri = new URI("memory://ascribedFunctionType.enso");
    final Source src =
        Source.newBuilder("enso", """
                type My_Type
                    Value x
                type Other_Type
                    Value y
                f z w =
                    f1 = (z : My_Type -> Other_Type)
                    f2 = (w : My_Type -> My_Type -> Other_Type)
                    f2 (f1 (My_Type.Value 42))
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    Module module = compile(src);
    Method f = findStaticMethod(module, "f");

    // Here we will only know that both f1 and f2 are Any -> Any - because the ascribed check only really performs a
    // `is_a Function` check, we do not know anything about the argument nor return type of this function,
    // unfortunately.
    TypeRepresentation primitiveFunctionType = new TypeRepresentation.ArrowType(TypeRepresentation.ANY, TypeRepresentation.ANY);
    assertEquals(primitiveFunctionType, getInferredType(findAssignment(f.body(), "f1").expression()));
    assertEquals(primitiveFunctionType, getInferredType(findAssignment(f.body(), "f2").expression()));
  }

  @Test
  public void literals() throws Exception {
    final URI uri = new URI("memory://literals.enso");
    final Source src =
        Source.newBuilder("enso", """
                f =
                    x = 42
                    y = "foo"
                    z = 1.5
                    x.to_text + y + z.to_text
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    Module module = compile(src);
    Method f = findStaticMethod(module, "f");

    assertEquals(TypeRepresentation.INTEGER, getInferredType(findAssignment(f.body(), "x").expression()));
    assertEquals(TypeRepresentation.TEXT, getInferredType(findAssignment(f.body(), "y").expression()));
    assertEquals(TypeRepresentation.FLOAT, getInferredType(findAssignment(f.body(), "z").expression()));
  }

  @Ignore("TODO resolving local bindings/arguments")
  @Test
  public void bindingsFlow() throws Exception {
    final URI uri = new URI("memory://bindingsFlow.enso");
    final Source src =
        Source.newBuilder("enso", """
                type My_Type
                    Value v
                foo (x1 : My_Type) x2 x3 =
                    y2 = (x2 : My_Type)
                    
                    z1 = x1
                    z2 = y2
                    
                    w1 = z1
                    w2 = z2
                    
                    w3 = x3
                    [w1, w2, w3]
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = findStaticMethod(module, "foo");

    var myType = new TypeRepresentation.AtomType("bindingsFlow.My_Type");

    assertEquals(myType, getInferredType(findAssignment(foo, "w1").expression()));
    assertEquals(myType, getInferredType(findAssignment(foo, "w2").expression()));

    assertNoInferredType(findAssignment(foo, "w3").expression());
  }

  @Ignore("TODO ctors 0")
  @Test
  public void zeroArgConstructor() throws Exception {
    final URI uri = new URI("memory://zeroArgConstructor.enso");
    final Source src =
        Source.newBuilder("enso", """
                type My_Type
                    Singleton
                foo =
                    x = My_Type.Singleton
                    x
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = findStaticMethod(module, "foo");

    var myType = new TypeRepresentation.AtomType("zeroArgConstructor.My_Type");
    assertEquals(myType, getInferredType(findAssignment(foo, "x").expression()));
  }

  @Ignore("TODO")
  @Test
  public void multiArgConstructor() throws Exception {
    final URI uri = new URI("memory://multiArgConstructor.enso");
    final Source src =
        Source.newBuilder("enso", """
                type My_Type
                    Value x y z
                foo =
                    x = My_Type.Value 1 2 3
                    x
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var foo = findStaticMethod(module, "foo");

    var myType = new TypeRepresentation.AtomType("multiArgConstructor.My_Type");
    assertEquals(myType, getInferredType(findAssignment(foo, "x").expression()));
  }

  @Ignore("TODO: ifte")
  @Test
  public void commonIfThenElse() throws Exception {
    final URI uri = new URI("memory://commonIfThenElse.enso");
    final Source src =
        Source.newBuilder("enso", """
                f x =
                  y = if x == 10 then 1 else 2
                  y
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var f = findStaticMethod(module, "f");
    assertEquals(TypeRepresentation.INTEGER, getInferredType(findAssignment(f, "y").expression()));
  }

  @Ignore
  @Test
  public void commonCase() throws Exception {
    final URI uri = new URI("memory://commonCase.enso");
    final Source src =
        Source.newBuilder("enso", """
                f x =
                  y = case x of
                    i : Integer -> i
                    _ -> 0
                  y
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var f = findStaticMethod(module, "f");
    assertEquals(TypeRepresentation.INTEGER, getInferredType(findAssignment(f, "y").expression()));
  }

  @Ignore
  @Test
  public void inferBoundsFromCase() throws Exception {
    final URI uri = new URI("memory://inferBoundsFromCase.enso");
    final Source src =
        Source.newBuilder("enso", """
                f x =
                  y = case x of
                    _ : Integer -> x
                    _ -> 0
                  y
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var f = findStaticMethod(module, "f");
    assertEquals(TypeRepresentation.INTEGER, getInferredType(findAssignment(f, "y").expression()));
  }

  @Ignore
  @Test
  public void sumTypeFromIf() throws Exception {
    final URI uri = new URI("memory://sumTypeFromIf.enso");
    final Source src =
        Source.newBuilder("enso", """
                f x =
                  y = if x == 1 then "foo" else 42
                  y
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var f = findStaticMethod(module, "f");
    var expectedType = new TypeRepresentation.SumType(List.of(TypeRepresentation.TEXT, TypeRepresentation.INTEGER));
    assertEquals(expectedType, getInferredType(findAssignment(f, "y").expression()));
  }

  @Ignore
  @Test
  public void sumTypeFromIfWithoutElse() throws Exception {
    final URI uri = new URI("memory://sumTypeFromIf.enso");
    final Source src =
        Source.newBuilder("enso", """
                f x =
                  y = if x == 1 then "foo"
                  y
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var f = findStaticMethod(module, "f");
    var expectedType = new TypeRepresentation.SumType(List.of(TypeRepresentation.TEXT, TypeRepresentation.NOTHING));
    assertEquals(expectedType, getInferredType(findAssignment(f, "y").expression()));
  }

  private Method findStaticMethod(Module module, String name) {
    var option = module.bindings().find(
        (def) ->
            (def instanceof Method binding)
                && binding.methodReference().typePointer().isEmpty()
                && binding.methodReference().methodName().name().equals(name)
    );

    assertTrue("The method " + name + " should exist within the IR.", option.isDefined());
    return (Method) option.get();
  }

  private Expression.Binding findAssignment(IR ir, String name) {
    var option = ir.preorder().find(
        (node) ->
            (node instanceof Expression.Binding binding)
                && binding.name().name().equals(name)
    );
    assertTrue("The binding `" + name + " = ...` should exist within the IR.", option.isDefined());
    return (Expression.Binding) option.get();
  }

  private TypeRepresentation getInferredType(IR ir) {
    Option<ProcessingPass.Metadata> metadata = ir.passData().get(TypeInference.INSTANCE);
    assertTrue("Expecting " + ir.showCode() + " to contain an inferred type within metadata.", metadata.isDefined());
    InferredType inferred = (InferredType) metadata.get();
    return inferred.type();
  }

  private void assertNoInferredType(IR ir) {
    Option<ProcessingPass.Metadata> metadata = ir.passData().get(TypeInference.INSTANCE);
    assertTrue("Expecting " + ir.showCode() + " to contain no inferred type within metadata.", metadata.isEmpty());
  }

  private Module compile(Source src) {
    System.out.println("\n\n\n=========================================\nSOURCE " + src.getURI().toString() + "\n");
    Module rawModule = parse(src.getCharacters());

    var compilerConfig = new CompilerConfig(false, true, true, true, true, Option.empty());
    var passes = new Passes(compilerConfig, Option.empty());
    @SuppressWarnings("unchecked") var passConfig = new PassConfiguration((Seq<PassConfiguration.ConfigPair<?>>) Seq$.MODULE$.empty());
    PassManager passManager = new PassManager(passes.passOrdering(), passConfig);
    var compilerRunner = new CompilerRunner() {
      @Override
      public CompilerConfig defaultConfig() {
        return compilerConfig;
      }

      @Override
      public void org$enso$compiler$test$CompilerRunner$_setter_$defaultConfig_$eq(CompilerConfig x$1) {
      }
    };
    var moduleName = QualifiedName.simpleName(src.getName().replace(".enso", ""));
    ModuleContext moduleContext = compilerRunner.buildModuleContext(moduleName, Option.apply(new FreshNameSupply()), Option.empty(), compilerConfig, false);
    Module processedModule = passManager.runPassesOnModule(rawModule, moduleContext);
    return processedModule;
  }
}
