package org.enso.compiler;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.net.URISyntaxException;

import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.ProcessingPass;
import org.enso.compiler.pass.analyse.types.InferredType;
import org.enso.compiler.pass.analyse.types.TypeInferenceSignatures;
import org.enso.compiler.pass.analyse.types.TypeRepresentation;
import org.graalvm.polyglot.Source;
import org.junit.Test;
import scala.Option;

public class TypesFromSignaturesTest extends CompilerTest {

  @Test
  public void simpleCheck() throws Exception {
    final URI uri = new URI("memory://simpleCheck.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                    type A
                    type B
                    type C
                        Value v

                    f (x : A) (y : B) -> C = C.Value [x, y]
                    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    var f1 = findStaticMethod(module, "f");
    assertInferredType(f1, "(A -> (B -> C))");
  }


  @Test
  public void justArity() throws Exception {
    final URI uri = new URI("memory://justArity.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                    type A
                    type B
                    type C
                        Value v

                    f0 = 0

                    f4 x y z w = [x, y, z, w]

                    f2 : A -> B -> C
                    f2 x y = [x, y]
                    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);

    var f0 = findStaticMethod(module, "f0");
    var f4 = findStaticMethod(module, "f4");
    var f2 = findStaticMethod(module, "f2");

    // For 0 arguments and unknown return type we know nothing useful, so no information is registered.
    assertNoInferredType(f0);

    // For a function without ascriptions, we can at least infer the _arity_
    // Currently that is denoted by replacing unknowns with Any. Later this may be free type variables.
    assertInferredType(f4, "(Any -> (Any -> (Any -> (Any -> Any))))");


    // For the 'opted-out' ascription, the types are ignored, because they are not checked types. But we still infer arity.
    assertInferredType(f2, "(Any -> (Any -> Any))");
  }

  @Test
  public void memberMethods() throws URISyntaxException {
    final URI uri = new URI("memory://memberMethods.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                    type A
                        static_method (x : A) -> A = x
                        member_method self (x : A) -> A = x
                    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    module.bindings().map((b) -> {
      System.out.println(b);
      return null;
    });
  }

  @Test
  public void extensionMethods() throws URISyntaxException {
    final URI uri = new URI("memory://extensionMethods.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                    type A
                    
                    A.extension_static_method (x : A) -> A = x
                    A.extension_member_method self (x : A) -> A = x
                    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    module.bindings().map((b) -> {
      System.out.println(b);
      return null;
    });
  }

  private void assertInferredType(IR ir, String expected) {
    TypeRepresentation inferred = getInferredType(ir);
    assertEquals(expected, inferred.toString());
  }

  private TypeRepresentation getInferredType(IR ir) {
    Option<ProcessingPass.Metadata> metadata = ir.passData().get(TypeInferenceSignatures.INSTANCE);
    assertTrue(
        "Expecting " + ir.showCode() + " to contain a type within metadata.", metadata.isDefined());
    InferredType inferred = (InferredType) metadata.get();
    return inferred.type();
  }

  private void assertNoInferredType(IR ir) {
    Option<ProcessingPass.Metadata> metadata = ir.passData().get(TypeInferenceSignatures.INSTANCE);
    assertTrue(
        "Expecting " + ir.showCode() + " to contain no type within metadata.", metadata.isEmpty());
  }
}
