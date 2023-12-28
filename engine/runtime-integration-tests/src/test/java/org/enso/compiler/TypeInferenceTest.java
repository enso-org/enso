package org.enso.compiler;

import org.enso.interpreter.test.TestBase;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import java.io.OutputStream;
import java.net.URI;

public class TypeInferenceTest extends TestBase {
  private static Context ctx;

  @BeforeClass
  public static void prepareCtx() {
    ctx =
        defaultContextBuilder()
            .option(RuntimeOptions.ENABLE_TYPE_CHECK, "true")
//            .out(OutputStream.nullOutputStream())
//            .err(OutputStream.nullOutputStream())
            .build();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
  }

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
                    x
                    
                bar =
                    y = foo
                    y
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
    System.out.println(module);
    // TODO checking what is inferred - probably need instrumentation?
  }

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
    // TODO checking what is inferred
  }

  @Test
  public void argChecks() throws Exception {
    final URI uri = new URI("memory://argChecks.enso");
    final Source src =
        Source.newBuilder("enso", """
                type My_Type
                    Value v
                    
                f1 (x1 : My_Type) = My_Type.Value (x1.v + x1.v)
                
                f2 : My_Type -> My_Type
                f2 x2 = My_Type.Value (x2.v + x2.v)
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
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

    var module = compile(src);
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

    var module = compile(src);
  }

  @Test
  public void commonIfThenElse() throws Exception {
    final URI uri = new URI("memory://commonIfThenElse.enso");
    final Source src =
        Source.newBuilder("enso", """
                f x = if x == 10 then 1 else 2
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
  }


  @Test
  public void commonCase() throws Exception {
    final URI uri = new URI("memory://commonCase.enso");
    final Source src =
        Source.newBuilder("enso", """
                f x = case x of
                    i : Integer -> i
                    _ -> 0
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
  }

  @Ignore
  @Test
  public void inferBoundsFromCase() throws Exception {
    final URI uri = new URI("memory://inferBoundsFromCase.enso");
    final Source src =
        Source.newBuilder("enso", """
                f x = case x of
                    _ : Integer -> x
                    _ -> 0
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
  }

  @Ignore
  @Test
  public void sumTypeFromIf() throws Exception {
    final URI uri = new URI("memory://sumTypeFromIf.enso");
    final Source src =
        Source.newBuilder("enso", """
                f x = if x == 1 then "foo" else 42
                """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = compile(src);
  }

  private Value compile(Source src) {
    System.out.println("\n\n\n=========================================\nSOURCE " + src.getURI().toString() + "\n");
    Value module = ctx.eval(src);
    // This ensures that the compiler actually is run.
    module.invokeMember("get_associated_type");
    return module;
  }
}
