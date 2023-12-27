package org.enso.compiler;

import org.enso.interpreter.test.TestBase;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
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

  private Value compile(Source src) {
    Value module = ctx.eval(src);
    // This ensures that the compiler actually is run.
    module.invokeMember("get_associated_type");
    return module;
  }
}
