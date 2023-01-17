package org.enso.compiler;

import com.oracle.truffle.api.source.Source;
import org.enso.compiler.core.IR;
import org.junit.AfterClass;
import org.junit.BeforeClass;

import java.io.IOException;

import static org.junit.Assert.assertNotNull;

public abstract class CompilerTest {

  protected static EnsoCompiler ensoCompiler;

  @BeforeClass
  public static void initEnsoCompiler() {
    ensoCompiler = new EnsoCompiler();
  }

  @AfterClass
  public static void closeEnsoCompiler() throws Exception {
    ensoCompiler.close();
  }

  protected static IR.Module parse(String code) throws IOException {
    var src =
        Source.newBuilder("enso", code, "test-" + Integer.toHexString(code.hashCode()) + ".enso")
            .build();
    IR.Module ir = ensoCompiler.compile(src);
    assertNotNull("IR was generated", ir);
    return ir;
  }
}
