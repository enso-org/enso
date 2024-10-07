package org.enso.runtime.parser.processor.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

import com.google.testing.compile.Compilation;
import com.google.testing.compile.Compiler;
import com.google.testing.compile.JavaFileObjects;
import org.enso.runtime.parser.processor.IRProcessor;
import org.junit.Assert;
import org.junit.Test;

public class TestIRProcessor {
  @Test
  public void testIRProcessorFailsWithUnimplemented() {
    var src =
        JavaFileObjects.forSourceLines(
            "HelloWorld",
            "import org.enso.runtime.parser.dsl.IRNode;",
            "@IRNode(name = \"HelloWorld\")",
            "final class HelloWorld {",
            "}");
    var compiler = Compiler.javac().withProcessors(new IRProcessor());
    Compilation compilation = null;
    try {
      compilation = compiler.compile(src);
      Assert.fail("Expected compilation to fail with UnimplementedException");
    } catch (Exception e) {
      // nop
      assertThat(compilation, is(nullValue()));
    }
  }
}
