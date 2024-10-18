package org.enso.runtime.parser.processor.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import com.google.testing.compile.CompilationSubject;
import com.google.testing.compile.Compiler;
import com.google.testing.compile.JavaFileObjects;
import javax.tools.FileObject;
import javax.tools.StandardLocation;
import org.enso.runtime.parser.processor.IRProcessor;
import org.junit.Test;

public class TestIRProcessor {
  @Test
  public void simpleIRNodeWithoutChildren_CompilationSucceeds() {
    var src =
        JavaFileObjects.forSourceString(
            "JName",
            """
        import org.enso.runtime.parser.dsl.IRNode;
        import org.enso.compiler.core.IR;
        @IRNode
        public interface JName extends IR {}
        """);
    var compiler = Compiler.javac().withProcessors(new IRProcessor());
    var compilation = compiler.compile(src);
    CompilationSubject.assertThat(compilation).succeeded();
  }

  @Test
  public void annotatedInterfaceMustExtendIR() {
    var src =
        JavaFileObjects.forSourceString(
            "Hello",
            """
        import org.enso.runtime.parser.dsl.IRNode;
        @IRNode
        public interface Hello {}
        """);
    var compiler = Compiler.javac().withProcessors(new IRProcessor());
    var compilation = compiler.compile(src);
    CompilationSubject.assertThat(compilation).failed();
  }

  @Test
  public void annotationCanOnlyBeAppliedToInterface() {
    var src =
        JavaFileObjects.forSourceString(
            "Hello",
            """
        import org.enso.runtime.parser.dsl.IRNode;
        @IRNode
        public class Hello {}
        """);
    var compiler = Compiler.javac().withProcessors(new IRProcessor());
    var compilation = compiler.compile(src);
    CompilationSubject.assertThat(compilation).failed();
  }

  @Test
  public void simpleIRNodeWithoutChildren_GeneratesSource() {
    var src =
        JavaFileObjects.forSourceString(
            "JName",
            """
        import org.enso.runtime.parser.dsl.IRNode;
        import org.enso.compiler.core.IR;
        @IRNode
        public interface JName extends IR {}
        """);
    var compiler = Compiler.javac().withProcessors(new IRProcessor());
    var compilation = compiler.compile(src);
    CompilationSubject.assertThat(compilation).succeeded();
    var generatedScalaClass =
        compilation.generatedFile(StandardLocation.SOURCE_OUTPUT, "JNameGen.scala");
    assertThat(generatedScalaClass.isPresent(), is(true));
    var srcContent = readSrcFile(generatedScalaClass.get());
    assertThat("Generated just one source", compilation.generatedFiles().size(), is(1));
  }

  @Test
  public void simpleIRNodeWithChild() {
    var src =
        JavaFileObjects.forSourceString(
            "MyIR",
            """
        import org.enso.runtime.parser.dsl.IRNode;
        import org.enso.runtime.parser.dsl.IRChild;
        import org.enso.compiler.core.IR;
        import org.enso.compiler.core.ir.JExpression;

        @IRNode
        public interface MyIR extends IR {
          @IRChild JExpression expression();
        }
        """);
    var compiler = Compiler.javac().withProcessors(new IRProcessor());
    var compilation = compiler.compile(src);
    CompilationSubject.assertThat(compilation).succeeded();
    CompilationSubject.assertThat(compilation)
        .generatedFile(StandardLocation.SOURCE_OUTPUT, "MyIRGen.scala")
        .isNotNull();
    var genSrc = compilation.generatedFile(StandardLocation.SOURCE_OUTPUT, "MyIRGen.scala");
    assertThat(genSrc.isPresent(), is(true));
    var srcContent = readSrcFile(genSrc.get());
    assertThat("Generated just one source", compilation.generatedSourceFiles().size(), is(1));
  }

  private static String readSrcFile(FileObject src) {
    try {
      return src.getCharContent(true).toString();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
}
