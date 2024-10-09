package org.enso.runtime.parser.processor.test;

import com.google.testing.compile.CompilationSubject;
import com.google.testing.compile.Compiler;
import com.google.testing.compile.JavaFileObjects;
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
    CompilationSubject.assertThat(compilation).generatedSourceFile("JNameGen").isNotNull();
  }
}
