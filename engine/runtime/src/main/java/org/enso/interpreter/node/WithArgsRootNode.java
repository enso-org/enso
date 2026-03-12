package org.enso.interpreter.node;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;

final class WithArgsRootNode extends RootNode {

  private final List<String> argNames;
  private final Source src;
  private final String name;

  public WithArgsRootNode(
      TruffleLanguage<?> language, List<String> argNames, Source src, String name) {
    super(language);
    this.argNames = argNames;
    this.src = src;
    this.name = name;
  }

  @CompilerDirectives.CompilationFinal private Function fn;
  @CompilerDirectives.CompilationFinal private Type self;
  @CompilerDirectives.CompilationFinal private Text code;
  @Child private InvokeFunctionNode invokeNode;

  @Override
  public Object execute(VirtualFrame frame) {
    var ctx = EnsoContext.get(this);
    if (fn == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      var originalSrc = src.getCharacters().toString();

      Predicate<String> isImportStatement =
          (line) -> line.startsWith("import ") || line.startsWith("from ") || line.trim().isEmpty();

      var imports =
          originalSrc.lines().takeWhile(isImportStatement).collect(Collectors.joining("\n"));

      var executableCode =
          originalSrc.lines().dropWhile(isImportStatement).collect(Collectors.joining("\n"));

      var lambdaArgs = argNames.stream().collect(Collectors.joining("-> "));
      var lambdaCode =
          """
          import Standard.Base
          ${imports}

          lambda code =
              ${args}->
                  Standard.Base.Runtime.Debug.eval code
          """
              .replace("${imports}", imports)
              .replace("${args}", lambdaArgs);
      var lambda = Source.newBuilder(src).content(lambdaCode).build();
      var module = ProgramRootNode.createModule(ctx, name, lambda);
      var moduleScope = module.compileScope(ctx);
      self = moduleScope.getAssociatedType();
      var lambdaFn = moduleScope.getMethodForType(self, "lambda");
      fn = lambdaFn;
      code = Text.create(executableCode);
      invokeNode = InvokeFunctionNode.buildWithArity(argNames.size() + 2);
    }
    var realArgs = frame.getArguments();
    var args = new Object[realArgs.length + 2];
    args[0] = self;
    args[1] = code;
    System.arraycopy(realArgs, 0, args, 2, realArgs.length);
    var state = ctx.currentState();
    var res = invokeNode.execute(fn, frame, state, args);
    return res;
  }

  @Override
  @CompilerDirectives.TruffleBoundary
  public SourceSection getSourceSection() {
    return src.createSection(0, src.getLength());
  }
}
