package org.enso.interpreter.node;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import java.util.List;
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
      final String lambdaCode =
          """
          import Standard.Base.Runtime.Debug
          import Standard.Base

          lambda code =
              ${args}->
                  Debug.eval code
          """
              .replace("${args}", argNames.stream().collect(Collectors.joining("-> ")));
      var lambda = Source.newBuilder(src).content(lambdaCode).build();
      var module = ProgramRootNode.createModule(ctx, name, lambda);
      var moduleScope = module.compileScope(ctx);
      self = moduleScope.getAssociatedType();
      var lambdaFn = moduleScope.getMethodForType(self, "lambda");
      fn = lambdaFn;
      code = Text.create(src.getCharacters().toString());
      invokeNode = InvokeFunctionNode.buildWithArity(argNames.size() + 2);
    }
    java.lang.Object[] args = new Object[frame.getArguments().length + 1];
    args[0] = self;
    args[1] = code;
    System.arraycopy(frame.getArguments(), 1, args, 2, args.length - 2);
    org.enso.interpreter.runtime.state.State state = ctx.currentState();
    java.lang.Object res = invokeNode.execute(fn, frame, state, args);
    return res;
  }

  @Override
  @CompilerDirectives.TruffleBoundary
  public SourceSection getSourceSection() {
    return src.createSection(0, src.getLength());
  }
}
