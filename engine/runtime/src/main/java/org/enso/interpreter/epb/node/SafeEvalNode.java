package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.*;
import com.oracle.truffle.api.TruffleLanguage.ContextReference;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;
import org.enso.interpreter.epb.EpbContext;
import org.enso.interpreter.epb.EpbLanguage;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.Stateful;

import java.util.Arrays;
import java.util.List;

public abstract class SafeEvalNode extends RootNode {
  private final String source;
  private @Child DirectCallNode callNode;
  private @Children ContextFlipNode[] argConverters;
  private @Child ContextFlipNode resultConverter = ContextFlipNodeGen.create();
  private final String[] argNames;
  private final String lang;

  public SafeEvalNode(EpbLanguage language, String lang, String source, List<String> arguments) {
    super(language, new FrameDescriptor());
    this.source = source;
    argNames = arguments.toArray(new String[0]);
    if (argNames.length > 0 && argNames[0].equals("this")) {
      argNames[0] = "here";
    }
    argConverters = new ContextFlipNode[argNames.length];
    for (int i = 0; i < argNames.length; i++) {
      argConverters[i] = ContextFlipNodeGen.create();
    }
    this.lang = lang.equals("r") ? "R" : lang;
  }

  @Specialization
  Stateful doExecute(
      VirtualFrame frame,
      @CachedContext(EpbLanguage.class) ContextReference<EpbContext> contextRef) {
    EpbContext context = contextRef.get();
    TruffleContext outer = context.getEnv().getContext();
    TruffleContext inner = context.getInnerContext();
    ensureParsed(contextRef, inner);
    Object[] args =
        prepareArgs(
            Function.ArgumentsHelper.getPositionalArguments(frame.getArguments()), inner, outer);
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    return doRun(args, state, inner, outer);
  }

  private void ensureParsed(ContextReference<EpbContext> ctxRef, TruffleContext inner) {
    if (callNode == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      Object p = inner.enter();
      try {
        Source source = Source.newBuilder(lang, this.source, "").build();
        CallTarget ct = ctxRef.get().getEnv().parseInternal(source, argNames);
        callNode = Truffle.getRuntime().createDirectCallNode(ct);
      } finally {
        inner.leave(p);
      }
    }
  }

  private Stateful doRun(
      Object[] arguments, Object state, TruffleContext inner, TruffleContext outer) {
    Object p = inner.enter();
    try {
      Object r = callNode.call(arguments);
      Object wrapped = resultConverter.execute(r, inner, outer);
      return new Stateful(state, wrapped);
    } finally {
      inner.leave(p);
    }
  }

  @ExplodeLoop
  private Object[] prepareArgs(Object[] args, TruffleContext inner, TruffleContext outer) {
    Object[] newArgs = new Object[argConverters.length];
    for (int i = 0; i < argConverters.length; i++) {
      newArgs[i] = argConverters[i].execute(args[i], outer, inner);
    }
    return newArgs;
  }
}
