package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.*;
import com.oracle.truffle.api.TruffleLanguage.ContextReference;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;
import org.enso.interpreter.epb.EpbContext;
import org.enso.interpreter.epb.EpbLanguage;
import org.enso.interpreter.epb.runtime.GuardedTruffleContext;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.Stateful;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public abstract class SafeEvalNode extends RootNode {
  private final String source;
  private @Child ForeignFunctionCallNode foreign;
  private @Child ContextFlipNode flipNode = ContextFlipNodeGen.create();
  private final String[] argNames;
  private final String lang;

  public SafeEvalNode(EpbLanguage language, String lang, String source, List<String> arguments) {
    super(language, new FrameDescriptor());
    this.source = source;
    argNames = arguments.toArray(new String[0]);
    this.lang = lang.equals("r") ? "R" : lang;
  }

  @Specialization
  Object doExecute(
      VirtualFrame frame,
      @CachedContext(EpbLanguage.class) ContextReference<EpbContext> contextRef) {
    ensureParsed(contextRef);
    return foreign.execute(frame.getArguments());
  }

  private void ensureParsed(ContextReference<EpbContext> ctxRef) {
    if (foreign == null) {
      getLock().lock();
      try {
        if (foreign == null) {
          CompilerDirectives.transferToInterpreterAndInvalidate();
          if (lang.equals("js")) {
            parseJs(ctxRef);
          } else {
            EpbContext context = ctxRef.get();
            GuardedTruffleContext inner = context.getInnerContext();
            Object p = inner.enter();
            try {
              Source source = Source.newBuilder(lang, this.source, "").build();
              CallTarget ct = ctxRef.get().getEnv().parseInternal(source, argNames);
              foreign = insert(DefaultForeignNodeGen.create(argNames.length, ct));
            } finally {
              inner.leave(p);
            }
          }
        }
      } finally {
        getLock().unlock();
      }
    }
  }

  private void parseJs(ContextReference<EpbContext> ctxRef) {
    EpbContext context = ctxRef.get();
    GuardedTruffleContext outer = context.getCurrentContext();
    GuardedTruffleContext inner = context.getInnerContext();
    Object p = inner.enter();
    try {
      String args = Arrays.stream(argNames).skip(1).collect(Collectors.joining(","));
      String wrappedSrc =
          "var poly_enso_eval=function(" + args + "){" + source + "};poly_enso_eval";
      Source source = Source.newBuilder(lang, wrappedSrc, "").build();
      CallTarget ct = ctxRef.get().getEnv().parseInternal(source);
      Object fn = flipNode.execute(ct.call(), inner, outer);
      foreign = insert(JsForeignNodeGen.create(argNames.length, fn));
    } finally {
      inner.leave(p);
    }
  }
}
