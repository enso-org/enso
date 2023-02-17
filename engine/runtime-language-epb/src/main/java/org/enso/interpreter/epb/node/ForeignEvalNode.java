package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.enso.interpreter.epb.EpbContext;
import org.enso.interpreter.epb.EpbLanguage;
import org.enso.interpreter.epb.EpbParser;
import org.enso.interpreter.epb.runtime.ForeignParsingException;
import org.enso.interpreter.epb.runtime.GuardedTruffleContext;
import org.graalvm.polyglot.Context;

public class ForeignEvalNode extends RootNode {
  private final EpbParser.Result code;
  private @Child ForeignFunctionCallNode foreign;
  private @Child ContextRewrapNode rewrapNode = ContextRewrapNode.build();

  private @Child ContextRewrapExceptionNode rewrapExceptionNode =
      ContextRewrapExceptionNode.build();
  private @CompilationFinal ForeignParsingException parseException;
  private final String[] argNames;

  /**
   * Creates a new instance of this node
   *
   * @param language the current language instance
   * @param code the result of parsing EPB code
   * @param arguments argument names allowed in the function body
   * @return an instance of this node
   */
  public static ForeignEvalNode build(
      EpbLanguage language, EpbParser.Result code, List<String> arguments) {
    return new ForeignEvalNode(language, code, arguments);
  }

  ForeignEvalNode(EpbLanguage language, EpbParser.Result code, List<String> arguments) {
    super(language, new FrameDescriptor());
    this.code = code;
    argNames = arguments.toArray(new String[0]);
  }

  public Object execute(VirtualFrame frame) {
    ensureParsed();
    if (foreign != null) {
      return foreign.execute(frame.getArguments());
    } else {
      CompilerDirectives.transferToInterpreter();
      throw parseException;
    }
  }

  private void ensureParsed() {
    if (foreign == null && parseException == null) {
      lockAndParse();
    }
  }

  @CompilerDirectives.TruffleBoundary
  private void lockAndParse() throws IllegalStateException {
    getLock().lock();
    try {
      if (foreign == null) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        var foreignLang = code.getLanguage();
        String truffleLangId = foreignLang.getTruffleId();
        var installedLanguages = Context.getCurrent().getEngine().getLanguages();
        if (!installedLanguages.containsKey(truffleLangId)) {
          this.parseException =
              new ForeignParsingException(truffleLangId, installedLanguages.keySet(), this);
        } else {
          switch (foreignLang) {
            case JS:
              parseJs();
              break;
            case PY:
              parsePy();
              break;
            case R:
              parseR();
              break;
            default:
              throw new IllegalStateException("Unsupported language resulted from EPB parsing");
          }
        }
      }
    } finally {
      getLock().unlock();
    }
  }

  private void parseJs() {
    EpbContext context = EpbContext.get(this);
    GuardedTruffleContext outer = context.getCurrentContext();
    GuardedTruffleContext inner = context.getInnerContext();
    Object p = inner.enter(this);
    try {
      String args = Arrays.stream(argNames).skip(1).collect(Collectors.joining(","));
      String wrappedSrc =
          "var poly_enso_eval=function("
              + args
              + "){\n"
              + code.getForeignSource()
              + "\n};poly_enso_eval";
      Source source = Source.newBuilder(code.getLanguage().getTruffleId(), wrappedSrc, "").build();

      // After calling inner.enter, operating in a different, isolated truffle instance so need to
      // call one with the correct semantics.
      CallTarget ct = EpbContext.get(this).getEnv().parsePublic(source);
      Object fn = rewrapNode.execute(ct.call(), inner, outer);
      foreign = insert(JsForeignNode.build(fn, argNames.length));
    } catch (Throwable e) {
      if (InteropLibrary.getUncached().isException(e)) {
        throw rewrapExceptionNode.execute((AbstractTruffleException) e, inner, outer);
      } else {
        throw e;
      }
    } finally {
      inner.leave(this, p);
    }
  }

  private void parsePy() {
    try {
      String args = Arrays.stream(argNames).collect(Collectors.joining(","));
      String head =
          "import polyglot\n"
              + "@polyglot.export_value\n"
              + "def polyglot_enso_python_eval("
              + args
              + "):\n";
      String indentLines =
          code.getForeignSource().lines().map(l -> "    " + l).collect(Collectors.joining("\n"));
      Source source =
          Source.newBuilder(code.getLanguage().getTruffleId(), head + indentLines, "").build();
      EpbContext context = EpbContext.get(this);
      CallTarget ct = context.getEnv().parsePublic(source);
      ct.call();
      Object fn = context.getEnv().importSymbol("polyglot_enso_python_eval");
      foreign = insert(PyForeignNodeGen.create(fn));
    } catch (Throwable e) {
      if (InteropLibrary.getUncached().isException(e)) {
        throw (AbstractTruffleException) e;
      } else {
        throw e;
      }
    }
  }

  private void parseR() {
    EpbContext context = EpbContext.get(this);
    GuardedTruffleContext outer = context.getCurrentContext();
    GuardedTruffleContext inner = context.getInnerContext();
    Object p = inner.enter(this);
    try {
      String args = String.join(",", argNames);
      String wrappedSrc = "function(" + args + "){\n" + code.getForeignSource() + "\n}";
      Source source = Source.newBuilder(code.getLanguage().getTruffleId(), wrappedSrc, "").build();

      // After calling inner.enter, operating in a different, isolated truffle instance so need to
      // call one with the correct semantics.
      CallTarget ct = EpbContext.get(this).getEnv().parsePublic(source);
      Object fn = rewrapNode.execute(ct.call(), inner, outer);
      foreign = insert(RForeignNodeGen.create(fn));
    } catch (Throwable e) {
      if (InteropLibrary.getUncached().isException(e)) {
        throw rewrapExceptionNode.execute((AbstractTruffleException) e, inner, outer);
      } else {
        throw e;
      }
    } finally {
      inner.leave(this, p);
    }
  }
}
