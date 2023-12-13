package org.enso.interpreter.epb;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;

class ForeignEvalNode extends RootNode {
  private final EpbParser.Result code;
  private @Child
  ForeignFunctionCallNode foreign;

  private @CompilationFinal
  ForeignParsingException parseException;
  private final String[] argNames;

  /**
   * Creates a new instance of this node
   *
   * @param language the current language instance
   * @param code the result of parsing EPB code
   * @param arguments argument names allowed in the function body
   * @return an instance of this node
   */
  static ForeignEvalNode build(
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
      try {
        var toRet = foreign.execute(frame.getArguments());
        return toRet;
      } catch (InteropException ex) {
        throw new ForeignParsingException(ex.getMessage(), this);
      }
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
    var ctxLock = EpbContext.get(this).getLock();
    ctxLock.lock();
    try {
      if (foreign == null) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        var foreignLang = code.getLanguage();
        String truffleLangId = foreignLang.getTruffleId();
        var context = EpbContext.get(this);
        var installedLanguages = context.getEnv().getInternalLanguages();
        if (!installedLanguages.containsKey(truffleLangId)) {
          this.parseException
                  = new ForeignParsingException(truffleLangId, installedLanguages.keySet(), this);
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
      ctxLock.unlock();
    }
  }

  private void parseJs() {
    EpbContext context = EpbContext.get(this);
    GuardedTruffleContext inner = context.getInnerContext();
    String args = Arrays.stream(argNames).skip(1).collect(Collectors.joining(","));
    String wrappedSrc
            = "var poly_enso_eval=function("
            + args
            + "){\n"
            + code.getForeignSource()
            + "\n};poly_enso_eval";
    Source source = Source.newBuilder(code.getLanguage().getTruffleId(), wrappedSrc, "").build();
    var fn = inner.eval(this, source);
    foreign = insert(JsForeignNode.build(fn, argNames.length));
  }

  private void parsePy() {
    String args = Arrays.stream(argNames).collect(Collectors.joining(","));
    String head = """
        import site
        import polyglot
        @polyglot.export_value
        def polyglot_enso_python_eval("""
            + args
            + "):\n";
    String indentLines
            = code.getForeignSource().lines().map(l -> "    " + l).collect(Collectors.joining("\n"));
    Source source
            = Source.newBuilder(code.getLanguage().getTruffleId(), head + indentLines, "").build();
    EpbContext context = EpbContext.get(this);
    CallTarget ct = context.getEnv().parsePublic(source);
    ct.call();
    Object fn = context.getEnv().importSymbol("polyglot_enso_python_eval");
    foreign = insert(PyForeignNodeGen.create(fn));
  }

  private void parseR() {
    EpbContext context = EpbContext.get(this);
    String args = String.join(",", argNames);
    String wrappedSrc = "function(" + args + "){\n" + code.getForeignSource() + "\n}";
    Source source = Source.newBuilder(code.getLanguage().getTruffleId(), wrappedSrc, "").build();
    CallTarget ct = context.getEnv().parsePublic(source);
    foreign = insert(RForeignNodeGen.create(ct.call()));
  }
}
