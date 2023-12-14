package org.enso.interpreter.epb;

import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.stream.Collectors;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;

final class ForeignEvalNode extends RootNode {
  private final Source code;
  private @Child ForeignFunctionCallNode foreign;
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
  static ForeignEvalNode build(
          EpbLanguage language, Source code, List<String> arguments) {
    return new ForeignEvalNode(language, code, arguments);
  }

  ForeignEvalNode(EpbLanguage language, Source code, List<String> arguments) {
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

  private void ensureParsed() throws IllegalStateException {
    if (foreign == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      String truffleLangId = EpbLanguage.truffleId(code);
      var context = EpbContext.get(this);
      var installedLanguages = context.getEnv().getInternalLanguages();
      if (!installedLanguages.containsKey(truffleLangId)) {
        this.parseException = new ForeignParsingException(truffleLangId, installedLanguages.keySet(), this);
      } else {
        context.log(Level.FINE, "Parsing foreign script {1} - language {0}", truffleLangId, code.getName());
        switch (truffleLangId) {
          case "js" -> parseJs();
          case "python" -> parseGeneric("python", PyForeignNode::new);
          default -> parseGeneric(truffleLangId, GenericForeignNode::new);
        }
      }
    }
  }

  private void parseJs() {
    var context = EpbContext.get(this);
    var inner = context.getInnerContext();
    String args = Arrays.stream(argNames).skip(1).collect(Collectors.joining(","));
    String wrappedSrc
            = "var poly_enso_eval=function("
            + args
            + "){\n"
            + EpbLanguage.foreignSource(code)
            + "\n};poly_enso_eval";
    Source source = Source.newBuilder("js", wrappedSrc, "").build();
    var fn = inner.evalPublic(this, source);
    foreign = insert(JsForeignNode.build(fn));
  }

  private void parseGeneric(String language, Function<CallTarget,ForeignFunctionCallNode> nodeFactory) {
    var ctx = EpbContext.get(this);
    Source source = Source.newBuilder(language, EpbLanguage.foreignSource(code), code.getName()).build();
    CallTarget ct = ctx.getEnv().parsePublic(source, argNames);
    foreign = insert(nodeFactory.apply(ct));
  }
}
