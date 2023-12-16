package org.enso.interpreter.epb;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.stream.Collectors;

final class ForeignEvalNode extends RootNode {
  private final Source langAndCode;
  private @Child ForeignFunctionCallNode foreign;
  private final String[] argNames;

  private ForeignEvalNode(EpbLanguage language, Source langAndCode, List<String> arguments) {
    super(language, new FrameDescriptor());
    this.langAndCode = langAndCode;
    this.argNames = arguments.toArray(new String[0]);
  }

  static ForeignEvalNode parse(EpbLanguage epb, Source langAndCode, List<String> args) {
    var node = new ForeignEvalNode(epb, langAndCode, args);
    return node;
  }

  private String truffleId(Source langAndCode) {
    var seq = langAndCode.getCharacters();
    return seq.subSequence(0, splitAt(seq)).toString().toLowerCase();
  }

  private int splitAt(CharSequence seq) {
    var at = 0;
    while (at < seq.length()) {
      if (seq.charAt(at) == '#') {
        return at;
      }
      at++;
    }
    throw new ForeignParsingException("No # found", this);
  }

  private String foreignSource(Source langAndCode) {
    var seq = langAndCode.getCharacters();
    return seq.toString().substring(splitAt(seq) + 1);
  }

  @Override
  public Object execute(VirtualFrame frame) {
    if (foreign == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      var id = truffleId(langAndCode);
      var context = EpbContext.get(this);
      var installedLanguages = context.getEnv().getInternalLanguages();
      var node =
          switch (installedLanguages.containsKey(id) ? 1 : 0) {
            case 0 -> {
              var ex = new ForeignParsingException(id, installedLanguages.keySet(), this);
              yield new ExceptionForeignNode(ex);
            }
            default -> {
              context.log(
                  Level.FINE,
                  "Parsing foreign script {1} - language {0}",
                  id,
                  langAndCode.getName());
              yield switch (id) {
                case "js" -> parseJs();
                case "python" -> parseGeneric("python", PyForeignNode::new);
                default -> parseGeneric(id, GenericForeignNode::new);
              };
            }
          };
      foreign = insert(node);
    }
    try {
      var toRet = foreign.execute(frame.getArguments());
      return toRet;
    } catch (InteropException ex) {
      throw new ForeignParsingException(ex.getMessage(), this);
    }
  }

  private ForeignFunctionCallNode parseJs() {
    var context = EpbContext.get(this);
    var inner = context.getInnerContext();
    var code = foreignSource(langAndCode);
    var args = Arrays.stream(argNames).skip(1).collect(Collectors.joining(","));
    var wrappedSrc = "var poly_enso_eval=function(" + args + "){\n" + code + "\n};poly_enso_eval";
    Source source = Source.newBuilder("js", wrappedSrc, "").build();
    var fn = inner.evalPublic(this, source);
    return JsForeignNode.build(fn);
  }

  private ForeignFunctionCallNode parseGeneric(
      String language, Function<CallTarget, ForeignFunctionCallNode> nodeFactory) {
    var ctx = EpbContext.get(this);
    Source source =
        Source.newBuilder(language, foreignSource(langAndCode), langAndCode.getName()).build();
    CallTarget ct = ctx.getEnv().parsePublic(source, argNames);
    return nodeFactory.apply(ct);
  }
}
