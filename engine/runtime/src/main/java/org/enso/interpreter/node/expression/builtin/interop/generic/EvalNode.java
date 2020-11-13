package org.enso.interpreter.node.expression.builtin.interop.generic;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.Source;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(
    type = "Polyglot",
    name = "eval",
    description = "Evaluates a foreign language string.")
@ReportPolymorphism
public abstract class EvalNode extends Node {
  static final int LIMIT = 10;

  static EvalNode build() {
    return EvalNodeGen.create();
  }

  abstract Object execute(Object _this, Text language, Text code);

  @Specialization(
      guards = {"cachedLanguage == language", "cachedCode == code"},
      limit = "LIMIT")
  Object doCached(
      Object _this,
      Text language,
      Text code,
      @CachedContext(Language.class) Context context,
      @Cached("language") Text cachedLanguage,
      @Cached("code") Text cachedCode,
      @Cached("build()") ToJavaStringNode toJavaStringNode,
      @Cached("parse(context, cachedLanguage, cachedCode, toJavaStringNode)") CallTarget callTarget,
      @Cached("create(callTarget)") DirectCallNode callNode,
      @Cached("build()") HostValueToEnsoNode hostValueToEnsoNode) {
    return hostValueToEnsoNode.execute(callNode.call());
  }

  @Specialization(replaces = "doCached")
  Object doUncached(
      Object _this,
      Text language,
      Text code,
      @CachedContext(Language.class) Context context,
      @Cached IndirectCallNode callNode,
      @Cached("build()") ToJavaStringNode toJavaStringNode,
      @Cached("build()") HostValueToEnsoNode hostValueToEnsoNode) {
    CallTarget ct = parse(context, language, code, toJavaStringNode);
    return hostValueToEnsoNode.execute(callNode.call(ct));
  }

  CallTarget parse(Context context, Text language, Text code, ToJavaStringNode toJavaStringNode) {
    String languageStr = toJavaStringNode.execute(language);
    String codeStr = toJavaStringNode.execute(code);

    Source source = Source.newBuilder(languageStr, codeStr, "<polyglot_eval>").build();
    return context.getEnvironment().parsePublic(source);
  }
}
