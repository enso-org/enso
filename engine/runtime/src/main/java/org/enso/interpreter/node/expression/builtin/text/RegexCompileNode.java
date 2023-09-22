package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.Source;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Prim_Text_Helper",
    name = "compile_regex",
    description = "Compiles a regexp.",
    autoRegister = false)
public abstract class RegexCompileNode extends Node {
  static RegexCompileNode build() {
    return RegexCompileNodeGen.create();
  }

  abstract Object execute(Object pattern, Object options);

  @Specialization(
      limit = "3",
      guards = {
        "pattern.toString().equals(cachedPattern)",
        "options.toString().equals(cachedOptions)"
      })
  Object parseRegexPattern(
      Text pattern,
      Text options,
      @Cached("pattern.toString()") String cachedPattern,
      @Cached("options.toString()") String cachedOptions,
      @Cached("compile(cachedPattern, cachedOptions)") Object regex) {
    return regex;
  }

  @Specialization
  Object alwaysCompile(Text pattern, Text options) {
    return compile(pattern.toString(), options.toString());
  }

  @Fallback
  Object doOther(Object pattern, Object options) {
    Builtins builtins = EnsoContext.get(this).getBuiltins();
    Atom err = builtins.error().makeTypeError(builtins.text(), pattern, "pattern");
    throw new PanicException(err, this);
  }

  @TruffleBoundary
  Object compile(String pattern, String options) {
    var ctx = EnsoContext.get(this);
    var s = "Flavor=ECMAScript/" + pattern + "/" + options;
    var src =
        Source.newBuilder("regex", s, "myRegex")
            .mimeType("application/tregex")
            .internal(true)
            .build();

    try {
      var regex = ctx.parseInternal(src).call();
      return regex;
    } catch (AbstractTruffleException e) {
      Builtins builtins = ctx.getBuiltins();
      String msg = "Regex parse error: " + e.getMessage();
      Atom err = builtins.error().makeSyntaxError(msg);
      throw new PanicException(err, this);
    }
  }
}
