package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.dsl.Cached;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.text.Text;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.Source;
import org.enso.interpreter.runtime.EnsoContext;

@BuiltinMethod(
    type = "Prim_Text_Helper",
    name = "compile_regex",
    description = "Compiles a regexp.",
    autoRegister = false)
public abstract class RegexCompileNode extends Node {
  static RegexCompileNode build() {
    return RegexCompileNodeGen.create();
  }

  abstract Object execute(Object self, Object pattern, Object options);



  @Specialization(limit = "3", guards = {
    "pattern.toString().equals(cachedPattern)",
    "options == cachedOptions"
  })
  Object parseRegexPattern(Object self, Text pattern, long options,
    @Cached("pattern.toString()") String cachedPattern,
    @Cached("options") long cachedOptions,
    @Cached("compile(cachedPattern, cachedOptions)") Object regex
  ) {
    return regex;
  }

  @Specialization
  Object alwaysCompile(Object self, Text pattern, long options) {
    return compile(pattern.toString(), options);
  }

  Object compile(String pattern, long options) {
    var ctx = EnsoContext.get(this);
    var env = ctx.getEnvironment();
    var s = "Flavor=ECMAScript/" + pattern + "/"; // + options;
    var src =
        Source.newBuilder("regex", s, "myRegex")
            .mimeType("application/tregex")
            .internal(true)
            .build();
    var regex = env.parseInternal(src).call();
    return regex;
  }
}
