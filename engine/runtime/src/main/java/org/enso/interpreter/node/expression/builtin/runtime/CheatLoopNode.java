package org.enso.interpreter.node.expression.builtin.runtime;

import cats.data.Func;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.LoopNode;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.data.EmptyMap;

@BuiltinMethod(type = "Runtime", name = "cheat_loop", description = "cheats a loop")
@ReportPolymorphism
public abstract class CheatLoopNode extends Node {
  public static CheatLoopNode build() {
    return CheatLoopNodeGen.create();
  }

  public abstract Object execute(Object _this, long count, Object fun);

  @Specialization(guards = "fn.getCallTarget() == cachedCt")
  public Object doCached(
      Object _this,
      long count,
      Function fn,
      @Cached("fn.getCallTarget()") RootCallTarget cachedCt,
      @Cached("create(fn.getCallTarget())") DirectCallNode callNode,
      @CachedContext(Language.class) Context ctx) {
    for (int i = 0; i < count; i++) {
      callNode.call(
          Function.ArgumentsHelper.buildArguments(fn, null, EmptyMap.create(), new Object[] {i}));
    }
    LoopNode.reportLoopCount(this, (int) count);
    return ctx.getUnit().newInstance();
  }
}
