package org.enso.interpreter.node.callable.argument;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WithWarnings;

import java.util.stream.Stream;

public class GatherWarningsNode extends BaseNode {
  private final int argsCount;
  private final @CompilerDirectives.CompilationFinal(dimensions = 1) ConditionProfile[] profiles;

  private GatherWarningsNode(int argsCount) {
    this.argsCount = argsCount;
    this.profiles =
        Stream.generate(ConditionProfile::createCountingProfile)
            .limit(argsCount)
            .toArray(ConditionProfile[]::new);
  }

  public static GatherWarningsNode create(int argsCount) {
    return new GatherWarningsNode(argsCount);
  }

  @ExplodeLoop
  public ArrayRope<Warning> execute(Object... arguments) {
    ArrayRope<Warning> result = new ArrayRope<>();
    boolean anyFound = false;
    for (int i = 0; i < argsCount; i++) {
      if (profiles[i].profile(arguments[i] instanceof WithWarnings)) {
        anyFound = true;
        result = result.append(((WithWarnings) arguments[i]).getWarnings());
      }
    }
    if (anyFound) {
      return result;
    } else {
      return null;
    }
  }
}
