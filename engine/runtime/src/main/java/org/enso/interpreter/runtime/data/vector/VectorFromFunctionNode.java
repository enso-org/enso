package org.enso.interpreter.runtime.data.vector;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import com.oracle.truffle.api.profiles.LoopConditionProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.HasContextEnabledNode;
import org.enso.interpreter.runtime.state.State;
import org.enso.interpreter.runtime.warning.AppendWarningNode;
import org.enso.interpreter.runtime.warning.Warning;
import org.enso.interpreter.runtime.warning.WarningsLibrary;

@BuiltinMethod(
    type = "Array_Like_Helpers",
    name = "vector_from_function",
    description = "Creates a vector from a function.")
public abstract class VectorFromFunctionNode extends Node {
  public static VectorFromFunctionNode build() {
    return VectorFromFunctionNodeGen.create();
  }

  private static final int MAX_MAP_WARNINGS = 10;

  /**
   * @param length Length of the vector to create.
   * @param func Callback function called with index as argument.
   * @param onProblems Can be either an atom of type {@code Problem_Behavior} or {@code No_Wrap}
   *     type.
   * @return Vector constructed from the given function.
   */
  abstract Object execute(
      VirtualFrame frame, State state, long length, Function func, Object onProblems);

  @Specialization(
      guards = "getCtor(onProblemsAtom) == onProblemsAtomCtorCached",
      limit = "onProblemsCtorsCount()")
  Object doItCached(
      VirtualFrame frame,
      State state,
      long length,
      Function func,
      Atom onProblemsAtom,
      @Cached("getCtor(onProblemsAtom)") AtomConstructor onProblemsAtomCtorCached,
      @Cached("processOnProblemsArg(onProblemsAtomCtorCached)") OnProblems onProblems,
      @Cached("buildWithArity(1)") InvokeFunctionNode invokeFunctionNode,
      @Cached("build()") AppendWarningNode appendWarningNode,
      @CachedLibrary(limit = "3") WarningsLibrary warnsLib,
      @Cached BranchProfile errorEncounteredProfile,
      @Cached HasContextEnabledNode hasContextEnabledNode,
      @Cached LoopConditionProfile loopConditionProfile) {
    var ctx = EnsoContext.get(this);
    var len = (int) length;
    var nothing = ctx.getNothing();
    var target = ArrayBuilder.newBuilder(len);
    var errorsEncountered = 0;
    loopConditionProfile.profileCounted(len);
    for (int i = 0; loopConditionProfile.inject(i < len); i++) {
      var value = invokeFunctionNode.execute(func, frame, state, new Long[] {(long) i});
      Object valueToAdd = value;
      if (value instanceof DataflowError err) {
        errorEncounteredProfile.enter();
        switch (onProblems) {
          case IGNORE -> valueToAdd = nothing;
          case REPORT_ERROR -> {
            var mapErr = ctx.getBuiltins().error().makeMapError(i, err.getPayload());
            return DataflowError.withDefaultTrace(state, mapErr, this, hasContextEnabledNode);
          }
          case REPORT_WARNING -> {
            errorsEncountered++;
            if (errorsEncountered > MAX_MAP_WARNINGS) {
              valueToAdd = nothing;
            } else {
              var wrappedInWarn =
                  Warning.attach(ctx, nothing, err.getPayload(), null, appendWarningNode);
              valueToAdd = wrappedInWarn;
            }
          }
          case NO_WRAP -> {
            return err;
          }
        }
      }
      target.add(valueToAdd, warnsLib);
    }
    var vector = target.asVector(true);
    if (errorsEncountered >= MAX_MAP_WARNINGS) {
      var additionalWarnsBuiltin = ctx.getBuiltins().additionalWarnings();
      long additionalWarnsCnt = errorsEncountered - MAX_MAP_WARNINGS;
      var additionalWarns = additionalWarnsBuiltin.newInstance(additionalWarnsCnt);
      var vecWithAdditionalWarns =
          Warning.attach(ctx, vector, additionalWarns, null, appendWarningNode);
      return vecWithAdditionalWarns;
    } else {
      return vector;
    }
  }

  /**
   * Unreachable: The {@code doItCached} specialization has the same limit of instantiations as
   * there are possible onProblems arguments. So this specialization is only reached if {@code
   * onProblems} argument is an unexpected type.
   *
   * @return Just throws Type_Error dataflow error.
   */
  @Specialization(replaces = "doItCached")
  Object unreachable(
      VirtualFrame frame, State state, long length, Function func, Object onProblems) {
    var problemBehaviorBuiltin = EnsoContext.get(this).getBuiltins().problemBehavior();
    throw makeTypeError(problemBehaviorBuiltin.getType(), onProblems, "onProblems");
  }

  protected OnProblems processOnProblemsArg(AtomConstructor onProblems) {
    var ctx = EnsoContext.get(this);
    var problemBehaviorBuiltin = ctx.getBuiltins().problemBehavior();
    var noWrapBuiltin = ctx.getBuiltins().noWrap();
    if (onProblems == problemBehaviorBuiltin.getIgnore()) {
      return OnProblems.IGNORE;
    } else if (onProblems == problemBehaviorBuiltin.getReportError()) {
      return OnProblems.REPORT_ERROR;
    } else if (onProblems == problemBehaviorBuiltin.getReportWarning()) {
      return OnProblems.REPORT_WARNING;
    } else if (onProblems == noWrapBuiltin.getUniqueConstructor()) {
      return OnProblems.NO_WRAP;
    }
    throw makeTypeError(problemBehaviorBuiltin.getType(), onProblems, "onProblems");
  }

  protected static AtomConstructor getCtor(Atom atom) {
    return atom.getConstructor();
  }

  protected static int onProblemsCtorsCount() {
    return OnProblems.values().length;
  }

  @TruffleBoundary
  private PanicException makeTypeError(Object expected, Object actual, String name) {
    var ctx = EnsoContext.get(this);
    var typeError = ctx.getBuiltins().error().makeTypeError(expected, actual, name);
    return new PanicException(typeError, this);
  }

  /** All the possible values for the {@code onProblems} argument. */
  protected enum OnProblems {
    IGNORE,
    REPORT_ERROR,
    REPORT_WARNING,
    NO_WRAP
  }
}
