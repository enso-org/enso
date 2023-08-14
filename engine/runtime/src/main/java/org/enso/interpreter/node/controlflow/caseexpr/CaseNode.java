package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.*;
import org.enso.interpreter.runtime.state.State;
import org.enso.interpreter.runtime.type.TypesGen;

/**
 * A node representing a pattern match on an arbitrary runtime value.
 *
 * <p>Has a scrutinee node and a collection of {@link BranchNode}s. The case nodes get executed one
 * by one, until one throws an {@link BranchSelectedException}, the value of which becomes the
 * result of this pattern match.
 */
@NodeChild(value = "scrutinee", type = ExpressionNode.class)
@NodeInfo(shortName = "case_of", description = "The runtime representation of a case expression.")
public abstract class CaseNode extends ExpressionNode {

  @Children private final BranchNode[] cases;
  private final boolean isNested;

  private final CountingConditionProfile fallthroughProfile = CountingConditionProfile.create();

  CaseNode(boolean isNested, BranchNode[] cases) {
    this.cases = cases;
    this.isNested = isNested;
  }

  /**
   * Creates an instance of this node.
   *
   * @param scrutinee the value being scrutinised
   * @param cases the case branches
   * @param isNested if true, the flag indicates that the case node represents a nested pattern. If
   *     false, the case node represents a top-level case involving potentially nested patterns.
   * @return a node representing a pattern match
   */
  public static CaseNode build(ExpressionNode scrutinee, BranchNode[] cases, boolean isNested) {
    return CaseNodeGen.create(isNested, cases, scrutinee);
  }

  /**
   * Forwards an error in the case's scrutinee.
   *
   * <p>It is important that this is the first specialization.
   *
   * @param frame the stack frame in which to execute
   * @param error the error being matched against
   * @return the result of executing the case expression on {@code error}
   */
  @Specialization
  public Object doError(VirtualFrame frame, DataflowError error) {
    return error;
  }

  /**
   * Rethrows a panic sentinel if it encounters one.
   *
   * @param frame the stack frame in which to execute
   * @param sentinel the sentinel being matched against
   * @return nothing
   */
  @Specialization
  public Object doPanicSentinel(VirtualFrame frame, PanicSentinel sentinel) {
    CompilerDirectives.transferToInterpreter();
    throw sentinel;
  }

  @Specialization(guards = {"object != null", "warnings.hasWarnings(object)"})
  Object doWarning(
      VirtualFrame frame,
      Object object,
      @Shared("warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings) {
    try {
      EnsoContext ctx = EnsoContext.get(this);
      Warning[] ws = warnings.getWarnings(object, this);
      Object result = doMatch(frame, warnings.removeWarnings(object), warnings);
      return WithWarnings.wrap(ctx, result, ws);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  /**
   * Executes the case expression.
   *
   * @param frame the stack frame in which to execute
   * @param object the object being matched against
   * @return the result of executing the case expression on {@code object}
   */
  @Specialization(
      guards = {
        "!isDataflowError(object)",
        "!isPanicSentinel(object)",
        "!warnings.hasWarnings(object)"
      })
  @ExplodeLoop
  public Object doMatch(
      VirtualFrame frame,
      Object object,
      @Shared("warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings) {
    State state = Function.ArgumentsHelper.getState(frame.getArguments());
    try {
      for (BranchNode branchNode : cases) {
        branchNode.execute(frame, state, object);
      }
      if (fallthroughProfile.profile(isNested)) {
        return BranchResult.failure(this);
      } else {
        CompilerDirectives.transferToInterpreter();
        throw new PanicException(
            EnsoContext.get(this).getBuiltins().error().makeInexhaustivePatternMatch(object), this);
      }
    } catch (BranchSelectedException e) {
      // Note [Branch Selection Control Flow]
      return isNested ? e.getBranchResult() : e.getBranchResult().result();
    }
  }

  boolean isDataflowError(Object error) {
    return TypesGen.isDataflowError(error);
  }

  boolean isPanicSentinel(Object sentinel) {
    return TypesGen.isPanicSentinel(sentinel);
  }

  /* Note [Branch Selection Control Flow]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Truffle provides no easy way to return control flow from multiple paths. This is entirely due
   * to Java's (current) lack of support for case-expressions.
   *
   * As a result, this implementation resorts to using an exception to short-circuit evaluation on
   * a successful match. This short-circuiting also allows us to hand the result of evaluation back
   * out to the caller (here) wrapped in the exception.
   *
   * The main alternative to this was desugaring to a nested-if, which would've been significantly
   * harder to maintain, and also resulted in significantly higher code complexity.
   *
   * Note that the CaseNode may return either a BranchResult or it's underlying value.
   * This depends on whether the current CaseNode has been constructed as part of the desugaring phase
   * for nested patterns.
   * Case expressions that are synthetic, correspond to nested patterns and must propagate additional
   * information about the state of the match. That way, in the case of a failure in a deeply
   * nested case, other branches of the original case expression are tried.
   * `isNested` check ensures that `CaseResult` never leaks outside the CaseNode/BranchNode hierarchy.
   */
}
