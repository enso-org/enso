package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameUtil;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.RuntimeError;
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

  CaseNode(BranchNode[] cases) {
    this.cases = cases;
  }

  /**
   * Creates an instance of this node.
   *
   * @param scrutinee the value being scrutinised
   * @param cases the case branches
   * @return a node representing a pattern match
   */
  public static CaseNode build(ExpressionNode scrutinee, BranchNode[] cases) {
    return CaseNodeGen.create(cases, scrutinee);
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
  public Object doError(VirtualFrame frame, RuntimeError error) {
    return error;
  }

  /**
   * Executes the case expression.
   *
   * @param frame the stack frame in which to execute
   * @param object the object being matched against
   * @param ctx the language context reference
   * @return the result of executing the case expression on {@code object}
   */
  @Specialization(guards = "!isError(object)")
  @ExplodeLoop
  public Object doMatch(
      VirtualFrame frame,
      Object object,
      @CachedContext(Language.class) TruffleLanguage.ContextReference<Context> ctx) {
    Object state = FrameUtil.getObjectSafe(frame, getStateFrameSlot());
    try {
      for (BranchNode branchNode : cases) {
        branchNode.execute(frame, state, object);
      }
      CompilerDirectives.transferToInterpreter();
      throw new PanicException(
          ctx.get().getBuiltins().error().inexhaustivePatternMatchError().newInstance(object),
          this);
    } catch (BranchSelectedException e) {
      // Note [Branch Selection Control Flow]
      frame.setObject(getStateFrameSlot(), e.getResult().getState());
      return e.getResult().getValue();
    }
  }

  boolean isError(Object error) {
    return TypesGen.isRuntimeError(error);
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
   */
}
