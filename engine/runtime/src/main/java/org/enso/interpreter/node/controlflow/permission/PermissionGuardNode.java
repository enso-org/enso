package org.enso.interpreter.node.controlflow.permission;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.State;

public class PermissionGuardNode extends ExpressionNode {
  private @Child ExpressionNode body;
  private final boolean checkInput;
  private final boolean checkOutput;
  private final BranchProfile inputDisallowed = BranchProfile.create();
  private final BranchProfile outputDisallowed = BranchProfile.create();

  public PermissionGuardNode(ExpressionNode body, boolean checkInput, boolean checkOutput) {
    this.body = body;
    this.checkInput = checkInput;
    this.checkOutput = checkOutput;
  }

  @Override
  public Object executeGeneric(VirtualFrame frame) {
    State state = Function.ArgumentsHelper.getState(frame.getArguments());

    if (checkInput && !state.getIoPermissions().isInputAllowed()) {
      inputDisallowed.enter();
      throw new PanicException(
          EnsoContext.get(this)
              .getBuiltins()
              .error()
              .getForbiddenOperation()
              .newInstance(Text.create("Input")),
          this);
    }

    if (checkOutput && !state.getIoPermissions().isOutputAllowed()) {
      outputDisallowed.enter();
      throw new PanicException(
          EnsoContext.get(this)
              .getBuiltins()
              .error()
              .getForbiddenOperation()
              .newInstance(Text.create("Output")),
          this);
    }

    return body.executeGeneric(frame);
  }
}
