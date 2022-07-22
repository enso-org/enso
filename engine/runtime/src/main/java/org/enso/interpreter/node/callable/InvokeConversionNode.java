package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import com.oracle.truffle.api.profiles.ConditionProfile;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.*;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;
import org.enso.interpreter.runtime.state.Stateful;

import java.util.UUID;
import java.util.concurrent.locks.Lock;

public abstract class InvokeConversionNode extends BaseNode {
  private @Child InvokeFunctionNode invokeFunctionNode;
  private @Child InvokeConversionNode childDispatch;
  private final ConditionProfile atomProfile = ConditionProfile.createCountingProfile();
  private final ConditionProfile atomConstructorProfile = ConditionProfile.createCountingProfile();
  private final int thatArgumentPosition;

  /**
   * Creates a new node for method invocation.
   *
   * @param schema a description of the arguments being applied to the callable
   * @param defaultsExecutionMode the defaulted arguments handling mode for this call
   * @param argumentsExecutionMode the arguments execution mode for this call
   * @return a new invoke method node
   */
  public static InvokeConversionNode build(
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      int thatArgumentPosition) {
    return InvokeConversionNodeGen.create(
        schema, defaultsExecutionMode, argumentsExecutionMode, thatArgumentPosition);
  }

  InvokeConversionNode(
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      int thatArgumentPosition) {
    this.invokeFunctionNode =
        InvokeFunctionNode.build(schema, defaultsExecutionMode, argumentsExecutionMode);
    this.thatArgumentPosition = thatArgumentPosition;
  }

  @Override
  public void setTailStatus(TailStatus tailStatus) {
    super.setTailStatus(tailStatus);
    this.invokeFunctionNode.setTailStatus(tailStatus);
  }

  public abstract Stateful execute(
      VirtualFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object self,
      Object that,
      Object[] arguments);

  static AtomConstructor extractConstructor(
      Node thisNode,
      Object self,
      ConditionProfile atomConstructorProfile,
      ConditionProfile atomProfile) {
    if (atomConstructorProfile.profile(self instanceof AtomConstructor)) {
      return (AtomConstructor) self;
    } else if (atomProfile.profile(self instanceof Atom)) {
      return ((Atom) self).getConstructor();
    } else {
      throw new PanicException(
          Context.get(thisNode).getBuiltins().error().makeInvalidConversionTargetError(self),
          thisNode);
    }
  }

  AtomConstructor extractConstructor(Object self) {
    return extractConstructor(this, self, atomConstructorProfile, atomProfile);
  }

  @Specialization(guards = "dispatch.canConvertFrom(that)")
  Stateful doConvertFrom(
      VirtualFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object self,
      Object that,
      Object[] arguments,
      @CachedLibrary(limit = "10") MethodDispatchLibrary dispatch) {
    try {
      Function function =
          dispatch.getConversionFunction(that, extractConstructor(self), conversion);
      return invokeFunctionNode.execute(function, frame, state, arguments);
    } catch (MethodDispatchLibrary.NoSuchConversionException e) {
      throw new PanicException(
          Context.get(this).getBuiltins().error().makeNoSuchConversionError(self, that, conversion),
          this);
    }
  }

  @Specialization
  Stateful doDataflowError(
      VirtualFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object self,
      DataflowError that,
      Object[] arguments,
      @CachedLibrary(limit = "10") MethodDispatchLibrary dispatch,
      @Cached BranchProfile profile) {
    try {
      Function function =
          dispatch.getConversionFunction(that, extractConstructor(self), conversion);
      return invokeFunctionNode.execute(function, frame, state, arguments);
    } catch (MethodDispatchLibrary.NoSuchConversionException e) {
      profile.enter();
      return new Stateful(state, that);
    }
  }

  @Specialization
  Stateful doPanicSentinel(
      VirtualFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object self,
      PanicSentinel that,
      Object[] arguments) {
    throw that;
  }

  @Specialization
  Stateful doWarning(
      VirtualFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object self,
      WithWarnings that,
      Object[] arguments) {
    // Cannot use @Cached for childDispatch, because we need to call notifyInserted.
    if (childDispatch == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      Lock lock = getLock();
      lock.lock();
      try {
        if (childDispatch == null) {
          childDispatch =
              insert(
                  build(
                      invokeFunctionNode.getSchema(),
                      invokeFunctionNode.getDefaultsExecutionMode(),
                      invokeFunctionNode.getArgumentsExecutionMode(),
                      thatArgumentPosition));
          childDispatch.setTailStatus(getTailStatus());
          notifyInserted(childDispatch);
        }
      } finally {
        lock.unlock();
      }
    }
    arguments[thatArgumentPosition] = that.getValue();
    ArrayRope<Warning> warnings = that.getReassignedWarnings(this);
    Stateful result =
        childDispatch.execute(frame, state, conversion, self, that.getValue(), arguments);
    return new Stateful(result.getState(), WithWarnings.prependTo(result.getValue(), warnings));
  }

  @Specialization(guards = "interop.isString(that)")
  Stateful doConvertText(
      VirtualFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object self,
      Object that,
      Object[] arguments,
      @CachedLibrary(limit = "1") MethodDispatchLibrary textDispatch,
      @CachedLibrary(limit = "10") InteropLibrary interop) {
    try {
      String str = interop.asString(that);
      Text txt = Text.create(str);
      Function function =
          textDispatch.getConversionFunction(txt, extractConstructor(self), conversion);
      arguments[0] = txt;
      return invokeFunctionNode.execute(function, frame, state, arguments);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, that is guaranteed to be a string.");
    } catch (MethodDispatchLibrary.NoSuchConversionException e) {
      throw new PanicException(
          Context.get(this).getBuiltins().error().makeNoSuchConversionError(self, that, conversion),
          this);
    }
  }

  @Specialization(
      guards = {
        "!methods.canConvertFrom(that)",
        "!interop.isString(that)",
        "!methods.hasSpecialConversion(that)"
      })
  Stateful doFallback(
      VirtualFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object self,
      Object that,
      Object[] arguments,
      @CachedLibrary(limit = "10") MethodDispatchLibrary methods,
      @CachedLibrary(limit = "10") InteropLibrary interop) {
    throw new PanicException(
        Context.get(this).getBuiltins().error().makeNoSuchConversionError(self, that, conversion),
        this);
  }

  @Override
  public SourceSection getSourceSection() {
    Node parent = getParent();
    return parent == null ? null : parent.getSourceSection();
  }

  /**
   * Sets the expression ID of this node.
   *
   * @param id the expression ID to assign this node.
   */
  public void setId(UUID id) {
    invokeFunctionNode.setId(id);
  }
}
