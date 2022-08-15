package org.enso.interpreter.node.callable.resolver;

import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.PanicException;

/** Discovers and performs method calls on foreign values. */
@GenerateUncached
@ReportPolymorphism
@ImportStatic(HostMethodCallNode.PolyglotCallType.class)
public abstract class HostMethodCallNode extends Node {

  /** Represents a mode of calling a method on a polyglot value. */
  public enum PolyglotCallType {
    /**
     * The method call should be handled through {@link InteropLibrary#invokeMember(Object, String,
     * Object...)}.
     */
    CALL_METHOD,
    /**
     * The method call should be handled through {@link InteropLibrary#readMember(Object, String)}.
     */
    GET_MEMBER,
    /**
     * The method call should be handled through {@link InteropLibrary#instantiate(Object,
     * Object...)}.
     */
    INSTANTIATE,
    /** The method call should be handled through {@link InteropLibrary#getArraySize(Object)}. */
    GET_ARRAY_LENGTH,
    /**
     * The method call should be handled through {@link InteropLibrary#readArrayElement(Object,
     * long)}.
     */
    READ_ARRAY_ELEMENT,
    /**
     * The method call should be handled by converting {@code self} to a {@link
     * org.enso.interpreter.runtime.data.text.Text} and dispatching natively.
     */
    CONVERT_TO_TEXT,
    /**
     * The method call should be handled by converting {@code self} to a {@code
     * Standard.Base.Data.Time.Date} and dispatching natively.
     */
    CONVERT_TO_DATE,
    /** The method call should be handled by dispatching through the {@code Any} type. */
    NOT_SUPPORTED;

    /**
     * Directly use {@link InteropLibrary}, or not. Types that return false are either {@link
     * #NOT_SUPPORTED unsupported} or require additional conversions like {@link #CONVERT_TO_TEXT}
     * and {@link #CONVERT_TO_DATE}.
     *
     * @return true if one can directly pass this object to {@link InteropLibrary}
     */
    public boolean isInteropLibrary() {
      return this != NOT_SUPPORTED && this != CONVERT_TO_TEXT && this != CONVERT_TO_DATE;
    }
  }

  private static final String ARRAY_LENGTH_NAME = "length";
  private static final String ARRAY_READ_NAME = "at";
  private static final String NEW_NAME = "new";

  static final int LIB_LIMIT = 3;

  /**
   * Returns a token instructing the caller about what mode of calling the given method should be
   * used.
   *
   * @param self the method call target
   * @param methodName the method name
   * @param library an instance of interop library to use for interacting with the target
   * @return a {@link PolyglotCallType} to use for this target and method
   */
  public static PolyglotCallType getPolyglotCallType(
      Object self, String methodName, InteropLibrary library) {
    if (library.isDate(self) && !library.isTime(self)) {
      return PolyglotCallType.CONVERT_TO_DATE;
    } else if (library.isString(self)) {
      return PolyglotCallType.CONVERT_TO_TEXT;
    } else if (library.isMemberInvocable(self, methodName)) {
      return PolyglotCallType.CALL_METHOD;
    } else if (library.isMemberReadable(self, methodName)) {
      return PolyglotCallType.GET_MEMBER;
    } else if (library.isInstantiable(self) && methodName.equals(NEW_NAME)) {
      return PolyglotCallType.INSTANTIATE;
    } else if (library.hasArrayElements(self) && methodName.equals(ARRAY_LENGTH_NAME)) {
      return PolyglotCallType.GET_ARRAY_LENGTH;
    } else if (library.hasArrayElements(self) && methodName.equals(ARRAY_READ_NAME)) {
      return PolyglotCallType.READ_ARRAY_ELEMENT;
    }
    return PolyglotCallType.NOT_SUPPORTED;
  }

  /**
   * Calls a method on an object, using a specified {@link PolyglotCallType}.
   *
   * @param callType the call type to perform
   * @param symbol the method name
   * @param self the call receiver
   * @param args the arguments
   * @return the result of calling the method on the receiver
   */
  public abstract Object execute(
      PolyglotCallType callType, String symbol, Object self, Object[] args);

  @Specialization(guards = {"callType == CALL_METHOD"})
  Object resolveHostMethod(
      PolyglotCallType callType,
      String symbol,
      Object self,
      Object[] args,
      @CachedLibrary(limit = "LIB_LIMIT") InteropLibrary members,
      @Cached HostValueToEnsoNode hostValueToEnsoNode) {
    try {
      return hostValueToEnsoNode.execute(members.invokeMember(self, symbol, args));
    } catch (UnsupportedMessageException | UnknownIdentifierException e) {
      throw new IllegalStateException(
          "Impossible to reach here. The member is checked to be invocable.");
    } catch (ArityException e) {
      throw new PanicException(
          Context.get(this)
              .getBuiltins()
              .error()
              .makeArityError(e.getExpectedMinArity(), e.getExpectedMaxArity(), e.getActualArity()),
          this);
    } catch (UnsupportedTypeException e) {
      throw new PanicException(
          Context.get(this)
              .getBuiltins()
              .error()
              .makeUnsupportedArgumentsError(e.getSuppliedValues()),
          this);
    }
  }

  @Specialization(guards = {"callType == GET_MEMBER"})
  Object resolveHostField(
      PolyglotCallType callType,
      String symbol,
      Object self,
      Object[] args,
      @CachedLibrary(limit = "LIB_LIMIT") InteropLibrary members,
      @Cached HostValueToEnsoNode hostValueToEnsoNode,
      @Cached BranchProfile errorProfile) {
    if (args.length != 0) {
      errorProfile.enter();
      throw new PanicException(
          Context.get(this).getBuiltins().error().makeArityError(0, 0, args.length), this);
    }
    try {
      return hostValueToEnsoNode.execute(members.readMember(self, symbol));
    } catch (UnsupportedMessageException | UnknownIdentifierException e) {
      throw new IllegalStateException(
          "Impossible to reach here. The member is checked to be readable.");
    }
  }

  @Specialization(guards = {"callType == INSTANTIATE"})
  Object resolveHostConstructor(
      PolyglotCallType callType,
      String symbol,
      Object self,
      Object[] args,
      @CachedLibrary(limit = "LIB_LIMIT") InteropLibrary instances,
      @Cached HostValueToEnsoNode hostValueToEnsoNode) {
    try {
      return hostValueToEnsoNode.execute(instances.instantiate(self, args));
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(
          "Impossible to reach here. The member is checked to be instantiable.");
    } catch (ArityException e) {
      throw new PanicException(
          Context.get(this)
              .getBuiltins()
              .error()
              .makeArityError(e.getExpectedMinArity(), e.getExpectedMaxArity(), e.getActualArity()),
          this);
    } catch (UnsupportedTypeException e) {
      throw new PanicException(
          Context.get(this)
              .getBuiltins()
              .error()
              .makeUnsupportedArgumentsError(e.getSuppliedValues()),
          this);
    }
  }

  @Specialization(guards = {"callType == GET_ARRAY_LENGTH"})
  Object resolveHostArrayLength(
      PolyglotCallType callType,
      String symbol,
      Object self,
      Object[] args,
      @CachedLibrary(limit = "LIB_LIMIT") InteropLibrary arrays,
      @Cached BranchProfile errorProfile,
      @Cached HostValueToEnsoNode hostValueToEnsoNode) {
    if (args.length != 0) {
      errorProfile.enter();
      throw new PanicException(
          Context.get(this).getBuiltins().error().makeArityError(0, 0, args.length), this);
    }
    try {
      return hostValueToEnsoNode.execute(arrays.getArraySize(self));
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible to reach here, self is checked to be an array");
    }
  }

  @Specialization(guards = {"callType == READ_ARRAY_ELEMENT"})
  Object resolveHostArrayRead(
      PolyglotCallType callType,
      String symbol,
      Object self,
      Object[] args,
      @CachedLibrary(limit = "LIB_LIMIT") InteropLibrary arrays,
      @Cached BranchProfile arityErrorProfile,
      @Cached BranchProfile typeErrorProfile,
      @Cached HostValueToEnsoNode hostValueToEnsoNode) {
    if (args.length != 1) {
      arityErrorProfile.enter();
      throw new PanicException(
          Context.get(this).getBuiltins().error().makeArityError(1, 1, args.length), this);
    }
    if (!(args[0] instanceof Long)) {
      typeErrorProfile.enter();
      throw new PanicException(
          Context.get(this).getBuiltins().error().makeInvalidArrayIndexError(self, args[0]), this);
    }
    long idx = (Long) args[0];
    try {
      return hostValueToEnsoNode.execute(arrays.readArrayElement(self, idx));
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible to reach here, self is checked to be an array");
    } catch (InvalidArrayIndexException e) {
      throw new PanicException(
          Context.get(this).getBuiltins().error().makeInvalidArrayIndexError(self, idx), this);
    }
  }
}
