package org.enso.interpreter.node.callable.resolver;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
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
    /**
     * The method call should be handled by converting {@code self} to a {@link
     * org.enso.interpreter.runtime.data.text.Text} and dispatching natively.
     */
    CONVERT_TO_TEXT,
    /**
     * The method call should be handled by converting {@code self} dispatching natively to methods
     * of {@link org.enso.interpreter.runtime.data.Array}
     */
    CONVERT_TO_ARRAY,
    /**
     * The method call should be handled by converting {@code self} to a {@code
     * Standard.Base.Data.Time.Date} and dispatching natively.
     */
    CONVERT_TO_DATE,
    /**
     * The method call should be handled by converting {@code self} to a {@code
     * Standard.Base.Data.Time.Date_Time} and dispatching natively.
     */
    CONVERT_TO_ZONED_DATE_TIME,
    /**
     * The method call should be handled by converting {@code self} to a {@code
     * Standard.Base.Data.Time.Date_Time} with a system Time_Zone and dispatching natively.
     */
    CONVERT_TO_DATE_TIME,
    /**
     * The method call should be handled by converting {@code self} to a {@code
     * Standard.Base.Data.Time.Duration} and dispatching natively.
     */
    CONVERT_TO_DURATION,
    /**
     * The method call should be handled by converting {@code self} to a {@code
     * Standard.Base.Data.Time.Time_Of_Day} and dispatching natively.
     */
    CONVERT_TO_TIME_OF_DAY,
    /**
     * The method call should be handled by converting {@code self} to a {@code
     * Standard.Base.Data.Time.Time_Zone} and dispatching natively.
     */
    CONVERT_TO_TIME_ZONE,
    /**
     * The method call should be handled by converting {@code self} to a {@code
     * Standard.Base.Data.Map} and dispatching natively.
     */
    CONVERT_TO_HASH_MAP,
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
      return this != NOT_SUPPORTED
          && this != CONVERT_TO_ARRAY
          && this != CONVERT_TO_TEXT
          && this != CONVERT_TO_DATE
          && this != CONVERT_TO_DATE_TIME
          && this != CONVERT_TO_DURATION
          && this != CONVERT_TO_ZONED_DATE_TIME
          && this != CONVERT_TO_TIME_OF_DAY
          && this != CONVERT_TO_TIME_ZONE
          && this != CONVERT_TO_HASH_MAP;
    }
  }

  private static final String NEW_NAME = "new";

  static final int LIB_LIMIT = 3;

  /**
   * Returns a token instructing the caller about what mode of calling the given method should be
   * used.
   *
   * @param self the method call target
   * @param symbol symbol representing method to be resolved
   * @param library an instance of interop library to use for interacting with the target
   * @return a {@link PolyglotCallType} to use for this target and method
   */
  public static PolyglotCallType getPolyglotCallType(
      Object self, UnresolvedSymbol symbol, InteropLibrary library) {
    return getPolyglotCallType(self, symbol, library, null);
  }

  /**
   * Returns a token instructing the caller about what mode of calling the given method should be
   * used.
   *
   * @param self the method call target
   * @param symbol symbol representing method to be resolved
   * @param library an instance of interop library to use for interacting with the target
   * @param methodResolverNode {@code null} or real instances of the node to resolve methods
   * @return a {@link PolyglotCallType} to use for this target and method
   */
  public static PolyglotCallType getPolyglotCallType(
      Object self,
      UnresolvedSymbol symbol,
      InteropLibrary library,
      MethodResolverNode methodResolverNode) {
    if (library.isDate(self)) {
      if (library.isTime(self)) {
        if (library.isTimeZone(self)) {
          return PolyglotCallType.CONVERT_TO_ZONED_DATE_TIME;
        } else {
          return PolyglotCallType.CONVERT_TO_DATE_TIME;
        }
      } else {
        return PolyglotCallType.CONVERT_TO_DATE;
      }
    } else if (library.isTime(self)) {
      return PolyglotCallType.CONVERT_TO_TIME_OF_DAY;
    } else if (library.isDuration(self)) {
      return PolyglotCallType.CONVERT_TO_DURATION;
    } else if (library.isTimeZone(self)) {
      return PolyglotCallType.CONVERT_TO_TIME_ZONE;
    } else if (library.isString(self)) {
      return PolyglotCallType.CONVERT_TO_TEXT;
    } else if (library.hasArrayElements(self)) {
      if (methodResolverNode != null) {
        var ctx = EnsoContext.get(library);
        var arrayType = ctx.getBuiltins().array();
        var fn = methodResolverNode.execute(arrayType, symbol);
        if (fn != null) {
          return PolyglotCallType.CONVERT_TO_ARRAY;
        }
      }
    } else if (library.hasHashEntries(self)) {
      return PolyglotCallType.CONVERT_TO_HASH_MAP;
    }

    String methodName = symbol.getName();
    if (library.isMemberInvocable(self, methodName)) {
      return PolyglotCallType.CALL_METHOD;
    } else if (library.isMemberReadable(self, methodName)) {
      return PolyglotCallType.GET_MEMBER;
    } else if (library.isInstantiable(self) && methodName.equals(NEW_NAME)) {
      return PolyglotCallType.INSTANTIATE;
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
          EnsoContext.get(this)
              .getBuiltins()
              .error()
              .makeArityError(e.getExpectedMinArity(), e.getExpectedMaxArity(), e.getActualArity()),
          this);
    } catch (UnsupportedTypeException e) {
      throw new PanicException(
          EnsoContext.get(this)
              .getBuiltins()
              .error()
              .makeUnsupportedArgumentsError(e.getSuppliedValues(), e.getMessage()),
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
          EnsoContext.get(this).getBuiltins().error().makeArityError(0, 0, args.length), this);
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
          EnsoContext.get(this)
              .getBuiltins()
              .error()
              .makeArityError(e.getExpectedMinArity(), e.getExpectedMaxArity(), e.getActualArity()),
          this);
    } catch (UnsupportedTypeException e) {
      throw new PanicException(
          EnsoContext.get(this)
              .getBuiltins()
              .error()
              .makeUnsupportedArgumentsError(e.getSuppliedValues(), e.getMessage()),
          this);
    }
  }
}
