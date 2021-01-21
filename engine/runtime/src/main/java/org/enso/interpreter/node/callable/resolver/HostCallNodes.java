package org.enso.interpreter.node.callable.resolver;

import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.PanicException;

public class HostCallNodes {

  public static final Object CALL_METHOD = new Object();
  public static final Object GET_MEMBER = new Object();
  public static final Object CALL_CONSTRUCTOR = new Object();
  public static final Object CALL_ARRAY_LENGTH = new Object();
  public static final Object READ_ARRAY_ELEMENT = new Object();
  public static final Object TRY_ANY = new Object();

  private static final String ARRAY_LENGTH_NAME = "length";
  private static final String ARRAY_READ_NAME = "at";
  private static final String NEW_NAME = "new";

  public static Object getPolyglotCallType(
      Object _this, String methodName, InteropLibrary library) {
    if (library.isMemberInvocable(_this, methodName)) {
      return CALL_METHOD;
    } else if (library.isMemberReadable(_this, methodName)) {
      return GET_MEMBER;
    } else if (library.isInstantiable(_this) && methodName.equals(NEW_NAME)) {
      return CALL_CONSTRUCTOR;
    } else if (library.hasArrayElements(_this) && methodName.equals(ARRAY_LENGTH_NAME)) {
      return CALL_ARRAY_LENGTH;
    } else if (library.hasArrayElements(_this) && methodName.equals(ARRAY_READ_NAME)) {
      return READ_ARRAY_ELEMENT;
    } else {
      return TRY_ANY;
    }
  }

  @GenerateUncached
  @ReportPolymorphism
  @ImportStatic(HostCallNodes.class)
  public abstract static class HostMethodCallNode extends Node {
    public abstract Object execute(Object callType, String symbol, Object _this, Object[] args);

    static final int LIB_LIMIT = 3;

    @Specialization(guards = {"callType == CALL_METHOD"})
    Object resolveHostMethod(
        Object callType,
        String symbol,
        Object _this,
        Object[] args,
        @CachedLibrary(limit = "LIB_LIMIT") InteropLibrary members,
        @CachedContext(Language.class) Context context,
        @Cached HostValueToEnsoNode hostValueToEnsoNode) {
      try {
        return hostValueToEnsoNode.execute(members.invokeMember(_this, symbol, args));
      } catch (UnsupportedMessageException | UnknownIdentifierException e) {
        throw new IllegalStateException(
            "Impossible to reach here. The member is checked to be invocable.");
      } catch (ArityException e) {
        throw new PanicException(
            context.getBuiltins().error().makeArityError(e.getExpectedArity(), e.getActualArity()),
            this);
      } catch (UnsupportedTypeException e) {
        throw new PanicException(
            context.getBuiltins().error().makeUnsupportedArgumentsError(e.getSuppliedValues()),
            this);
      }
    }

    @Specialization(guards = {"callType == GET_MEMBER"})
    Object resolveHostField(
        Object callType,
        String symbol,
        Object _this,
        Object[] args,
        @CachedLibrary(limit = "LIB_LIMIT") InteropLibrary members,
        @CachedContext(Language.class) Context context,
        @Cached HostValueToEnsoNode hostValueToEnsoNode) {
      try {
        return hostValueToEnsoNode.execute(members.readMember(_this, symbol));
      } catch (UnsupportedMessageException | UnknownIdentifierException e) {
        throw new IllegalStateException(
            "Impossible to reach here. The member is checked to be readable.");
      }
    }

    @Specialization(guards = {"callType == CALL_CONSTRUCTOR"})
    Object resolveHostConstructor(
        Object callType,
        String symbol,
        Object _this,
        Object[] args,
        @CachedLibrary(limit = "LIB_LIMIT") InteropLibrary instances,
        @CachedContext(Language.class) Context context,
        @Cached HostValueToEnsoNode hostValueToEnsoNode) {
      try {
        return hostValueToEnsoNode.execute(instances.instantiate(_this, args));
      } catch (UnsupportedMessageException e) {
        throw new IllegalStateException(
            "Impossible to reach here. The member is checked to be instantiable.");
      } catch (ArityException e) {
        throw new PanicException(
            context.getBuiltins().error().makeArityError(e.getExpectedArity(), e.getActualArity()),
            this);
      } catch (UnsupportedTypeException e) {
        throw new PanicException(
            context.getBuiltins().error().makeUnsupportedArgumentsError(e.getSuppliedValues()),
            this);
      }
    }

    @Specialization(guards = {"callType == CALL_ARRAY_LENGTH"})
    Object resolveHostArrayLength(
        Object callType,
        String symbol,
        Object _this,
        Object[] args,
        @CachedLibrary(limit = "LIB_LIMIT") InteropLibrary arrays,
        @CachedContext(Language.class) Context ctx,
        @Cached HostValueToEnsoNode hostValueToEnsoNode) {
      if (args.length != 0) {
        throw new PanicException(ctx.getBuiltins().error().makeArityError(0, args.length), this);
      }
      try {
        return hostValueToEnsoNode.execute(arrays.getArraySize(_this));
      } catch (UnsupportedMessageException e) {
        throw new IllegalStateException(
            "Impossible to reach here, _this is checked to be an array");
      }
    }

    @Specialization(guards = {"callType == READ_ARRAY_ELEMENT"})
    Object resolveHostArrayRead(
        Object callType,
        String symbol,
        Object _this,
        Object[] args,
        @CachedLibrary(limit = "LIB_LIMIT") InteropLibrary arrays,
        @CachedContext(Language.class) Context ctx,
        @Cached HostValueToEnsoNode hostValueToEnsoNode) {
      if (args.length != 1) {
        throw new PanicException(ctx.getBuiltins().error().makeArityError(1, args.length), this);
      }
      if (!(args[0] instanceof Long)) {
        throw new PanicException(
            ctx.getBuiltins().error().makeInvalidArrayIndexError(_this, args[0]), this);
      }
      long idx = (Long) args[0];
      try {
        return hostValueToEnsoNode.execute(arrays.readArrayElement(_this, idx));
      } catch (UnsupportedMessageException e) {
        throw new IllegalStateException(
            "Impossible to reach here, _this is checked to be an array");
      } catch (InvalidArrayIndexException e) {
        throw new PanicException(
            ctx.getBuiltins().error().makeInvalidArrayIndexError(_this, idx), this);
      }
    }
  }
}
