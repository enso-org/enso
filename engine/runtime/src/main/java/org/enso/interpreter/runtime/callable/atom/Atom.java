package org.enso.interpreter.runtime.callable.atom;

import java.time.LocalDate;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;
import org.enso.interpreter.runtime.type.TypesGen;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/** A runtime representation of an Atom in Enso. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(MethodDispatchLibrary.class)
public class Atom implements TruffleObject {
  final AtomConstructor constructor;
  private final Object[] fields;

  /**
   * Creates a new Atom for a given constructor.
   *
   * @param constructor the Atom's constructor
   * @param fields the Atom's fields
   */
  public Atom(AtomConstructor constructor, Object... fields) {
    this.constructor = constructor;
    this.fields = fields;
  }

  /**
   * Gets the Atom's constructor.
   *
   * @return the constructor for this Atom
   */
  public AtomConstructor getConstructor() {
    return constructor;
  }

  /**
   * Gets the fields from the Atom.
   *
   * @return this Atom's fields
   */
  public Object[] getFields() {
    return fields;
  }

  private String toString(boolean shouldParen) {
    StringBuilder builder = new StringBuilder();
    boolean parensNeeded = shouldParen && fields.length > 0;
    if (parensNeeded) {
      builder.append("(");
    }
    builder.append(getConstructor().getName());
    if (fields.length > 0) {
      builder.append(" ");
    }
    List<String> fieldStrings =
        Arrays.stream(fields)
            .map(
                obj -> {
                  if (obj instanceof Atom) {
                    return ((Atom) obj).toString(true);
                  } else {
                    return obj.toString();
                  }
                })
            .collect(Collectors.toList());
    builder.append(String.join(" ", fieldStrings));
    if (parensNeeded) {
      builder.append(")");
    }
    return builder.toString();
  }

  /**
   * Creates a textual representation of this Atom, useful for debugging.
   *
   * @return a textual representation of this Atom.
   */
  @Override
  @CompilerDirectives.TruffleBoundary
  public String toString() {
    return toString(false);
  }

  @ExportMessage
  public boolean hasMembers() {
    return true;
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  public Array getMembers(boolean includeInternal) {
    Map<String, Function> members = constructor.getDefinitionScope().getMethods().get(constructor);
    if (members == null) {
      return new Array(0);
    }
    Object[] mems = members.keySet().toArray();
    return new Array(mems);
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  public boolean isMemberInvocable(String member) {
    Map<String, ?> members = constructor.getDefinitionScope().getMethods().get(constructor);
    return members != null && members.containsKey(member);
  }

  @ExportMessage
  @ExplodeLoop
  public boolean isMemberReadable(String member) {
    for (int i = 0; i < constructor.getArity(); i++) {
      if (member.equals(constructor.getFields()[i].getName())) {
        return true;
      }
    }
    return false;
  }

  @ExportMessage
  @ExplodeLoop
  public Object readMember(String member) throws UnknownIdentifierException {
    for (int i = 0; i < constructor.getArity(); i++) {
      if (member.equals(constructor.getFields()[i].getName())) {
        return fields[i];
      }
    }
    throw UnknownIdentifierException.create(member);
  }

  @ExportMessage
  static class InvokeMember {

    static UnresolvedSymbol buildSym(AtomConstructor cons, String name) {
      return UnresolvedSymbol.build(name, cons.getDefinitionScope());
    }

    @Specialization(
        guards = {"receiver.getConstructor() == cachedConstructor", "member.equals(cachedMember)"})
    static Object doCached(
        Atom receiver,
        String member,
        Object[] arguments,
        @Cached(value = "receiver.getConstructor()") AtomConstructor cachedConstructor,
        @Cached(value = "member") String cachedMember,
        @Cached(value = "buildSym(cachedConstructor, cachedMember)") UnresolvedSymbol cachedSym,
        @CachedLibrary("cachedSym") InteropLibrary symbols)
        throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
      Object[] args = new Object[arguments.length + 1];
      args[0] = receiver;
      System.arraycopy(arguments, 0, args, 1, arguments.length);
      return symbols.execute(cachedSym, args);
    }

    @Specialization(replaces = "doCached")
    static Object doUncached(
        Atom receiver,
        String member,
        Object[] arguments,
        @CachedLibrary(limit = "1") InteropLibrary symbols)
        throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
      UnresolvedSymbol symbol = buildSym(receiver.getConstructor(), member);
      return doCached(
          receiver, member, arguments, receiver.getConstructor(), member, symbol, symbols);
    }
  }

  @ExportMessage
  Text toDisplayString(boolean allowSideEffects, @CachedLibrary("this") InteropLibrary atoms) {
    try {
      return TypesGen.expectText(atoms.invokeMember(this, "to_text"));
    } catch (UnsupportedMessageException
        | ArityException
        | UnknownIdentifierException
        | UnsupportedTypeException
        | UnexpectedResultException e) {
      return Text.create(this.toString());
    }
  }

  @ExportMessage
  boolean isNull() {
    return this.getConstructor() == Context.get(null).getBuiltins().nothing();
  }

  @ExportMessage
  boolean hasFunctionalDispatch() {
    return true;
  }

  @ExportMessage
  boolean isDate(@CachedLibrary("this") InteropLibrary iop) {
    var dateConstructor = Context.get(iop).getDateConstructor();
    if (dateConstructor.isPresent()) {
      return dateConstructor.get() == this.constructor;
    } else {
      return false;
    }
  }

  @ExportMessage
  LocalDate asDate(@CachedLibrary(limit = "3") InteropLibrary iop) throws UnsupportedMessageException {
    return iop.asDate(fields[0]);
  }

  @ExportMessage
  static class GetFunctionalDispatch {
    static final int CACHE_SIZE = 10;

    @CompilerDirectives.TruffleBoundary
    static Function doResolve(AtomConstructor cons, UnresolvedSymbol symbol) {
      return symbol.resolveFor(cons, getContext().getBuiltins().any());
    }

    static Context getContext() {
      return Context.get(null);
    }

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedSymbol == symbol",
          "_this.constructor == cachedConstructor",
          "function != null"
        },
        limit = "CACHE_SIZE")
    static Function resolveCached(
        Atom _this,
        UnresolvedSymbol symbol,
        @Cached("symbol") UnresolvedSymbol cachedSymbol,
        @Cached("_this.constructor") AtomConstructor cachedConstructor,
        @Cached("doResolve(cachedConstructor, cachedSymbol)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(
        Atom _this, UnresolvedSymbol symbol)
        throws MethodDispatchLibrary.NoSuchMethodException {
      Function function = doResolve(_this.constructor, symbol);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchMethodException();
      }
      return function;
    }
  }

  @ExportMessage
  boolean canConvertFrom() {
    return true;
  }

  @ExportMessage
  static class GetConversionFunction {

    static final int CACHE_SIZE = 10;

    @CompilerDirectives.TruffleBoundary
    static Function doResolve(
        AtomConstructor cons,
        AtomConstructor target,
        UnresolvedConversion conversion) {
      return conversion.resolveFor(target, cons, getContext().getBuiltins().any());
    }

    static Context getContext() {
      return Context.get(null);
    }

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedConversion == conversion",
          "cachedTarget == target",
          "_this.constructor == cachedConstructor",
          "function != null"
        },
        limit = "CACHE_SIZE")
    static Function resolveCached(
        Atom _this,
        AtomConstructor target,
        UnresolvedConversion conversion,
        @Cached("conversion") UnresolvedConversion cachedConversion,
        @Cached("_this.constructor") AtomConstructor cachedConstructor,
        @Cached("target") AtomConstructor cachedTarget,
        @Cached("doResolve(cachedConstructor, cachedTarget, cachedConversion)")
            Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(
        Atom _this,
        AtomConstructor target,
        UnresolvedConversion conversion)
        throws MethodDispatchLibrary.NoSuchConversionException {
      Function function = doResolve(_this.constructor, target, conversion);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchConversionException();
      }
      return function;
    }
  }
}
