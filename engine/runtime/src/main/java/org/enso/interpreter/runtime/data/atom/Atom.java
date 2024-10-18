package org.enso.interpreter.runtime.data.atom;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.profiles.BranchProfile;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.type.TypesGen;
import org.enso.interpreter.runtime.warning.WarningsLibrary;

/**
 * A runtime representation of an Atom in Enso.
 *
 * <h2>{@link InteropLibrary Interop} protocol</h2>
 *
 * {@link InteropLibrary#getMembers(Object) Members} are fields and methods. Only fields of the atom
 * constructor used to construct this atom are considered members. If the constructor is
 * project-private, the fields are considered <emph>internal</emph> members. All methods (public and
 * project-private) are considered <emph>internal</emph> members.
 *
 * <p>Since all the members are methods, they are both {@link
 * InteropLibrary#isMemberReadable(Object, String) readable} and {@link
 * InteropLibrary#isMemberInvocable(Object, String) invocable}.
 *
 * <p>Trying to {@link InteropLibrary#invokeMember(Object, String, Object...) invoke}
 * project-private method results in {@link UnsupportedMessageException}.
 */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
public abstract class Atom implements EnsoObject {
  final AtomConstructor constructor;
  private Integer hashCode;

  /**
   * Creates a new Atom for a given constructor.
   *
   * @param constructor the Atom's constructor
   */
  Atom(AtomConstructor constructor) {
    this.constructor = constructor;
  }

  /**
   * Gets the Atom's constructor.
   *
   * @return the constructor for this Atom
   */
  public final AtomConstructor getConstructor() {
    return constructor;
  }

  public void setHashCode(int hashCode) {
    assert this.hashCode == null : "setHashCode must be called at most once";
    this.hashCode = hashCode;
  }

  public Integer getHashCode() {
    return hashCode;
  }

  @CompilerDirectives.TruffleBoundary
  private void toString(StringBuilder builder, boolean shouldParen, int depth) {
    if (depth <= 0) {
      builder.append("...");
      return;
    }
    boolean parensNeeded = shouldParen && constructor.getArity() > 0;
    if (parensNeeded) {
      builder.append("(");
    }
    builder.append(getConstructor().getName());
    for (var i = 0; i < constructor.getArity(); i++) {
      var obj = StructsLibrary.getUncached().getField(this, i);
      builder.append(" ");
      if (obj instanceof Atom atom) {
        atom.toString(builder, true, depth - 1);
      } else {
        builder.append(obj);
      }
    }
    if (parensNeeded) {
      builder.append(")");
    }
  }

  /**
   * Creates a textual representation of this Atom, useful for debugging.
   *
   * @return a textual representation of this Atom.
   */
  @Override
  public String toString() {
    return toString(null, 10, null, null);
  }

  @CompilerDirectives.TruffleBoundary
  private String toString(String prefix, int depth, String suffix, Object obj) {
    StringBuilder sb = new StringBuilder();
    if (prefix != null) {
      sb.append(prefix);
    }
    toString(sb, false, depth);
    if (suffix != null) {
      sb.append(suffix);
    }
    if (obj != null) {
      var errorMessage =
          switch (obj) {
            case Function fn -> fn.toString(false);
            default -> InteropLibrary.getUncached().toDisplayString(obj);
          };
      if (errorMessage != null) {
        sb.append(errorMessage);
      } else {
        sb.append("Nothing");
      }
    }
    return sb.toString();
  }

  @CompilerDirectives.TruffleBoundary
  private String toString(int depth) {
    StringBuilder sb = new StringBuilder();
    toString(sb, false, depth);
    return sb.toString();
  }

  @ExportMessage
  boolean hasMembers() {
    return true;
  }

  /**
   * Returns list of fields of the Atom. If {@code includeInternal} is true, all methods, including
   * project-private, are included. Fields are returned as filed getters, i.e., methods. Only fields
   * for the constructor that was used to construct this atom are returned.
   */
  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  EnsoObject getMembers(boolean includeInternal) {
    Set<Function> allMembers = new HashSet<>();
    allMembers.addAll(getInstanceMethods());

    if (includeInternal) {
      allMembers.addAll(getFieldGetters());
    } else {
      if (!hasProjectPrivateConstructor()) {
        allMembers.addAll(getFieldGetters());
      }
    }

    String[] filteredMembers =
        allMembers.stream()
            .filter(
                method -> {
                  if (includeInternal) {
                    return true;
                  } else {
                    return !method.getSchema().isProjectPrivate();
                  }
                })
            .map(
                func -> {
                  var funcNameItems = func.getName().split("\\.");
                  return funcNameItems[funcNameItems.length - 1];
                })
            .distinct()
            .toArray(String[]::new);
    return ArrayLikeHelpers.wrapStrings(filteredMembers);
  }

  /** Get all instance methods for this atom's type. */
  private Set<Function> getInstanceMethods() {
    var methodsFromCtorScope =
        constructor.getDefinitionScope().getMethodsForType(constructor.getType());
    var methodsFromTypeScope =
        constructor.getType().getDefinitionScope().getMethodsForType(constructor.getType());
    var allMethods = new HashSet<Function>();
    if (methodsFromCtorScope != null) {
      allMethods.addAll(methodsFromCtorScope);
    }
    if (methodsFromTypeScope != null) {
      allMethods.addAll(methodsFromTypeScope);
    }
    return allMethods.stream()
        .filter(method -> !isFieldGetter(method))
        .collect(Collectors.toUnmodifiableSet());
  }

  /** Get field getters for this atom's constructor. */
  private Set<Function> getFieldGetters() {
    var allMethods = constructor.getDefinitionScope().getMethodsForType(constructor.getType());
    if (allMethods != null) {
      return allMethods.stream()
          .filter(method -> isFieldGetter(method) && isGetterForOwnField(method))
          .collect(Collectors.toUnmodifiableSet());
    }
    return Set.of();
  }

  /**
   * Returns true if the given {@code function} is a getter for a field inside this atom
   * constructor.
   *
   * @param function the function to check.
   * @return true if the function is a getter for a field inside this atom constructor.
   */
  private boolean isGetterForOwnField(Function function) {
    if (function.getCallTarget() != null
        && function.getCallTarget().getRootNode() instanceof GetFieldBaseNode getFieldNode) {
      var fieldName = getFieldNode.getName();
      var thisConsFieldNames =
          Arrays.stream(constructor.getFields()).map(ArgumentDefinition::getName).toList();
      return thisConsFieldNames.contains(fieldName);
    }
    return false;
  }

  private boolean isFieldGetter(Function function) {
    return function.getCallTarget() != null
        && function.getCallTarget().getRootNode() instanceof GetFieldBaseNode;
  }

  /** A member is invocable if it is readable, i.e., if it is a field or a method. */
  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  final boolean isMemberInvocable(String member) {
    return isMemberReadable(member);
  }

  /** Readable members are fields of non-project-private constructors and public methods. */
  @ExportMessage
  @ExplodeLoop
  final boolean isMemberReadable(String member) {
    for (int i = 0; i < constructor.getArity(); i++) {
      if (member.equals(constructor.getFields()[i].getName())) {
        return true;
      }
    }
    var method = findMethod(member);
    return method != null;
  }

  /**
   * All methods are internal, including public methods. Fields of project-private constructor are
   * internal as well.
   */
  @ExportMessage
  final boolean isMemberInternal(String member) {
    if (hasProjectPrivateConstructor()) {
      return true;
    }
    for (int i = 0; i < constructor.getArity(); i++) {
      if (member.equals(constructor.getFields()[i].getName())) {
        // Fields of public constructor are not internal.
        return false;
      }
    }
    // All methods are internal.
    return true;
  }

  /**
   * Reads a field or a method.
   *
   * @param member An identifier of a field or method.
   * @return Value of the field or function.
   * @throws UnknownIdentifierException If an unknown field/method is requested.
   * @throws UnsupportedMessageException If the requested member is not readable.
   */
  @ExportMessage
  @ExplodeLoop
  final Object readMember(String member, @CachedLibrary(limit = "3") StructsLibrary structs)
      throws UnknownIdentifierException, UnsupportedMessageException {
    if (!isMemberReadable(member)) {
      throw UnknownIdentifierException.create(member);
    }
    for (int i = 0; i < constructor.getArity(); i++) {
      if (member.equals(constructor.getFields()[i].getName())) {
        return structs.getField(this, i);
      }
    }
    var method = findMethod(member);
    if (method != null) {
      return method;
    }
    throw UnknownIdentifierException.create(member);
  }

  @TruffleBoundary
  private Function findMethod(String methodName) {
    var matchedMethod =
        getInstanceMethods().stream()
            .filter(
                method -> {
                  var nameItems = method.getName().split("\\.");
                  return nameItems[nameItems.length - 1].equals(methodName);
                })
            .findFirst();
    return matchedMethod.orElse(null);
  }

  /**
   * All members - fields (field getters) and methods can be invoked. Including project-private
   * methods.
   */
  @ExportMessage
  static class InvokeMember {

    static UnresolvedSymbol buildSym(AtomConstructor cons, String name) {
      return UnresolvedSymbol.build(name, cons.getDefinitionScope());
    }

    @Specialization(
        guards = {
          "receiver.getConstructor() == cachedConstructor",
          "member.equals(cachedMember)",
        },
        limit = "3")
    static Object doCached(
        Atom receiver,
        String member,
        Object[] arguments,
        @Cached(value = "receiver.getConstructor()") AtomConstructor cachedConstructor,
        @Cached(value = "member") String cachedMember,
        @Cached(value = "buildSym(cachedConstructor, cachedMember)") UnresolvedSymbol cachedSym,
        @CachedLibrary("cachedSym") InteropLibrary symbols)
        throws UnsupportedMessageException,
            ArityException,
            UnsupportedTypeException,
            UnknownIdentifierException {
      Object[] args = new Object[arguments.length + 1];
      args[0] = receiver;
      System.arraycopy(arguments, 0, args, 1, arguments.length);
      try {
        return symbols.execute(cachedSym, args);
      } catch (PanicException ex) {
        if (ex.getCause() instanceof UnknownIdentifierException interopEx) {
          throw interopEx;
        } else {
          throw ex;
        }
      }
    }

    @Specialization(replaces = "doCached")
    static Object doUncached(
        Atom receiver,
        String member,
        Object[] arguments,
        @CachedLibrary(limit = "1") InteropLibrary symbols)
        throws UnsupportedMessageException,
            ArityException,
            UnsupportedTypeException,
            UnknownIdentifierException {
      UnresolvedSymbol symbol = buildSym(receiver.getConstructor(), member);
      return doCached(
          receiver, member, arguments, receiver.getConstructor(), member, symbol, symbols);
    }
  }

  @ExportMessage
  Text toDisplayString(
      boolean allowSideEffects,
      @CachedLibrary("this") InteropLibrary atoms,
      @CachedLibrary(limit = "3") WarningsLibrary warnings,
      @CachedLibrary(limit = "3") InteropLibrary interop,
      @Cached BranchProfile handleError) {
    Object result = null;
    String msg;
    try {
      result = atoms.invokeMember(this, "to_text");
      if (warnings.hasWarnings(result)) {
        result = warnings.removeWarnings(result);
      }
      if (TypesGen.isDataflowError(result)) {
        msg = this.toString("Error in method `to_text` of [", 10, "]: ", result);
      } else if (TypesGen.isText(result)) {
        return TypesGen.asText(result);
      } else if (interop.isString(result)) {
        return Text.create(interop.asString(result));
      } else {
        msg =
            this.toString(
                "Error in method `to_text` of [", 10, "]: Expected Text but got ", result);
      }
    } catch (AbstractTruffleException
        | UnsupportedMessageException
        | ArityException
        | UnknownIdentifierException
        | UnsupportedTypeException panic) {
      handleError.enter();
      msg = this.toString("Panic in method `to_text` of [", 10, "]: ", panic);
    }
    return Text.create(msg);
  }

  @ExportMessage
  Class<EnsoLanguage> getLanguage() {
    return EnsoLanguage.class;
  }

  @ExportMessage
  boolean hasLanguage() {
    return true;
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType() {
    return getConstructor().getType();
  }

  @ExportMessage
  Type getMetaObject() {
    return getType();
  }

  @ExportMessage
  boolean hasMetaObject() {
    return true;
  }

  private boolean hasProjectPrivateConstructor() {
    return constructor.getType().isProjectPrivate();
  }
}
