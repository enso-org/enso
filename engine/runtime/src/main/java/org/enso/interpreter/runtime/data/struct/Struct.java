package org.enso.interpreter.runtime.data.struct;


import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.ExportLibrary;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

/** A runtime representation of an Atom in Enso. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
public abstract class Struct implements TruffleObject {
  final AtomConstructor constructor;

  /**
   * Creates a new Atom for a given constructor.
   *
   * @param constructor the Atom's constructor
   */
  public Struct(AtomConstructor constructor) {
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

  public final Object[] getFields() {
    return StructsLibrary.getUncached().getFields(this);
  }

//
//  /**
//   * Gets the fields from the Atom.
//   *
//   * @return this Atom's fields
//   */
//  public Object[] getFields() {
//    return fields;
//  }
//  private void toString(StringBuilder builder, boolean shouldParen, int depth) {
//    if (depth <= 0) {
//      builder.append("...");
//      return;
//    }
//    boolean parensNeeded = shouldParen && fields.length > 0;
//    if (parensNeeded) {
//      builder.append("(");
//    }
//    builder.append(getConstructor().getName());
//    for (var obj : fields) {
//        builder.append(" ");
//        if (obj instanceof Atom atom) {
//          atom.toString(builder, true, depth - 1);
//        } else {
//          builder.append(obj);
//        }
//    }
//    if (parensNeeded) {
//      builder.append(")");
//    }
//  }
//
//  /**
//   * Creates a textual representation of this Atom, useful for debugging.
//   *
//   * @return a textual representation of this Atom.
//   */
//  @Override
//  public String toString() {
//    return toString(10);
//  }
//
//  @CompilerDirectives.TruffleBoundary
//  private String toString(int depth) {
//    StringBuilder sb = new StringBuilder();
//    toString(sb, false, depth);
//    return sb.toString();
//  }
//
//  @ExportMessage
//  public boolean hasMembers() {
//    return true;
//  }
//
//  @ExportMessage
//  @CompilerDirectives.TruffleBoundary
//  public Array getMembers(boolean includeInternal) {
//    Map<String, Function> members = constructor.getDefinitionScope().getMethods().get(constructor.getType());
//    if (members == null) {
//      return new Array(0);
//    }
//    Object[] mems = members.keySet().toArray();
//    return new Array(mems);
//  }
//
//  @ExportMessage
//  @CompilerDirectives.TruffleBoundary
//  public boolean isMemberInvocable(String member) {
//    Map<String, ?> members = constructor.getDefinitionScope().getMethods().get(constructor.getType());
//    return members != null && members.containsKey(member);
//  }
//
//  @ExportMessage
//  @ExplodeLoop
//  public boolean isMemberReadable(String member) {
//    for (int i = 0; i < constructor.getArity(); i++) {
//      if (member.equals(constructor.getFields()[i].getName())) {
//        return true;
//      }
//    }
//    return false;
//  }
//
//  @ExportMessage
//  @ExplodeLoop
//  public Object readMember(String member) throws UnknownIdentifierException {
//    for (int i = 0; i < constructor.getArity(); i++) {
//      if (member.equals(constructor.getFields()[i].getName())) {
//        return fields[i];
//      }
//    }
//    throw UnknownIdentifierException.create(member);
//  }
//
//  @ExportMessage
//  static class InvokeMember {
//
//    static UnresolvedSymbol buildSym(AtomConstructor cons, String name) {
//      return UnresolvedSymbol.build(name, cons.getDefinitionScope());
//    }
//
//    @Specialization(
//        guards = {"receiver.getConstructor() == cachedConstructor", "member.equals(cachedMember)"})
//    static Object doCached(
//        Atom receiver,
//        String member,
//        Object[] arguments,
//        @Cached(value = "receiver.getConstructor()") AtomConstructor cachedConstructor,
//        @Cached(value = "member") String cachedMember,
//        @Cached(value = "buildSym(cachedConstructor, cachedMember)") UnresolvedSymbol cachedSym,
//        @CachedLibrary("cachedSym") InteropLibrary symbols)
//        throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
//      Object[] args = new Object[arguments.length + 1];
//      args[0] = receiver;
//      System.arraycopy(arguments, 0, args, 1, arguments.length);
//      return symbols.execute(cachedSym, args);
//    }
//
//    @Specialization(replaces = "doCached")
//    static Object doUncached(
//        Atom receiver,
//        String member,
//        Object[] arguments,
//        @CachedLibrary(limit = "1") InteropLibrary symbols)
//        throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
//      UnresolvedSymbol symbol = buildSym(receiver.getConstructor(), member);
//      return doCached(
//          receiver, member, arguments, receiver.getConstructor(), member, symbol, symbols);
//    }
//  }
//
//  @ExportMessage
//  Text toDisplayString(boolean allowSideEffects, @CachedLibrary("this") InteropLibrary atoms) {
//    try {
//      return TypesGen.expectText(atoms.invokeMember(this, "to_text"));
//    } catch (UnsupportedMessageException
//        | ArityException
//        | UnknownIdentifierException
//        | UnsupportedTypeException
//        | UnexpectedResultException e) {
//      return Text.create(this.toString(10));
//    }
//  }
//
//  @ExportMessage
//  boolean hasType() {
//    return true;
//  }
//
//  @ExportMessage
//  Type getType() {
//    return getConstructor().getType();
//  }
}
