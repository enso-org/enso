package org.enso.interpreter.runtime.scope;

import com.oracle.truffle.api.Scope;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.Builtins;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.data.Vector;

import java.util.Map;
import java.util.Optional;

/** Represents the top scope of Enso execution, containing all the importable modules. */
@ExportLibrary(InteropLibrary.class)
public class TopLevelScope implements TruffleObject {
  private final Builtins builtins;
  private final Map<String, Module> modules;
  private final Scope scope = Scope.newBuilder("top_scope", this).build();

  /**
   * Creates a new instance of top scope.
   *
   * @param builtins the automatically-imported builtin module.
   * @param modules the initial modules this scope contains.
   */
  public TopLevelScope(Builtins builtins, Map<String, Module> modules) {
    this.builtins = builtins;
    this.modules = modules;
  }

  /**
   * Returns a polyglot representation of this scope.
   *
   * @return a polyglot Scope object.
   */
  public Scope getScope() {
    return scope;
  }

  /**
   * Looks up a module by name.
   *
   * @param name the name of the module to look up.
   * @return empty result if the module does not exist or the requested module.
   */
  public Optional<Module> getModule(String name) {
    return Optional.ofNullable(modules.get(name));
  }

  /**
   * Returns the builtins module.
   *
   * @return the builtins module.
   */
  public Builtins getBuiltins() {
    return builtins;
  }

  private static class PolyglotKeys {
    private static final String GET_MODULE = "get_module";
    private static final String CREATE_MODULE = "create_module";
  }

  /**
   * Marks this object as having members accessible through the polyglot API.
   *
   * @return {@code true}
   */
  @ExportMessage
  boolean hasMembers() {
    return true;
  }

  /**
   * Returns a collection of all the members of this scope.
   *
   * <p>The exported members are the {@code get_module} and {@code create_module} methods.
   *
   * @param includeInternal ignored.
   * @return a collection of all the exported members.
   */
  @ExportMessage
  Vector getMembers(boolean includeInternal) {
    return new Vector(PolyglotKeys.GET_MODULE, PolyglotKeys.CREATE_MODULE);
  }

  /** Handles member invocation through the polyglot API. */
  @ExportMessage
  abstract static class InvokeMember {
    private static ModuleScope getModule(
        TopLevelScope scope,
        Object[] arguments,
        TruffleLanguage.ContextReference<Context> contextReference)
        throws ArityException, UnsupportedTypeException, UnknownIdentifierException {
      if (arguments.length != 1) {
        throw ArityException.create(1, arguments.length);
      }
      if (!(arguments[0] instanceof String)) {
        throw UnsupportedTypeException.create(arguments, "Argument must be a String");
      }
      String moduleName = (String) arguments[0];

      if (moduleName.equals(Builtins.MODULE_NAME)) {
        return scope.builtins.getScope();
      }
      Module module = scope.modules.get(moduleName);
      if (module == null) {
        throw UnknownIdentifierException.create(moduleName);
      }
      if (module.hasComputedScope()) {
        return module.getScope();
      } else {
        return module.requestParse(contextReference.get());
      }
    }

    private static ModuleScope createModule(
        TopLevelScope scope, Object[] arguments, Context context)
        throws ArityException, UnsupportedTypeException {
      if (arguments.length != 1) {
        throw ArityException.create(1, arguments.length);
      }
      if (!(arguments[0] instanceof String)) {
        throw UnsupportedTypeException.create(arguments, "Argument must be a String");
      }
      String moduleName = (String) arguments[0];
      return context.createScope(moduleName);
    }

    @Specialization
    static ModuleScope doInvoke(
        TopLevelScope scope,
        String member,
        Object[] arguments,
        @CachedContext(Language.class) TruffleLanguage.ContextReference<Context> contextRef)
        throws UnknownIdentifierException, ArityException, UnsupportedTypeException {
      switch (member) {
        case PolyglotKeys.GET_MODULE:
          return getModule(scope, arguments, contextRef);
        case PolyglotKeys.CREATE_MODULE:
          return createModule(scope, arguments, contextRef.get());
        default:
          throw UnknownIdentifierException.create(member);
      }
    }
  }

  /**
   * Checks if a member can be invoked through the polyglot API.
   *
   * @param member the member name.
   * @return {@code true} if the member is invocable, {@code false} otherwise.
   */
  @ExportMessage
  boolean isMemberInvocable(String member) {
    return member.equals(PolyglotKeys.GET_MODULE)
        || member.equals(PolyglotKeys.CREATE_MODULE);
  }
}
