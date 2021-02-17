package org.enso.interpreter.runtime.scope;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.io.File;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.type.Types;
import org.enso.pkg.QualifiedName;
import org.enso.pkg.QualifiedName$;
import org.enso.polyglot.MethodNames;

/** Represents the top scope of Enso execution, containing all the importable modules. */
@ExportLibrary(InteropLibrary.class)
public class TopLevelScope implements TruffleObject {
  private final Builtins builtins;
  private final Map<String, Module> modules;

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

  /** @return the list of modules in the scope. */
  public Collection<Module> getModules() {
    return modules.values();
  }

  /**
   * Looks up a module by name.
   *
   * @param name the name of the module to look up.
   * @return empty result if the module does not exist or the requested module.
   */
  public Optional<Module> getModule(String name) {
    if (name.equals(Builtins.MODULE_NAME)) {
      return Optional.of(builtins.getModule());
    }
    return Optional.ofNullable(modules.get(name));
  }

  /**
   * Creates and registers a new module with given name and source file.
   *
   * @param name the module name.
   * @param sourceFile the module source file.
   * @return the newly created module.
   */
  public Module createModule(QualifiedName name, TruffleFile sourceFile) {
    Module module = new Module(name, sourceFile);
    modules.put(name.toString(), module);
    return module;
  }

  /**
   * Renames a project part of the included modules.
   *
   * @param oldName the old project name
   * @param newName the new project name
   */
  public void renameProjectInModules(String oldName, String newName) {
    String separator = QualifiedName$.MODULE$.separator();
    List<String> keys =
        modules.keySet().stream()
            .filter(name -> name.startsWith(oldName + separator))
            .collect(Collectors.toList());

    keys.stream()
        .map(modules::remove)
        .forEach(
            module -> {
              module.renameProject(newName);
              modules.put(module.getName().toString(), module);
            });
  }

  /**
   * Returns the builtins module.
   *
   * @return the builtins module.
   */
  public Builtins getBuiltins() {
    return builtins;
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
  Array getMembers(boolean includeInternal) {
    return new Array(
        MethodNames.TopScope.GET_MODULE,
        MethodNames.TopScope.CREATE_MODULE,
        MethodNames.TopScope.REGISTER_MODULE,
        MethodNames.TopScope.UNREGISTER_MODULE);
  }

  /** Handles member invocation through the polyglot API. */
  @ExportMessage
  abstract static class InvokeMember {
    private static Module getModule(
        TopLevelScope scope,
        Object[] arguments,
        TruffleLanguage.ContextReference<Context> contextReference)
        throws ArityException, UnsupportedTypeException, UnknownIdentifierException {
      String moduleName = Types.extractArguments(arguments, String.class);

      if (moduleName.equals(Builtins.MODULE_NAME)) {
        return scope.builtins.getModule();
      }
      Module module = scope.modules.get(moduleName);
      if (module == null) {
        throw UnknownIdentifierException.create(moduleName);
      }

      return module;
    }

    private static Module createModule(TopLevelScope scope, Object[] arguments, Context context)
        throws ArityException, UnsupportedTypeException {
      String moduleName = Types.extractArguments(arguments, String.class);
      return Module.empty(QualifiedName.simpleName(moduleName));
    }

    private static Module registerModule(TopLevelScope scope, Object[] arguments, Context context)
        throws ArityException, UnsupportedTypeException {
      Types.Pair<String, String> args =
          Types.extractArguments(arguments, String.class, String.class);
      QualifiedName qualName = QualifiedName.fromString(args.getFirst());
      File location = new File(args.getSecond());
      Module module = new Module(qualName, context.getTruffleFile(location));
      scope.modules.put(qualName.toString(), module);
      return module;
    }

    private static Object unregisterModule(TopLevelScope scope, Object[] arguments, Context context)
        throws ArityException, UnsupportedTypeException {
      String name = Types.extractArguments(arguments, String.class);
      scope.modules.remove(name);
      return context.getNothing().newInstance();
    }

    private static Object leakContext(Context context) {
      return context.getEnvironment().asGuestValue(context);
    }

    @Specialization
    static Object doInvoke(
        TopLevelScope scope,
        String member,
        Object[] arguments,
        @CachedContext(Language.class) TruffleLanguage.ContextReference<Context> contextRef)
        throws UnknownIdentifierException, ArityException, UnsupportedTypeException {
      switch (member) {
        case MethodNames.TopScope.GET_MODULE:
          return getModule(scope, arguments, contextRef);
        case MethodNames.TopScope.CREATE_MODULE:
          return createModule(scope, arguments, contextRef.get());
        case MethodNames.TopScope.REGISTER_MODULE:
          return registerModule(scope, arguments, contextRef.get());
        case MethodNames.TopScope.UNREGISTER_MODULE:
          return unregisterModule(scope, arguments, contextRef.get());
        case MethodNames.TopScope.LEAK_CONTEXT:
          return leakContext(contextRef.get());
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
    return member.equals(MethodNames.TopScope.GET_MODULE)
        || member.equals(MethodNames.TopScope.CREATE_MODULE)
        || member.equals(MethodNames.TopScope.REGISTER_MODULE)
        || member.equals(MethodNames.TopScope.UNREGISTER_MODULE);
  }

  /**
   * Checks if the receiver is a scope object.
   *
   * @return {@code true}
   */
  @ExportMessage
  boolean isScope() {
    return true;
  }

  /**
   * Returns true if this scope has an enclosing parent scope, else false.
   *
   * @return {@code false}
   */
  @ExportMessage
  boolean hasScopeParent() {
    return false;
  }

  /**
   * Returns the parent scope of this scope, if one exists.
   *
   * @return {@code null}
   * @throws UnsupportedMessageException always, as this scope can never have a parent
   */
  @ExportMessage
  Object getScopeParent() throws UnsupportedMessageException {
    return UnsupportedMessageException.create();
  }

  /**
   * Checks if this value is associated with a language.
   *
   * @return {@code true}
   */
  @ExportMessage
  final boolean hasLanguage() {
    return true;
  }

  /**
   * Returns the language associated with this scope value.
   *
   * @return the language with which this value is associated
   */
  @ExportMessage
  final Class<Language> getLanguage() {
    return Language.class;
  }

  /**
   * Converts this scope to a human readable string.
   *
   * @param allowSideEffects whether or not side effects are allowed
   * @return a string representation of this scope
   */
  @ExportMessage
  final Object toDisplayString(boolean allowSideEffects) {
    return "Enso.Top_Scope";
  }
}
