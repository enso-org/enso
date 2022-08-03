package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.node.expression.atom.ConstantNode;
import org.enso.interpreter.node.expression.atom.GetFieldNode;
import org.enso.interpreter.node.expression.atom.QualifiedAccessorNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.pkg.QualifiedName;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

@ExportLibrary(MethodDispatchLibrary.class)
@ExportLibrary(InteropLibrary.class)
public class Type implements TruffleObject {
  private final String name;
  private @CompilerDirectives.CompilationFinal ModuleScope definitionScope;
  private final boolean builtin;
  private final Type supertype;
  private boolean gettersGenerated;

  public Type(String name, ModuleScope definitionScope, Type supertype, boolean builtin) {
    this.name = name;
    this.definitionScope = definitionScope;
    this.supertype = supertype;
    this.builtin = builtin;
    generateQualifiedAccessor();
  }

  private void generateQualifiedAccessor() {
    var node = new ConstantNode(null, this);
    var callTarget = Truffle.getRuntime().createCallTarget(node);
    var function =
        new Function(
            callTarget,
            null,
            new FunctionSchema(
                new ArgumentDefinition(0, "this", ArgumentDefinition.ExecutionMode.EXECUTE)));
    definitionScope.registerMethod(
        definitionScope.getAssociatedType(),
        this.name.toLowerCase(),
        function); // TODO lowercase remove when merge
  }

  public QualifiedName getQualifiedName() {
    if (this == this.getDefinitionScope().getAssociatedType()) {
      return definitionScope.getModule().getName();
    } else {
      return definitionScope.getModule().getName().createChild(getName());
    }
  }

  public void setShadowDefinitions(ModuleScope scope) {
    if (builtin) {
      // Ensure that synthetic methods, such as getters for fields are in the scope
      // Some scopes won't have any methods at this point, e.g., Nil or Nothing, hence the null
      // check.
      CompilerAsserts.neverPartOfCompilation();
      Map<String, Function> methods = this.definitionScope.getMethods().get(this);
      if (methods != null) {
        methods.forEach((name, fun) -> scope.registerMethod(this, name, fun));
      }
      this.definitionScope = scope;
    } else {
      throw new RuntimeException(
          "Attempting to modify scope of a non-builtin type post-construction is not allowed");
    }
  }

  public String getName() {
    return name;
  }

  public ModuleScope getDefinitionScope() {
    return definitionScope;
  }

  public boolean isBuiltin() {
    return builtin;
  }

  public Type getSupertype() {
    return supertype;
  }

  public void generateGetters(List<AtomConstructor> constructors) {
    if (gettersGenerated) return;
    if (constructors.size() != 1) return; // TODO
    var cons = constructors.get(0);
    Arrays.stream(cons.getFields())
        .forEach(
            field -> {
              GetFieldNode node = new GetFieldNode(null, field.getPosition());
              RootCallTarget callTarget = Truffle.getRuntime().createCallTarget(node);
              var f =
                  new Function(
                      callTarget,
                      null,
                      new FunctionSchema(
                          new ArgumentDefinition(
                              0, "this", ArgumentDefinition.ExecutionMode.EXECUTE)));
              definitionScope.registerMethod(this, field.getName(), f);
            });
  }

  @ExportMessage
  boolean hasFunctionalDispatch() {
    return true;
  }

  @ExportMessage
  static class GetFunctionalDispatch {
    static final int CACHE_SIZE = 10;

    @CompilerDirectives.TruffleBoundary
    static Function doResolve(Type type, UnresolvedSymbol symbol) {
      return symbol.resolveFor(type);
    }

    static Context getContext() {
      return Context.get(null);
    }

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedSymbol == symbol",
          "self == cachedType",
          "function != null"
        },
        limit = "CACHE_SIZE")
    static Function resolveCached(
        Type self,
        UnresolvedSymbol symbol,
        @Cached("symbol") UnresolvedSymbol cachedSymbol,
        @Cached("self") Type cachedType,
        @Cached("doResolve(cachedType, cachedSymbol)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(Type self, UnresolvedSymbol symbol)
        throws MethodDispatchLibrary.NoSuchMethodException {
      Function function = doResolve(self, symbol);
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
    static Function doResolve(Type type, Type target, UnresolvedConversion conversion) {
      return conversion.resolveFor(target, type);
    }

    static Context getContext() {
      return Context.get(null);
    }

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedConversion == conversion",
          "cachedTarget == target",
          "self == cachedType",
          "function != null"
        },
        limit = "CACHE_SIZE")
    static Function resolveCached(
        Type self,
        Type target,
        UnresolvedConversion conversion,
        @Cached("conversion") UnresolvedConversion cachedConversion,
        @Cached("self") Type cachedType,
        @Cached("target") Type cachedTarget,
        @Cached("doResolve(cachedType, cachedTarget, cachedConversion)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(Type self, Type target, UnresolvedConversion conversion)
        throws MethodDispatchLibrary.NoSuchConversionException {
      Function function = doResolve(self, target, conversion);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchConversionException();
      }
      return function;
    }
  }

  @ExportMessage
  String toDisplayString(boolean allowSideEffects) {
    return name;
  }
}
