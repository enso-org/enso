package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import java.lang.ref.WeakReference;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoFile;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomNewInstanceNode;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.pkg.Package;

@BuiltinMethod(
    type = "Project_Description",
    name = "enso_project_builtin",
    description =
        "Returns the project description of the project given as the argument, or the main project")
public abstract class EnsoProjectNode extends Node {

  public static EnsoProjectNode build() {
    return EnsoProjectNodeGen.create();
  }

  /** A weak reference to the context in which this node was last executed. */
  @CompilationFinal private WeakReference<EnsoContext> previousCtxRef = new WeakReference<>(null);

  /**
   * The project descriptor is cached, as it is safe to assume that it will not change during the
   * runtime.
   */
  private Object cachedProjectDescr;

  /**
   * @param module Either {@code Nothing}, or a module.
   */
  public abstract Object execute(Object module);

  /**
   * Returns the project description of the main project, i.e., the project specified as the root to
   * the engine.
   *
   * @param nothing Nothing, or interop null.
   */
  @Specialization(guards = "isNothing(nothing)")
  public Object getCurrentProjectDescr(Object nothing) {
    var ctx = EnsoContext.get(this);
    var previousCtx = previousCtxRef.get();
    if (previousCtx == null || cachedProjectDescr == null || previousCtx != ctx) {
      CompilerDirectives.transferToInterpreter();
      previousCtxRef = new WeakReference<>(ctx);
      var mainPkg = getMainProjectFromCtx(ctx);
      if (mainPkg != null) {
        cachedProjectDescr = createProjectDescriptionAtom(ctx, mainPkg);
      } else {
        cachedProjectDescr = notInModuleError(ctx);
      }
    }
    assert cachedProjectDescr != null;
    return cachedProjectDescr;
  }

  @Specialization(guards = "!isNothing(module)")
  @TruffleBoundary
  public Object getOtherProjectDescr(
      Object module, @CachedLibrary(limit = "5") TypesLibrary typesLib) {
    var ctx = EnsoContext.get(this);
    if (!typesLib.hasType(module)) {
      return unsupportedArgsError(module);
    }
    Type moduleType = typesLib.getType(module);
    // Currently, the module is represented as Type with no constructors.
    if (!moduleType.getConstructors().isEmpty()) {
      return unsupportedArgsError(module);
    }
    var pkg = moduleType.getDefinitionScope().getModule().getPackage();
    if (pkg != null) {
      return createProjectDescriptionAtom(ctx, pkg);
    } else {
      return notInModuleError(ctx);
    }
  }

  private static Package<TruffleFile> getMainProjectFromCtx(EnsoContext ctx) {
    var mainPkgOpt = ctx.getPackageRepository().getMainProjectPackage();
    if (mainPkgOpt.isDefined()) {
      return mainPkgOpt.get();
    }
    return null;
  }

  private static Atom createProjectDescriptionAtom(EnsoContext ctx, Package<TruffleFile> pkg) {
    var rootPath = new EnsoFile(pkg.root().normalize());
    var cfg = ctx.asGuestValue(pkg.getConfig());
    var cons = ctx.getBuiltins().getProjectDescription().getUniqueConstructor();

    return AtomNewInstanceNode.getUncached().newInstance(cons, rootPath, cfg);
  }

  private DataflowError unsupportedArgsError(Object moduleActual) {
    return DataflowError.withoutTrace(
        EnsoContext.get(this)
            .getBuiltins()
            .error()
            .makeUnsupportedArgumentsError(
                new Object[] {moduleActual}, "The `module` argument does not refer to a module"),
        this);
  }

  private DataflowError notInModuleError(EnsoContext ctx) {
    return DataflowError.withoutTrace(
        ctx.getBuiltins().error().makeModuleNotInPackageError(), this);
  }

  boolean isNothing(Object object) {
    return EnsoContext.get(this).getBuiltins().nothing() == object;
  }
}
