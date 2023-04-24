package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.data.EnsoFile;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.pkg.Package;

@BuiltinMethod(
    type = "Project_Description",
    name = "enso_project_builtin",
    description = "Returns the project description of the project given as the argument")
public abstract class EnsoProjectNode extends Node {
  public static EnsoProjectNode build() {
    return EnsoProjectNodeGen.create();
  }

  public abstract Object execute(Object project);

  @Specialization(guards = "isNothing(interop, nothing)")
  @TruffleBoundary
  public Object getCurrentProjectDescr(
      Object nothing, @CachedLibrary(limit = "5") InteropLibrary interop) {
    var ctx = EnsoContext.get(this);
    var mainProjectPkg = ctx.getPackageRepository().getMainProjectPackage();
    if (mainProjectPkg.isDefined()) {
      return createProjectDescriptionAtom(ctx, mainProjectPkg.get());
    } else {
      return DataflowError.withoutTrace(
          ctx.getBuiltins().error().makeModuleNotInPackageError(), this);
    }
  }

  @Specialization(guards = "!isNothing(interop, module)")
  @TruffleBoundary
  public Object getOtherProjectDescr(
      Object module,
      @CachedLibrary(limit = "5") InteropLibrary interop,
      @CachedLibrary(limit = "5") TypesLibrary typesLib) {
    var ctx = EnsoContext.get(this);
    if (!typesLib.hasType(module)) {
      return unsupportedArgsError(module);
    }
    Type moduleType = typesLib.getType(module);
    if (!moduleType.getConstructors().isEmpty()) {
      return unsupportedArgsError(module);
    }
    var pkg = moduleType.getDefinitionScope().getModule().getPackage();
    return createProjectDescriptionAtom(ctx, pkg);
  }

  private static Atom createProjectDescriptionAtom(EnsoContext ctx, Package<TruffleFile> pkg) {
    EnsoFile rootPath = new EnsoFile(pkg.root().normalize());
    Object cfg = ctx.getEnvironment().asGuestValue(pkg.config());
    return ctx.getBuiltins()
        .getProjectDescription()
        .getUniqueConstructor()
        .newInstance(rootPath, cfg);
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

  boolean isNothing(InteropLibrary interop, Object object) {
    return interop.isNull(object);
  }
}
