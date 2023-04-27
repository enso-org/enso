package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import java.lang.ref.WeakReference;
import java.util.Optional;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.EnsoRootNode;
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

  /**
   * It is OK to cache EnsoContext and project description atom here, since this node should be
   * cloned into every separate caller's AST.
   */
  @CompilationFinal
  private WeakReference<EnsoContext> previousCtxRef = new WeakReference<>(null);

  @CompilationFinal
  private WeakReference<Object> cachedProjectDescrRef = new WeakReference<>(null);

  /**
   * @param module Either {@code Nothing}, or a module.
   */
  public abstract Object execute(Object module);

  /**
   * Fetches the second stack frame from Truffle runtime (note that the first stack frame is the
   * call of {@code enso_project}) - the caller of {@code enso_project}, and finds in which package
   * the caller is located.
   *
   * @param nothing Nothing, or interop null.
   */
  @Specialization(guards = "isNothing(nothing)")
  public Object getCurrentProjectDescr(Object nothing) {
    var ctx = EnsoContext.get(this);
    var previousCtx = previousCtxRef.get();
    var cachedProjectDescr = cachedProjectDescrRef.get();
    if (previousCtx == null || cachedProjectDescr == null || previousCtx != ctx) {
      CompilerDirectives.transferToInterpreter();
      previousCtxRef = new WeakReference<>(ctx);
      // Find the caller of `enso_project`, i.e., of this node, and find in which package
      // it is located. The first frame is skipped, because it is always
      // `Enso_Project.enso_project`,
      // i.e., the first frame is always call of this specialization.
      Optional<Package<TruffleFile>> pkgOpt =
          Truffle.getRuntime()
              .iterateFrames(
                  frame -> {
                    var callNode = frame.getCallNode();
                    assert callNode != null
                        : "Should skip the first frame, therefore, callNode should not be null";
                    var callRootNode = callNode.getRootNode();
                    assert callRootNode != null
                        : "Should be called only from Enso code, and thus, should always have a root node";
                    if (callRootNode instanceof EnsoRootNode ensoRootNode) {
                      var pkg = ensoRootNode.getModuleScope().getModule().getPackage();
                      // Don't return null, as that would signal to Truffle that we want to
                      // continue the iteration.
                      if (pkg != null) {
                        return Optional.of(pkg);
                      } else {
                        return Optional.empty();
                      }
                    } else {
                      throw new IllegalStateException(
                          "Should not reach here: callRootNode = "
                              + callRootNode
                              + ". Probably not called from Enso?");
                    }
                  },
                  // The first frame is always Enso_Project.enso_project
                  1);
      if (pkgOpt.isPresent()) {
        cachedProjectDescrRef = new WeakReference<>(
            createProjectDescriptionAtom(ctx, pkgOpt.get()));
      } else {
        cachedProjectDescrRef = new WeakReference<>(notInModuleError(ctx));
      }
      cachedProjectDescr = cachedProjectDescrRef.get();
    }
    assert cachedProjectDescr != null;
    return cachedProjectDescr;
  }

  @Specialization(guards = "!isNothing(module)")
  @TruffleBoundary
  public Object getOtherProjectDescr(
      Object module,
      @CachedLibrary(limit = "5") TypesLibrary typesLib) {
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
                new Object[]{moduleActual}, "The `module` argument does not refer to a module"),
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
