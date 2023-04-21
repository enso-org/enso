package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoFile;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.pkg.Package;

@BuiltinMethod(
    type = "Project_Description",
    name = "enso_project",
    description = "Returns the description of the current Enso project",
    autoRegister = false
)
public final class EnsoProjectNode extends Node {
  public static EnsoProjectNode build() {
    return MyEnsoProjectNodeGen.create();
  }

  @TruffleBoundary
  public Object execute() {
    var ctx = EnsoContext.get(this);
    var mainProjectPkg = ctx.getPackageRepository().getMainProjectPackage();
    if (mainProjectPkg.isDefined()) {
      Package<TruffleFile> pkg = mainProjectPkg.get();
      EnsoFile rootPath = new EnsoFile(pkg.root().normalize());
      Object cfg = ctx.getEnvironment().asGuestValue(pkg.config());
      return ctx
              .getBuiltins()
              .getProjectDescription()
              .getUniqueConstructor()
              .newInstance(rootPath, cfg);
    } else {
      return DataflowError.withoutTrace(
              ctx.getBuiltins().error().makeModuleNotInPackageError(), this);
    }
  }
}
