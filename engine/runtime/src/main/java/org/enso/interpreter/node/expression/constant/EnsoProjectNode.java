package org.enso.interpreter.node.expression.constant;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoFile;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.pkg.Package;

import java.util.Optional;

@NodeInfo(description = "Returns a language-level representation of the given Enso project.")
public class EnsoProjectNode extends RootNode {
  private final Optional<Package<TruffleFile>> pkgOpt;

  public EnsoProjectNode(
      TruffleLanguage<?> language, EnsoContext context, Optional<Package<TruffleFile>> pkgOpt) {
    super(language);
    this.pkgOpt = pkgOpt;
  }

  @Override
  public Object execute(VirtualFrame frame) {
    return createProjectDescription();
  }

  @CompilerDirectives.TruffleBoundary
  private Object createProjectDescription() {
    var context = EnsoContext.get(this);
    if (pkgOpt.isPresent()) {
      Package<TruffleFile> pkg = pkgOpt.get();
      EnsoFile rootPath = new EnsoFile(pkg.root().normalize());
      Object cfg = context.getEnvironment().asGuestValue(pkg.config());
      var result =
          context
              .getBuiltins()
              .getProjectDescription()
              .getUniqueConstructor()
              .newInstance(rootPath, cfg);
      return result;
    } else {
      var result =
          DataflowError.withoutTrace(
              context.getBuiltins().error().makeModuleNotInPackageError(), this);
      return result;
    }
  }
}
