package org.enso.interpreter.node.expression.constant;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.EnsoFile;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.pkg.Package;

import java.util.Optional;

@NodeInfo(description = "Returns a language-level representation of the given Enso project.")
public class EnsoProjectNode extends RootNode {
  private final Object result;

  public EnsoProjectNode(
      TruffleLanguage<?> language, Context context, Optional<Package<TruffleFile>> pkgOpt) {
    super(language);
    if (pkgOpt.isPresent()) {
      Package<TruffleFile> pkg = pkgOpt.get();
      EnsoFile rootPath = new EnsoFile(pkg.root().normalize());
      Object cfg = context.getEnvironment().asGuestValue(pkg.config());
      result =
          context
              .getBuiltins()
              .getProjectDescription()
              .getUniqueConstructor()
              .newInstance(rootPath, cfg);
    } else {
      result =
          DataflowError.withoutTrace(
              context.getBuiltins().error().makeModuleNotInPackageError(), this);
    }
  }

  @Override
  public Object execute(VirtualFrame frame) {
    return result;
  }
}
