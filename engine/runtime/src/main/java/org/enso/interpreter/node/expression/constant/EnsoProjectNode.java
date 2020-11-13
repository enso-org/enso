package org.enso.interpreter.node.expression.constant;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoFile;
import org.enso.interpreter.runtime.error.RuntimeError;
import org.enso.interpreter.runtime.state.Stateful;
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
      Object rootPath = context.getEnvironment().asGuestValue(new EnsoFile(pkg.root().normalize()));
      result = context.getBuiltins().getEnsoProject().newInstance(rootPath);
    } else {
      result =
          new RuntimeError(context.getBuiltins().error().moduleNotInPackageError().newInstance());
    }
  }

  @Override
  public Stateful execute(VirtualFrame frame) {
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    return new Stateful(state, result);
  }
}
