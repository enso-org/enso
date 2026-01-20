package org.enso.interpreter.epb;

import com.oracle.truffle.api.TruffleOptions;
import com.oracle.truffle.api.nodes.RootNode;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.function.Function;
import org.enso.jvm.interop.api.OtherJvmClassLoader;

final class JavaPolyglotNode {
  static GenericForeignNode create(EpbContext context) {
    try {
      var isAot = TruffleOptions.AOT;
      Function<String, Object> polyglotBindings =
          (name) -> {
            var lang = context.getEnv().getInternalLanguages().get(name);
            return lang == null ? null : context.getEnv().getScopePublic(lang);
          };
      assert null != polyglotBindings;
      var loader =
          OtherJvmClassLoader.create(
              "org.enso.jvm.interop",
              EpbLanguage.class,
              polyglotBindings,
              isAot,
              context.getEnv().getContext());
      var target = RootNode.createConstantNode(loader).getCallTarget();
      return new GenericForeignNode(target);
    } catch (URISyntaxException | IOException ex) {
      throw new IllegalStateException(ex);
    }
  }

  static ForeignFunctionCallNode createHosted(EpbContext context) {
    var loader = new HostClassLoader();
    var target = RootNode.createConstantNode(loader).getCallTarget();
    return new GenericForeignNode(target);
  }
}
