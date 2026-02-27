package org.enso.interpreter.epb;

import com.oracle.truffle.api.TruffleOptions;
import com.oracle.truffle.api.nodes.RootNode;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Set;
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
      var onlyModules =
          Set.of(
              "org.enso.jvm.interop",
              "org.enso.jvm.channel",
              "org.enso.persistance",
              "org.graalvm.polyglot",
              "org.graalvm.truffle",
              "org.enso.engine.common",
              "java.net.http", // needed by Audit_Logs
              "java.rmi", // needed by Snowflake
              "org.slf4j",
              "org.enso.logging.utils",
              "org.enso.logging.config",
              "scala.library", // please remove in the future
              "typesafe.config");
      var loader =
          OtherJvmClassLoader.create(
              "org.enso.jvm.interop",
              EpbLanguage.class,
              polyglotBindings,
              isAot,
              context.getEnv().getContext(),
              onlyModules);
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
