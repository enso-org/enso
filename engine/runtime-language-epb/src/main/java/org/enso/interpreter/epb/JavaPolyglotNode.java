package org.enso.interpreter.epb;

import com.oracle.truffle.api.TruffleOptions;
import com.oracle.truffle.api.nodes.RootNode;
import java.io.IOException;
import java.net.URISyntaxException;
import org.enso.jvm.interop.api.OtherJvmClassLoader;

final class JavaPolyglotNode {
  static GenericForeignNode create() {
    try {
      var isAot = TruffleOptions.AOT;
      var loader = OtherJvmClassLoader.create(isAot);
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
