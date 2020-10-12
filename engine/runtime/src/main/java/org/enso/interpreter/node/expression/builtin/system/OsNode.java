package org.enso.interpreter.node.expression.builtin.system;

import com.oracle.truffle.api.nodes.Node;
import org.apache.commons.lang3.SystemUtils;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(
    type = "System",
    name = "os",
    description = "Get the type of operating system.")
public final class OsNode extends Node {

  private final Text LINUX = Text.create("linux");
  private final Text MACOS = Text.create("macos");
  private final Text WINDOWS = Text.create("windows");
  private final Text UNKNOWN = Text.create("unknown");

  Text execute(Object _this) {
    if (SystemUtils.IS_OS_LINUX) return LINUX;
    if (SystemUtils.IS_OS_MAC_OSX) return MACOS;
    if (SystemUtils.IS_OS_WINDOWS) return WINDOWS;
    return UNKNOWN;
  }
}
