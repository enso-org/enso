package org.enso.interpreter.node.expression.builtin.system;

import com.oracle.truffle.api.nodes.Node;
import org.apache.commons.lang3.SystemUtils;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(
    type = "System",
    name = "os",
    description = "Get the type of operating system.")
public final class OsNode extends Node {

  private final String LINUX = "linux";
  private final String MACOS = "macos";
  private final String WINDOWS = "windows";
  private final String UNKNOWN = "unknown";

  String execute(Object _this) {
    if (SystemUtils.IS_OS_LINUX) return LINUX;
    if (SystemUtils.IS_OS_MAC_OSX) return MACOS;
    if (SystemUtils.IS_OS_WINDOWS) return WINDOWS;
    return UNKNOWN;
  }
}
