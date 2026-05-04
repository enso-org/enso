package org.enso.os.environment;

import org.enso.common.Platform;

public sealed interface Arguments permits WindowsArguments, LinuxArguments {
  static Arguments getCurrent() {
    return switch (Platform.getOperatingSystem()) {
      case LINUX, MACOS -> LinuxArguments.INSTANCE;
      case WINDOWS -> WindowsArguments.INSTANCE;
    };
  }

  String[] alterArgs(String[] originalArgs);
}
