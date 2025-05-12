package org.enso.nativeimage.workarounds;

import java.util.function.BooleanSupplier;

final class OnlyWithDesktop implements BooleanSupplier {
  @Override
  public boolean getAsBoolean() {
    try {
      return Class.forName("java.awt.GraphicsEnvironment") != null;
    } catch (ClassNotFoundException ex) {
      return false;
    }
  }
}
