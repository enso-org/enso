package org.enso.snowflake;

import java.security.Security;
import org.bouncycastle.jce.provider.BouncyCastleProvider;

public final class BouncyCastleInitializer {
  private BouncyCastleInitializer() {}

  static {
    Security.addProvider(new BouncyCastleProvider());
  }
}
