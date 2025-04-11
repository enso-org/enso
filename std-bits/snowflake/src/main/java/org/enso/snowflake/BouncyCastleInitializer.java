package org.enso.snowflake;

import java.security.Security;
import org.bouncycastle.jce.provider.BouncyCastleProvider;

public class BouncyCastleInitializer {
  static {
    Security.addProvider(new BouncyCastleProvider());
  }
}
