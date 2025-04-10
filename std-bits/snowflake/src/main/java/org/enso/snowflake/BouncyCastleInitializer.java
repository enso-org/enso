package org.enso.snowflake;

import java.security.Security;
import org.bouncycastle.jce.provider.BouncyCastleProvider;

public class BouncyCastleInitializer {
  static {
    System.out.println("TEST KEY GENERATOR IS RUN AT BUILD TIME!");
    Security.addProvider(new BouncyCastleProvider());
  }
}
