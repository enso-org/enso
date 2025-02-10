package org.enso.snowflake_helpers;

import java.io.FileOutputStream;
import java.io.IOException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;

public class TestKeyGenerator {
  public static void generateKeyPairForTest(String privateKeyPath, String publicKeyPath, String passphrase) throws NoSuchAlgorithmException {
    KeyPairGenerator keyPairGenerator = KeyPairGenerator.getInstance("RSA");
    keyPairGenerator.initialize(2048);
    KeyPair keyPair = keyPairGenerator.generateKeyPair();

    // Save the private key
    try (FileOutputStream fileOutputStream = new FileOutputStream(privateKeyPath)) {
      JcaPEMWriter pemWriter = new JcaPEMWriter(new OutputStreamWriter(fileOutputStream));
    } catch (IOException e) {
      throw new RuntimeException("Failed to save the private key: " + e.getMessage(), e);
    }

  }
}
