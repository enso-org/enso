package org.enso.snowflake_helpers;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import net.snowflake.client.jdbc.internal.org.bouncycastle.openssl.jcajce.JcaPEMWriter;

public class TestKeyGenerator {
  public static void generateKeyPairForTest(
      String privateKeyPath, String publicKeyPath, String passphrase)
      throws NoSuchAlgorithmException, IOException {

    File privateKeyFile = new File(privateKeyPath);
    File publicKeyFile = new File(publicKeyPath);

    KeyPairGenerator keyPairGenerator = KeyPairGenerator.getInstance("RSA");
    keyPairGenerator.initialize(2048);
    KeyPair keyPair = keyPairGenerator.generateKeyPair();

    savePublicKey(keyPair.getPublic(), publicKeyFile);
    if (passphrase == null) {
      savePrivateKey(keyPair.getPrivate(), privateKeyFile);
    } else {
      savePrivateKeyEncrypted(keyPair.getPrivate(), privateKeyFile, passphrase);
    }
  }

  private static void savePublicKey(PublicKey key, File destination) throws IOException {
    try (FileOutputStream fileOutputStream = new FileOutputStream(destination);
        OutputStreamWriter outputStreamWriter = new OutputStreamWriter(fileOutputStream);
        JcaPEMWriter pemWriter = new JcaPEMWriter(outputStreamWriter)) {
      pemWriter.writeObject(key);
      pemWriter.flush();
    }
  }

  private static void savePrivateKey(PrivateKey key, File destination) throws IOException {
    try (FileOutputStream fileOutputStream = new FileOutputStream(destination);
        OutputStreamWriter outputStreamWriter = new OutputStreamWriter(fileOutputStream);
        JcaPEMWriter pemWriter = new JcaPEMWriter(outputStreamWriter)) {
      pemWriter.writeObject(key);
      pemWriter.flush();
    }
  }

  private static void savePrivateKeyEncrypted(PrivateKey key, File destination, String passphrase) {
    throw new UnsupportedOperationException("TODO");
  }
}
