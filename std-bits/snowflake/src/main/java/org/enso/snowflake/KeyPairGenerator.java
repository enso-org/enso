package org.enso.snowflake;

import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.util.Base64;

public class KeyPairGenerator {

  private static final Base64.Encoder Base64Encoder = Base64.getMimeEncoder();

  private KeyPairGenerator() {}

  public record GeneratedKeyPair(PublicKey publicKey, PrivateKey privateKey) {
    public String encodedPublicKey() {
      return Base64Encoder.encodeToString(publicKey.getEncoded());
    }

    public String encodedPrivateKey() {
      return Base64Encoder.encodeToString(privateKey.getEncoded());
    }
  }

  public static GeneratedKeyPair generate(String algorithm, int keySize)
      throws NoSuchAlgorithmException {
    var keyPairGen = java.security.KeyPairGenerator.getInstance(algorithm);
    keyPairGen.initialize(keySize);
    var keyPair = keyPairGen.generateKeyPair();
    return new GeneratedKeyPair(keyPair.getPublic(), keyPair.getPrivate());
  }
}
