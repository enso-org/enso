package org.enso.snowflake;

import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.util.Base64;

public class KeyPairHelper {

  private static final Base64.Encoder Base64Encoder = Base64.getMimeEncoder();

  private KeyPairHelper(PublicKey publicKey, PrivateKey privateKey) {
    this.publicKey = publicKey;
    this.privateKey = privateKey;
  }

  private final PublicKey publicKey;
  private final PrivateKey privateKey;

  public String encodedPublicKey() {
    return Base64Encoder.encodeToString(publicKey.getEncoded());
  }

  public String encodedPrivateKey() {
    return Base64Encoder.encodeToString(privateKey.getEncoded());
  }

  public static KeyPairHelper generate(String algorithm, int keySize)
      throws NoSuchAlgorithmException {
    var keyPairGen = KeyPairGenerator.getInstance(algorithm);
    keyPairGen.initialize(keySize);
    var keyPair = keyPairGen.generateKeyPair();
    return new KeyPairHelper(keyPair.getPublic(), keyPair.getPrivate());
  }
}
