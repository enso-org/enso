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
import java.security.Security;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.openssl.PKCS8Generator;
import org.bouncycastle.openssl.jcajce.JcaPEMWriter;
import org.bouncycastle.openssl.jcajce.JcaPKCS8Generator;
import org.bouncycastle.openssl.jcajce.JceOpenSSLPKCS8EncryptorBuilder;
import org.bouncycastle.operator.OperatorCreationException;
import org.bouncycastle.operator.OutputEncryptor;

public class TestKeyGenerator {

  static {
    if (Security.getProvider(BouncyCastleProvider.PROVIDER_NAME) == null) {
      Security.addProvider(new BouncyCastleProvider());
    }
  }

  public static void generateKeyPairForTest(
      String privateKeyPath, String publicKeyPath, String passphrase)
      throws NoSuchAlgorithmException, IOException, OperatorCreationException {

    File privateKeyFile = new File(privateKeyPath);
    File publicKeyFile = new File(publicKeyPath);

    KeyPairGenerator keyPairGenerator = KeyPairGenerator.getInstance("RSA");
    keyPairGenerator.initialize(2048);
    KeyPair keyPair = keyPairGenerator.generateKeyPair();

    if (passphrase.isEmpty()) {
      savePrivateKey(keyPair.getPrivate(), privateKeyFile);
    } else {
      savePrivateKeyEncrypted(keyPair.getPrivate(), privateKeyFile, passphrase);
    }

    savePublicKey(keyPair.getPublic(), publicKeyFile);
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

  private static void savePrivateKeyEncrypted(PrivateKey key, File destination, String passphrase)
      throws IOException, OperatorCreationException {
    var encryptorBuilder = new JceOpenSSLPKCS8EncryptorBuilder(PKCS8Generator.AES_256_CBC);
    encryptorBuilder.setPassword(passphrase.toCharArray().clone());
    OutputEncryptor encryptor = encryptorBuilder.build();
    JcaPKCS8Generator pkcs8Generator = new JcaPKCS8Generator(key, encryptor);

    try (FileOutputStream fileOutputStream = new FileOutputStream(destination);
        OutputStreamWriter outputStreamWriter = new OutputStreamWriter(fileOutputStream);
        JcaPEMWriter pemWriter = new JcaPEMWriter(outputStreamWriter)) {
      pemWriter.writeObject(pkcs8Generator.generate());
      pemWriter.flush();
    }
  }
}
