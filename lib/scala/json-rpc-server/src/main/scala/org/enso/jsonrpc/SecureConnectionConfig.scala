package org.enso.jsonrpc

import com.typesafe.config.Config

import java.io.{ByteArrayInputStream, File, FileInputStream, InputStream}
import java.security.cert.CertificateFactory
import java.security.spec.PKCS8EncodedKeySpec
import java.security.{KeyFactory, KeyStore, SecureRandom}
import java.util.Base64
import javax.net.ssl.{KeyManagerFactory, SSLContext}
import scala.util.Try

abstract class SecureConnectionConfig {
  def toSSLContext(): Try[SSLContext]
}

case class SecureConnectionConfigForPKCS12(
  pkcsFileStream: InputStream,
  password: String,
  keystoreType: String,
  keyManagerAlgorithm: String,
  sslType: String
) extends SecureConnectionConfig {
  def toSSLContext(): Try[SSLContext] = Try {
    val keyStore      = KeyStore.getInstance(keystoreType)
    val passwordChars = password.toCharArray
    keyStore.load(pkcsFileStream, passwordChars)

    val kmf = KeyManagerFactory.getInstance(keyManagerAlgorithm)
    kmf.init(keyStore, passwordChars)
    val keyManagers = kmf.getKeyManagers
    val sslContext  = SSLContext.getInstance(sslType)
    sslContext.init(keyManagers, null, new SecureRandom())
    sslContext
  }
}
object SecureConnectionConfigForPKCS12 {
  def apply(
    pkcsFile: File,
    password: String,
    keystoreType: String,
    keyManagerAlgorithm: String,
    sslType: String
  ): SecureConnectionConfig = SecureConnectionConfigForPKCS12(
    new FileInputStream(pkcsFile),
    password,
    keystoreType,
    keyManagerAlgorithm,
    sslType
  )
}

case class SecureConnectionConfigForPublicPrivateCert(
  publicCertificate: String,
  publicCertificateAlgorithm: String,
  privateKey: String,
  keystoreType: String,
  keyManagerAlgorithm: String,
  sslType: String
) extends SecureConnectionConfig {

  private val beginPrivateHeader = "-----BEGIN PRIVATE KEY-----\n"

  override def toSSLContext(): Try[SSLContext] = Try {
    val factory = CertificateFactory.getInstance(publicCertificateAlgorithm)
    val certificateStream =
      new ByteArrayInputStream(publicCertificate.getBytes())
    val cert = factory.generateCertificate(certificateStream)

    val prefixIdx = privateKey.indexOf(beginPrivateHeader)
    val privateKeyWithDroppedAttributes =
      if (prefixIdx == -1) privateKey else privateKey.substring(prefixIdx)

    val privateKeyPEM = privateKeyWithDroppedAttributes
      .replace(beginPrivateHeader, "")
      .replace("-----END PRIVATE KEY-----", "")
      .replaceAll("\n", "");
    val privateKeyDER = Base64.getDecoder().decode(privateKeyPEM);

    val spec            = new PKCS8EncodedKeySpec(privateKeyDER);
    val keyFactory      = KeyFactory.getInstance("RSA");
    val storePrivateKey = keyFactory.generatePrivate(spec);
    val keyStore        = KeyStore.getInstance("PKCS12")
    keyStore.load(null)
    val password = "temp-keystore".toCharArray
    keyStore.setKeyEntry("enso", storePrivateKey, password, Array(cert))

    val kmf =
      KeyManagerFactory.getInstance(keyManagerAlgorithm)
    kmf.init(keyStore, password)
    val keyManagers = kmf.getKeyManagers
    val sslContext  = SSLContext.getInstance(sslType)
    sslContext.init(keyManagers, null, new SecureRandom())
    sslContext

  }
}

object SecureConnectionConfig {
  def fromApplicationConfig(config: Config): Option[SecureConnectionConfig] = {
    if (config.hasPath("pkcs12-file")) {
      for {
        pkcs12 <- getStringFieldOpt(
          config,
          "pkcs12-file"
        )
        pkcs12File = new File(pkcs12)
        password            <- getStringFieldOpt(config, "password")
        keystoreType        <- getStringFieldOpt(config, "keystore-type")
        keymanagerAlgorithm <- getStringFieldOpt(config, "keymanager-algorithm")
        sslType             <- getStringFieldOpt(config, "ssl-type")
      } yield SecureConnectionConfigForPKCS12(
        pkcs12File,
        password,
        keystoreType,
        keymanagerAlgorithm,
        sslType
      )
    } else if (config.hasPath("public-certificate")) {
      for {
        publicKeyCertificate <- getStringFieldOpt(
          config,
          "public-certificate"
        )
        publicKeyCertificateAlg <- getStringFieldOpt(
          config,
          "public-certificate-algorithm"
        )
        privateKey          <- getStringFieldOpt(config, "private-key")
        keystoreType        <- getStringFieldOpt(config, "keystore-type")
        keymanagerAlgorithm <- getStringFieldOpt(config, "keymanager-algorithm")
        sslType             <- getStringFieldOpt(config, "ssl-type")
      } yield SecureConnectionConfigForPublicPrivateCert(
        publicKeyCertificate,
        publicKeyCertificateAlg,
        privateKey,
        keystoreType,
        keymanagerAlgorithm,
        sslType
      )

    } else None
  }

  private def getStringFieldOpt(
    config: Config,
    fieldName: String
  ): Option[String] = {
    if (config.hasPath(fieldName)) {
      Some(config.getString(fieldName))
    } else {
      None
    }
  }

}
